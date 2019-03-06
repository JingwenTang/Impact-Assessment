rm(list = ls())
library(readr)
library(openxlsx)
library(plyr)
library(dplyr)
library(Synth)
library(ggplot2)
library(graphics)
library(qpcR)
library(MASS)
library(ROCR)
library(plyr)
library(stargazer)
library(miceadds)
library(multiwayvcov)
library(radiant.data)
library(stringi)
library(lubridate)
Timemode = c("13-17 month","13-17 month all","13-17 month common types","13-17 month all common types",
             "13-17 week","13-17 week all","13-17 week common types","13-17 week all common types",
             "13-17 month without vdsa features","13-17 month all without vdsa features") #the time unit and lags chosen for the model in synthetic control
br="month"
for (tm in c(1,2)){
  timemode = Timemode[tm]
  wb = createWorkbook() 
  wbe = createWorkbook()
  #f[[2]] = c("annual","total_ar","lit_t","male_saw")
  fmode = "CV"
  f = c("annual","total_ar","lit_t","male_saw","annmai","lit_ru","ptmrkt","lroad","agrl_t")
 
  Product = c("Groundnut","Jowar(Sorghum)","Maize","Sunflower","Arhar (Tur-Red Gram)","Cotton","Green Gram (Moong)","Arecanut(Betelnut-Supari)","Bengal Gram(Gram)","Black Gram (Urd Beans)","Dry Chillies","Copra","Kulthi(Horse Gram)","Paddy(Dhan)")
  Wpiproduct = c("Groundnut Seed","Jowar","Maize","Sunflower","Arhar","Cotton Seed","Moong","Betelnut/Arecanut","Gram","ALL COMMODITIES","Chillies(Dry)","Copra (Coconut)","ALL COMMODITIES","ALL COMMODITIES")
 
  Features = list()
  Features[[1]] = c("gnut_tq",f)
  Features[[2]] = c("sorg_tq",f)
  Features[[3]] = c("maiz_tq",f)
  Features[[4]] = c("sunf_tq",f)
  for (j in 1:length(Product)){
    Features[[j]] = f
  }
  
  intervention= as.data.frame(read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/infrastructure_characteristics/Market Details.xlsx",sheet = "Market Live Dates"))[3:160,]
  intervention$X3 = tolower(as.character(intervention$X3))
  
  temp = read.csv("/Users/tangjingwen/Dropbox/jingwen_tasks/CombinedArrivals/intervented change to organized.csv",header = F)
  colnames(temp) = c("int","org")
  temp$int = as.character(temp$int)
  temp$org = as.character(temp$org)
  inind = intervention$X3%in%temp$int
  orind = match(intervention$X3[inind],temp$int)
  intervention$X3[inind] = temp$org[orind]
  intervented = tolower(as.character(intervention$X3))
  intervented = c(intervented,"virtual")
  interdate = as.Date(as.numeric(as.character(intervention$X4))-25569,origin = "1970-01-01")
  interdate = as.Date(cut(interdate,breaks = "month"))
  
  
  for (p in 1:length(Product)){

      re = c()
      ree=c()
      product = Product[p]
      dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/DiD/",fmode,"/",product,sep = ""))
      
      features = Features[[p]]
      
      predictors = features
      
      
      
      
      #market modal price monthly
      marketdat = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",timemode,"/",product," eleventh data.xlsx",sep = ""),sheet = 1)
      colnames(marketdat)[1] = "date"
      marketdat = marketdat[marketdat$market!="virtual",]
      temp = intervented%in%unique(marketdat$market)
      intervented1 = intervented[temp]
      interdate1 = interdate[temp]
      #lead and lags in month
      leaddate1 = interdate1 %m+% months(-1)
      leaddate2 = interdate1 %m+% months(-2)
      leaddate3 = interdate1 %m+% months(-3)
      leaddate4 = interdate1 %m+% months(-4)
      lagdate1 = interdate1 %m+% months(1)
      lagdate2 = interdate1 %m+% months(2)
      lagdate3 = interdate1 %m+% months(3)
      lagdate4 = interdate1 %m+% months(4)
      marketdat$inter = 0
      marketdat$lead0 = 0
      marketdat$lead1 = 0
      marketdat$lead2 = 0
      marketdat$lead3 = 0
      marketdat$lead4 = 0
      marketdat$lead = 0
      marketdat$lag1 = 0
      marketdat$lag2 = 0
      marketdat$lag3 = 0
      marketdat$lag4 = 0
      marketdat$lag = 0
      marketdat$date = as.Date(marketdat$date-25569,origin = "1970-01-01")
      for (i in 1:length(intervented1)){
        temp = marketdat[marketdat$market==intervented1[i],]
        marketdat[marketdat$market==intervented1[i],][temp$date>=(interdate1[i]+0),"inter"] = 1
        marketdat[marketdat$market==intervented1[i],][temp$date==leaddate1[i],"lead1"] = 1
        marketdat[marketdat$market==intervented1[i],][temp$date==leaddate2[i],"lead2"] = 1
        marketdat[marketdat$market==intervented1[i],][temp$date==leaddate3[i],"lead3"] = 1
        marketdat[marketdat$market==intervented1[i],][temp$date==leaddate4[i],"lead4"] = 1
        marketdat[marketdat$market==intervented1[i],][temp$date<leaddate4[i],"lead"] = 1
        marketdat[marketdat$market==intervented1[i],][temp$date==lagdate1[i],"lag1"] = 1
        marketdat[marketdat$market==intervented1[i],][temp$date==lagdate2[i],"lag2"] = 1
        marketdat[marketdat$market==intervented1[i],][temp$date==lagdate3[i],"lag3"] = 1
        marketdat[marketdat$market==intervented1[i],][temp$date==lagdate4[i],"lag4"] = 1
        marketdat[marketdat$market==intervented1[i],][temp$date>lagdate4[i],"lag"] = 1
        marketdat[marketdat$market==intervented1[i],][temp$date==(interdate1[i]+0),"lead0"] = 1
      }
      market = unique(marketdat$market)
      newd = c()
      for (i in 1:length(unique(marketdat$market))){
        temp = marketdat[marketdat$market==market[i],]
        temp = temp[order(temp$amount,decreasing = T),]
        thres = sum(temp$amount,na.rm = T)*0.9
        for (l in 1:nrow(temp)){
          if (sum(temp$amount[1:l],na.rm = T)>thres) break
        }
        temp[1:l,"active"] = TRUE
        if ((l+1)<=nrow(temp)) temp[(l+1):nrow(temp),"active"] = FALSE
        newd = rbind(newd,temp)
      }
      marketdat = newd
      #calculate the average before treatment
      temp = marketdat[(marketdat$market%in%intervented)&(marketdat$inter==0),]
      meanbefore = mean(temp$modal,na.rm = T)
      
      marketdat$inter = as.factor(marketdat$inter)
      
      
      ####for active quantile divide using all markets data:
      #market modal price monthly
      marketdat1 = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",timemode,"/",product," eleventh data.xlsx",sep = ""),sheet = 1)
      colnames(marketdat1)[1] = "date"
      marketdat1 = marketdat1[marketdat1$market!="virtual",]
      temp = intervented%in%unique(marketdat1$market)
      intervented1 = intervented[temp]
      interdate1 = interdate[temp]
      #lead and lags in month
      leaddate1 = interdate1 %m+% months(-1)
      leaddate2 = interdate1 %m+% months(-2)
      leaddate3 = interdate1 %m+% months(-3)
      leaddate4 = interdate1 %m+% months(-4)
      lagdate1 = interdate1 %m+% months(1)
      lagdate2 = interdate1 %m+% months(2)
      lagdate3 = interdate1 %m+% months(3)
      lagdate4 = interdate1 %m+% months(4)
      marketdat1$inter = 0
      marketdat1$lead0 = 0
      marketdat1$lead1 = 0
      marketdat1$lead2 = 0
      marketdat1$lead3 = 0
      marketdat1$lead4 = 0
      marketdat1$lead = 0
      marketdat1$lag1 = 0
      marketdat1$lag2 = 0
      marketdat1$lag3 = 0
      marketdat1$lag4 = 0
      marketdat1$lag = 0
      marketdat1$date = as.Date(marketdat1$date-25569,origin = "1970-01-01")
      for (i in 1:length(intervented1)){
        temp = marketdat1[marketdat1$market==intervented1[i],]
        marketdat1[marketdat1$market==intervented1[i],][temp$date>=(interdate1[i]+0),"inter"] = 1
        marketdat1[marketdat1$market==intervented1[i],][temp$date==leaddate1[i],"lead1"] = 1
        marketdat1[marketdat1$market==intervented1[i],][temp$date==leaddate2[i],"lead2"] = 1
        marketdat1[marketdat1$market==intervented1[i],][temp$date==leaddate3[i],"lead3"] = 1
        marketdat1[marketdat1$market==intervented1[i],][temp$date==leaddate4[i],"lead4"] = 1
        marketdat1[marketdat1$market==intervented1[i],][temp$date<leaddate4[i],"lead"] = 1
        marketdat1[marketdat1$market==intervented1[i],][temp$date==lagdate1[i],"lag1"] = 1
        marketdat1[marketdat1$market==intervented1[i],][temp$date==lagdate2[i],"lag2"] = 1
        marketdat1[marketdat1$market==intervented1[i],][temp$date==lagdate3[i],"lag3"] = 1
        marketdat1[marketdat1$market==intervented1[i],][temp$date==lagdate4[i],"lag4"] = 1
        marketdat1[marketdat1$market==intervented1[i],][temp$date>lagdate4[i],"lag"] = 1
        marketdat1[marketdat1$market==intervented1[i],][temp$date==(interdate1[i]+0),"lead0"] = 1
      }
      market = unique(marketdat1$market)
      newd = c()
      for (i in 1:length(unique(marketdat1$market))){
        temp = marketdat1[marketdat1$market==market[i],]
        temp = temp[order(temp$amount,decreasing = T),]
        thres = sum(temp$amount,na.rm = T)*0.9
        for (l in 1:nrow(temp)){
          if (sum(temp$amount[1:l],na.rm = T)>thres) break
        }
        temp[1:l,"active"] = TRUE
        if ((l+1)<=nrow(temp)) temp[(l+1):nrow(temp),"active"] = FALSE
        newd = rbind(newd,temp)
      }
      marketdat1 = newd
      #calculate the average before treatment
      temp = marketdat1[(marketdat1$market%in%intervented)&(marketdat1$inter==0),]
      meanbefore = mean(temp$modal,na.rm = T)
      
      marketdat1$inter = as.factor(marketdat1$inter)
      aa = aggregate(marketdat1$amount,by = list(marketdat1$market),sum,na.rm = T)
      colnames(aa) = c("market","amount")
      aa = aa[order(aa$amount,decreasing = T),]
      thresh1 = sum(aa$amount)*0.25
      thresh2 = sum(aa$amount)*0.5
      thresh3 = sum(aa$amount)*0.75
      for (l in 1:nrow(aa)){
        aaa = sum(aa$amount[1:l])
        if (aaa<=thresh1) l1 = l
        if (aaa<=thresh2) l2 = l
        if (aaa<=thresh3) l3 = l
      }
      large = aa$market[1:l1]
      medium_large = aa$market[l1:l2]
      medium_small = aa$market[l2:l3]
      small = aa$market[l3:nrow(aa)]
      marketdat1$size = "small"
      
      medium_large = medium_large[!medium_large%in%large]
      medium_small = medium_small[!medium_small%in%medium_large]
      small = small[!small%in%medium_small]
      
      marketdat1[marketdat1$market%in%small,"size"] = "small"
      marketdat1[marketdat1$market%in%medium_small,"size"] = "medium_small"
      marketdat1[marketdat1$market%in%medium_large,"size"] = "medium_large"
      marketdat1[marketdat1$market%in%large,"size"] = "large"
      
      
      ########state level data
      
      
      dat = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",timemode,"/",product," eleventh data.xlsx",sep = ""),sheet = 1)
      colnames(dat)[1] = "date"
      dat = dat[dat$market!="virtual",]
      rm = c()
      state = unique(dat$state)
      for (i in 1:length(state)){
        temp = dat[dat$state==state[i],]
        if (length(unique(temp$market))==1){
          rm = c(rm,state[i])
        }
      }
      dat = dat[!dat$state%in%rm,]
      
      
      temp = intervented%in%unique(dat$market)
      intervented2 = intervented[temp]
      interdate2 = interdate[temp]
      dat$date = as.Date(dat$date-25569,origin = "1970-01-01")
      
      notr = data.frame(date = as.Date(as.Date("2011-01-01"):as.Date("2017-12-31"),origin = "1970-01-01"))
      count = 0
      volume_fraction = 0
      origintable = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/",product,"/",timemode,"/original table.xlsx",sep = ""),sheet = 1)
      totalvolume = sum(origintable[origintable$market%in%intervented2,"amount"],na.rm = T)
      for (l in 1:nrow(notr)){
        temp = sum(notr$date[l]==interdate2)
        count = count + temp
        notr[l,"no.inter"] = count
        tempmar = intervented2[which(notr$date[l]==interdate2)]
        volume_fraction = volume_fraction + sum(origintable[origintable$market%in%tempmar,"amount"],na.rm = T)/totalvolume
        notr[l,"volume_fraction"] = volume_fraction
      }
      Max = aggregate(dat$modal,by = list(dat$date,dat$state),max)
      colnames(Max) = c("date","state","max")
      Min = aggregate(dat$modal,by = list(dat$date,dat$state),min)
      colnames(Min) = c("date","state","min")
      m = aggregate(dat$modal,by = list(dat$date,dat$state),mean)
      colnames(m) = c("date","state","mean")
      sd = aggregate(dat$modal,by = list(dat$date,dat$state),sd)
      colnames(sd) = c("date","state","sd")
      dat = dat[!is.na(dat$amount),]
      wm = ddply(dat,.(date,state),summarise, wmean = weighted.mean(modal,amount))
      wsd = ddply(dat,.(date,state),summarise, wsd = weighted.sd(modal,amount))
      Dat = join(Max,Min,by = c("date","state"),type = "left")
      Dat = join(Dat,m,by = c("date","state"),type = "left")
      Dat = join(Dat,sd,by = c("date","state"),type = "left")
      Dat = join(Dat,wm,by = c("date","state"),type = "left")
      Dat = join(Dat,wsd,by = c("date","state"),type = "left")
      Dat = na.omit(Dat)
      Dat$R = (Dat$max - Dat$min)/Dat$mean
      Dat$CV = Dat$sd/Dat$mean
      Dat$VWCV = Dat$wsd/Dat$wmean
      newdat = as.data.frame(aggregate(dat$amount, list(dat$date,dat$state), sum,na.rm = T))
      colnames(newdat) = c("date","state","amount")
      Dat = join(Dat,newdat,by = c("date","state"),type = "left")
      
      
      
      for (l in c(7:11,15:17,20:33)){
        name = colnames(dat)[l]
        colnames(dat)[l] = "target"
        newdat = as.data.frame(ddply(dat,.(date,state),summarise, wmean = weighted.mean(target,amount,na.rm = T)))
        colnames(newdat) = c("date","state",name)
        Dat = join(Dat,newdat,by = c("date","state"),type = "left")
        colnames(dat)[l] = name
      }
      
      dat = Dat
      dat_1 = dat[dat$state=="karnataka",]
      dat_2 = dat[dat$state!="karnataka",]
      dat_1 = join(dat_1,notr,by = "date")
      dat_2$no.inter = 0
      dat_2$volume_fraction = 0
      dat = rbind(dat_1,dat_2)
      dat = na.omit(dat[,-c(13:17)])
    
      clx <-function(fm, dfcw, cluster){
        library(sandwich)
        library(lmtest)
        M <- length(unique(cluster))
        N <- length(cluster)
        dfc <- (M/(M-1))*((N-1)/(N-fm$rank))
        u <- apply(estfun(fm),2,function(x) tapply(x, cluster, sum))
        vcovCL <- dfc*sandwich(fm, meat=crossprod(u)/N)*dfcw
        coeftest(fm, vcovCL) }
      
      
      mclx <-function(fm, dfcw, cluster1, cluster2){
        library(sandwich)
        library(lmtest)
        cluster12 = paste(cluster1,cluster2, sep="")
        M1 <- length(unique(cluster1))
        M2 <- length(unique(cluster2))
        M12 <- length(unique(cluster12))
        N <- length(cluster1)
        K <- fm$rank
        dfc1 <- (M1/(M1-1))*((N-1)/(N-K))
        dfc2 <- (M2/(M2-1))*((N-1)/(N-K))
        dfc12 <- (M12/(M12-1))*((N-1)/(N-K))
        u1 <- apply(estfun(fm), 2, function(x) tapply(x, cluster1, sum))
        u2 <- apply(estfun(fm), 2,function(x) tapply(x, cluster2, sum))
        u12 <- apply(estfun(fm), 2,function(x) tapply(x, cluster12, sum))
        vc1 <- dfc1*sandwich(fm, meat=crossprod(u1)/N )
        vc2 <- dfc2*sandwich(fm, meat=crossprod(u2)/N )
        vc12 <- dfc12*sandwich(fm, meat=crossprod(u12)/N)
        vcovMCL <- (vc1 + vc2 - vc12)*dfcw
        coeftest(fm, vcovMCL)}
      
 
      crse1 = t(rep(0,4))
      crse2 = t(rep(0,4))
      crse3 = t(rep(0,4))
      crse4 = t(rep(0,4))
      crse5 = t(rep(0,4))
      crse6 = t(rep(0,4))
      crse7 = t(rep(0,4))
      crse8 = t(rep(0,4))
      crse9 = t(rep(0,4))
      crse10 = t(rep(0,4))
      crse11 = t(rep(0,4))
      crse12 = t(rep(0,4))
      crse13 = t(rep(0,4))
      crse14 = t(rep(0,4))
      crse15 = t(rep(0,4))
      crse16 = t(rep(0,4))
      crse17 = t(rep(0,4))
      crse18 = t(rep(0,4))
      crse19 = matrix(0,10,4)
      crse20 = t(rep(0,4))
      crse21 = t(rep(0,4))
      crse22 = t(rep(0,4))
      crse23 = t(rep(0,4))
      crse24 = t(rep(0,4))
      crse25 = matrix(0,4,4)
      
      ###########################################
      #DiD for modal prices(base)
      ###########################################
      print(1)
      try({
        d = marketdat
        d$date = as.factor(d$date)
        d = d[!is.na(d$Per.Capita.GSDP),]
        d = d[!is.na(d$production),]
        d = d[!is.na(d$yield),]
        d = d[!is.na(d$amount),]
        d = d[!is.na(d$inter),]
        for (l in 20:32){
          d = d[!is.na(d[,l]),]
        }
        fit1 = lm(modal~ market+date+
                    Per.Capita.GSDP+production+yield+
                    amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                  +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                  +rainfall_lead10+rainfall_lead11+rainfall_lead12+inter,
                  data=d)
        
        crse1 = clx(fit1,1, d$market)
        crse1
      })
      
      
      
      ###########################################
      #DiD for CV
      ###########################################
      
      print(2)
      try({
        fit2 = lm(CV~ state+ date  +
                    Per.Capita.GSDP+production+yield+
                    amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                  +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                  +rainfall_lead10+rainfall_lead11+rainfall_lead12+ no.inter +0,
                  data=dat)
        
        crse2 = clx(fit2,1,dat$state)
        crse2
      })
      
      
      ###########################################
      #DiD for VWCV
      ###########################################
      
      print(3)
      try({
        fit3 = lm(VWCV~ state+ date +production+yield+
                    Per.Capita.GSDP+
                    amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                  +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                  +rainfall_lead10+rainfall_lead11+rainfall_lead12+ no.inter+0,
                  data=dat)
        
        crse3 = clx(fit3,1,dat$state)
        crse3
      })
      
      
      
      ###########################################
      #DiD for Range
      ###########################################
      print(4)
      try({
        fit4 = lm(R~ state+ date +
                    Per.Capita.GSDP+production+yield+
                    amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                  +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                  +rainfall_lead10+rainfall_lead11+rainfall_lead12+ no.inter+0 ,
                  data=dat)
        
        crse4 = clx(fit4,1,dat$state)
        crse4
      })
      
      
      ###########################################
      #DiD for modal prices parallel trend check
      ###########################################
      #because treated markets do not have enough data for period 2013-10 to 2013-12 so we remove this period
      #choose data before intervention
      print(5)
      try({
        d = marketdat[marketdat$date<min(interdate),]
        d$treat = d$market%in%intervented
        d = d[order(d$date),]
        d$time = as.numeric(as.factor((d$date)))
        d$date = as.factor(d$date)
        d = d[!is.na(d$Per.Capita.GSDP),]
        d = d[!is.na(d$production),]
        d = d[!is.na(d$yield),]
        d = d[!is.na(d$amount),]
        d = d[!is.na(d$inter),]
        for (l in 20:32){
          d = d[!is.na(d[,l]),]
        }
        fit5 = lm(modal~ Per.Capita.GSDP+production+yield+
                    amount+market+time  + date+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                  +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                  +rainfall_lead10+rainfall_lead11+rainfall_lead12 + treat:time+0 ,
                  data=d)
        crse5 = clx(fit5,1, d$market)
        crse5
      })
      
      
      
      ###########################################
      #DiD for modal prices remove interim
      ###########################################
      print(6)
      try({
        d = marketdat[marketdat$date<min(interdate1)|marketdat$date>=max(interdate1),]
        d$treat = d$market%in%intervented
        d = d[order(d$date),]
        d$time = as.numeric(as.factor((d$date)))
        d$date = as.factor(d$date)
        d = d[!is.na(d$Per.Capita.GSDP),]
        d = d[!is.na(d$production),]
        d = d[!is.na(d$yield),]
        d = d[!is.na(d$amount),]
        d = d[!is.na(d$inter),]
        for (l in 20:32){
          d = d[!is.na(d[,l]),]
        }
        fit6 = lm(modal~ Per.Capita.GSDP+production+yield+
                    amount+market + date + rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                  +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                  +rainfall_lead10+rainfall_lead11+rainfall_lead12+inter+0 ,
                  data=d)
        crse6 = clx(fit6,1, d$market)
        crse6
      })
     
      
      ###########################################
      #DiD for modal prices placebo
      ###########################################
      print(7)
      try({
        d = marketdat[marketdat$date<min(interdate1),]
        set.seed(1000000)
        sca = sort(unique(d$date))
        sca = sca[-c(1,length(sca))]
        ind = sample(x = sca,size = 1)
        d1 = d[d$date>=ind,]
        d2 = d[d$date<ind,]
        d1[d1$market%in%intervented,"inter"] = 1
        d = rbind(d1,d2)
        d$date = as.factor(d$date)
        d = d[!is.na(d$Per.Capita.GSDP),]
        d = d[!is.na(d$production),]
        d = d[!is.na(d$yield),]
        d = d[!is.na(d$amount),]
        d = d[!is.na(d$inter),]
        for (l in 20:32){
          d = d[!is.na(d[,l]),]
        }
        fit7 = lm(modal~ Per.Capita.GSDP+production+yield+
                    amount+market + date+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                  +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                  +rainfall_lead10+rainfall_lead11+rainfall_lead12+inter+0 ,
                  data=d)
        crse7 = clx(fit7,1, d$market)
        crse7
      })
      
      
      
      ###########################################
      #DiD for modal prices without features
      ###########################################
      print(8)
      try({
        d = marketdat
        d$date = as.factor(d$date)
        d = d[!is.na(d$Per.Capita.GSDP),]
        d = d[!is.na(d$production),]
        d = d[!is.na(d$yield),]
        d = d[!is.na(d$amount),]
        d = d[!is.na(d$inter),]
        
        fit8 = lm(modal~ market + date +inter +0,
                  data=d)
        crse8 = clx(fit8,1, d$market)
        crse8
      })
      
      
      
      ###########################################
      #DiD for modal prices within karnataka
      ###########################################
      #here volume fraction only depend on time and state
      #from the paper table 3 first column
      print(9)
      try({
        d = marketdat
        d = d[d$state=="karnataka",]
        d$date = as.factor(d$date)
        d = d[!is.na(d$Per.Capita.GSDP),]
        d = d[!is.na(d$production),]
        d = d[!is.na(d$yield),]
        d = d[!is.na(d$amount),]
        d = d[!is.na(d$inter),]
        for (l in 20:32){
          d = d[!is.na(d[,l]),]
        }
        fit9 = lm(modal~ market+date+production+yield+
                    Per.Capita.GSDP+
                    amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                  +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                  +rainfall_lead10+rainfall_lead11+rainfall_lead12+inter+0,
                  data=d)
        
        crse9 = clx(fit9,1, d$market)
        crse9
      })
      
      
      
      ###########################################
      #DiD for modal prices state level
      ###########################################
      #here volume_fraction depend on time and state
      print(10)
      try({
        fit10 = lm(wmean~ state+ date +production+yield+
                     Per.Capita.GSDP+
                     amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                   +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                   +rainfall_lead10+rainfall_lead11+rainfall_lead12+ volume_fraction +0,
                   data=dat)
        
        crse10 = clx(fit10,1, dat$state)
        crse10
      })
      
      
      ###########################################
      #DiD for modal prices state fixed
      ###########################################
      #here volume_fraction depend on time and state
      print(11)
      try({
        d = marketdat
        
        d_1 = d[d$state=="karnataka",]
        d_1 = join(d_1,notr,by = "date",type = "left")
        d_2 = d[d$state!="karnataka",]
        d_2$no.inter = 0
        d_2$volume_fraction = 0
        d = rbind(d_1,d_2)
        d$date = as.factor(d$date)
        d = na.omit(d[,-c(4,5,7:11)])
        fit11 = lm(modal~ state+ date +production+yield+
                     Per.Capita.GSDP+
                     amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                   +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                   +rainfall_lead10+rainfall_lead11+rainfall_lead12+ volume_fraction +0,
                   data=d)
        
        crse11 = clx(fit11,1,d$state)
        crse11
      })
      
      
      
      ###########################################
      #propensity score weighting (by logit model)
      ###########################################
      print(12)
      try({
        wei = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/propensity score weighting/weight/",product,timemode,"weight.xlsx"),sheet = 1)
        d = marketdat
        d$date = as.factor(d$date)
        d = join(d,wei,by = "market",type = "left")
        d = na.omit(d[,-c(4,5,7:11)])
        fit12 = lm(modal~ market+date +production+yield
                   +Per.Capita.GSDP+amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                   +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                   +rainfall_lead10+rainfall_lead11+rainfall_lead12 + inter +0,
                   data=d,weights = w)
        
        crse12 = clx(fit12,1, d$market)
        crse12
      })
      
      
      ###########################################
      #propensity score weighting (by probit)
      ###########################################
      print(13)
      try({
        wei = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/propensity score weighting/weight/",product,timemode,"weight probit.xlsx"),sheet = 1)
        d = marketdat
        d$date = as.factor(d$date)
        d = join(d,wei,by = "market",type = "left")
        d = na.omit(d[,-c(4,5,7:11)])
        fit13 = lm(modal~ market+date +production+yield
                   +Per.Capita.GSDP+amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                   +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                   +rainfall_lead10+rainfall_lead11+rainfall_lead12+inter+0,
                   data=d,weights = w)
        
        crse13 = clx(fit13,1, d$market)
        crse13
      })
      
      
      ###########################################
      #modal add month/week
      ###########################################
      print(14)
      try({
        d = marketdat
        d$month = month(d$date)
        
        d$week = stri_datetime_fields(d$date)$WeekOfMonth
        d$week = as.factor(d$week)
        d$month = as.factor(d$month)
        d$date = as.factor(d$date)
        d = d[!is.na(d$Per.Capita.GSDP),]
        d = d[!is.na(d$production),]
        d = d[!is.na(d$yield),]
        d = d[!is.na(d$amount),]
        d = d[!is.na(d$inter),]
        for (l in 20:32){
          d = d[!is.na(d[,l]),]
        }
        if (br=="week"){
          fit14 = lm(modal~ market+week + month + date +production+yield+
                       Per.Capita.GSDP+amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                     +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                     +rainfall_lead10+rainfall_lead11+rainfall_lead12+inter+0 ,
                     data=d)
          
          crse14 = clx(fit14,1, d$market)
          crse14
        }
        if (br=="month"){
          fit14 = lm(modal~ market + month + date +production+yield+
                       Per.Capita.GSDP+amount+inter+0 ,
                     data=d)
          
          crse14 = clx(fit14,1, d$market)
          crse14
        }
      })
      
      
      ###########################################
      #modal state specific trend
      ###########################################
      print(15)
      try({
        d = marketdat
        d$date = as.factor(d$date)
        d$timenumber = as.numeric(d$date)
        d = na.omit(d[,-c(4,5,7:11)])
        fit15 = lm(modal~ state + state:timenumber +market+date +
                     Per.Capita.GSDP+production+yield+
                     amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                   +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                   +rainfall_lead10+rainfall_lead11+rainfall_lead12+ inter+0,
                   data=d)
        
        crse15 = clx(fit15,1, d$market)
        crse15
      })
      
      
      ###########################################
      #modal market specific trend
      ###########################################
      print(16)
      try({
        d = marketdat
        d$date = as.factor(d$date)
        d$timenumber = as.numeric(d$date)
        d = d[!is.na(d$Per.Capita.GSDP),]
        d = d[!is.na(d$production),]
        d = d[!is.na(d$yield),]
        d = d[!is.na(d$amount),]
        d = d[!is.na(d$inter),]
        for (l in 20:32){
          d = d[!is.na(d[,l]),]
        }
        fit16 = lm(modal~ market + market:timenumber +date+production+yield+
                     Per.Capita.GSDP+
                     amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                   +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                   +rainfall_lead10+rainfall_lead11+rainfall_lead12+ inter+0,
                   data=d)
        
        crse16 = clx(fit16,1, d$market)
        crse16
      })
      
      
      ###########################################
      #DiD for modal prices state level  num
      ###########################################
      #here volume_fraction depend on time and state
      print(17)
      try({
        fit17 = lm(wmean~ state+ date +
                     Per.Capita.GSDP+production+yield+
                     amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                   +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                   +rainfall_lead10+rainfall_lead11+rainfall_lead12+ no.inter +0,
                   data=dat)
        
        crse17 = clx(fit17,1, dat$state)
        crse17
      })
      
      
      ###########################################
      #DiD for modal prices state fixed  num
      ###########################################
      #here volume_fraction depend on time and state
      print(18)
      try({
        d = marketdat
        
        d_1 = d[d$state=="karnataka",]
        d_1 = join(d_1,notr,by = "date",type = "left")
        d_2 = d[d$state!="karnataka",]
        d_2$no.inter = 0
        d_2$volume_fraction = 0
        d = rbind(d_1,d_2)
        d$date = as.factor(d$date)
        d = d[!is.na(d$Per.Capita.GSDP),]
        d = d[!is.na(d$production),]
        d = d[!is.na(d$yield),]
        d = d[!is.na(d$amount),]
        d = d[!is.na(d$inter),]
        for (l in 20:32){
          d = d[!is.na(d[,l]),]
        }
        fit18 = lm(modal~ state+ date+
                     Per.Capita.GSDP+
                     amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                   +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                   +rainfall_lead10+rainfall_lead11+rainfall_lead12+ no.inter +0,
                   data=d)
        
        crse18 = clx(fit18,1,d$state)
        crse18
      })
      
      
      ###########################################
      #DiD for parallel with leads and lags
      ###########################################
      print(19)
      try({
        d = marketdat
        d$date = as.factor(d$date)
        d = d[!is.na(d$Per.Capita.GSDP),]
        d = d[!is.na(d$production),]
        d = d[!is.na(d$yield),]
        d = d[!is.na(d$amount),]
        d = d[!is.na(d$inter),]
        for (l in 20:32){
          d = d[!is.na(d[,l]),]
        }
        fit19 = lm(modal~ market+date +production+yield+
                     Per.Capita.GSDP+
                     amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                   +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                   +rainfall_lead10+rainfall_lead11+rainfall_lead12+ lead+lead4+lead3+lead2+lead1+lag1+lag2+lag3+lag4+lag +0,
                   data=d)
        
        crse19 = clx(fit19,1, d$market)
        crse19
      })
      
      
      ###########################################
      #DiD for CV interim removed
      ###########################################
      
      print(20)
      try({
        dat = dat[(dat$date<min(interdate2))|(dat$date>=max(interdate2)),]
        fit20 = lm(CV~ state+ date  +
                     Per.Capita.GSDP+production+yield+
                     amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                   +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                   +rainfall_lead10+rainfall_lead11+rainfall_lead12+ no.inter +0,
                   data=dat)
        
        crse20 = clx(fit20,1,dat$state)
        crse20
      })
      
      
      ###########################################
      #DiD for VWCV interim removed
      ###########################################
      
      print(21)
      try({
        dat = dat[(dat$date<min(interdate2))|(dat$date>=max(interdate2)),]
        fit21 = lm(VWCV~ state+ date +production+yield+
                     Per.Capita.GSDP+
                     amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                   +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                   +rainfall_lead10+rainfall_lead11+rainfall_lead12+ no.inter+0,
                   data=dat)
        
        crse21 = clx(fit21,1,dat$state)
        crse21
      })
      
      
      
      ###########################################
      #DiD for Range interim removed
      ###########################################
      print(22)
      try({
        dat = dat[(dat$date<min(interdate2))|(dat$date>=max(interdate2)),]
        fit22 = lm(R~ state+ date +
                     Per.Capita.GSDP+production+yield+
                     amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                   +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                   +rainfall_lead10+rainfall_lead11+rainfall_lead12+ no.inter+0 ,
                   data=dat)
        
        crse22 = clx(fit22,1,dat$state)
        crse22
      })
      ###########################################
      #propensity score weighting remove interim (by logit model)
      ###########################################
      print(23)
      try({
        wei = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/propensity score weighting/weight/",product,timemode,"weight.xlsx"),sheet = 1)
        d = marketdat[marketdat$date<min(interdate1)|marketdat$date>=max(interdate1),]
        d$date = as.factor(d$date)
        d = join(d,wei,by = "market",type = "left")
        d = na.omit(d[,-c(4,5,7:11)])
        fit23 = lm(modal~ market+date +production+yield
                   +Per.Capita.GSDP+amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                   +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                   +rainfall_lead10+rainfall_lead11+rainfall_lead12 + inter +0,
                   data=d,weights = w)
        
        crse23 = clx(fit23,1, d$market)
        crse23
      })
      
      
      ###########################################
      #propensity score weighting remove interim (by probit)
      ###########################################
      print(24)
      try({
        wei = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/propensity score weighting/weight/",product,timemode,"weight probit.xlsx"),sheet = 1)
        d = marketdat[marketdat$date<min(interdate1)|marketdat$date>=max(interdate1),]
        d$date = as.factor(d$date)
        d = join(d,wei,by = "market",type = "left")
        d = na.omit(d[,-c(4,5,7:11)])
        fit24 = lm(modal~ market+date +production+yield
                   +Per.Capita.GSDP+amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                   +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                   +rainfall_lead10+rainfall_lead11+rainfall_lead12+inter+0,
                   data=d,weights = w)
        
        crse24 = clx(fit24,1, d$market)
        crse24
      })
      
      ###########################################
      #DiD for modal prices(base)
      ###########################################
      print(25)
      try({
        d = marketdat1
        d$date = as.factor(d$date)
        d = d[!is.na(d$Per.Capita.GSDP),]
        d = d[!is.na(d$production),]
        d = d[!is.na(d$yield),]
        d = d[!is.na(d$amount),]
        d = d[!is.na(d$inter),]
        for (l in 20:32){
          d = d[!is.na(d[,l]),]
        }
        d <- within(d, inter <- relevel(inter, ref = "1"))
        fit25 = lm(modal~ market+date+
                    Per.Capita.GSDP+production+yield+
                    amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                  +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                  +rainfall_lead10+rainfall_lead11+rainfall_lead12+size:inter,
                  data=d)
        
        crse25 = clx(fit25,1, d$market)
        crse25
      })
      
      dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/",timemode,sep = ""))
      dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/",timemode,"/",product,sep = ""))
      setwd(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/",timemode,"/",product,sep = ""))
      write.csv(crse1,paste(product,"modal.csv"))
      write.csv(crse2,paste(product,"CV.csv"))
      write.csv(crse3,paste(product,"VWCV.csv"))
      write.csv(crse4,paste(product,"R.csv"))
      write.csv(crse5,paste(product,"modal parallel.csv"))
      write.csv(crse6,paste(product,"modal drop interim.csv"))
      write.csv(crse7,paste(product,"modal placebo.csv"))
      write.csv(crse8,paste(product,"modal without features.csv"))
      write.csv(crse9,paste(product,"modal within karnataka.csv"))
      write.csv(crse10,paste(product,"modal state level.csv"))
      write.csv(crse11,paste(product,"modal state fixed.csv"))
      write.csv(crse12,paste(product,"modal psw logit.csv"))
      write.csv(crse13,paste(product,"modal psw probit.csv"))
      write.csv(crse14,paste(product,"modal day info.csv"))
      write.csv(crse15,paste(product,"modal state specific trend.csv"))
      write.csv(crse16,paste(product,"modal market specific trend.csv"))
      write.csv(crse17,paste(product,"modal state level num.csv"))
      write.csv(crse18,paste(product,"modal state fixed num.csv"))
      write.csv(crse19,paste(product,"modal lead lag.csv"))
      write.csv(crse20,paste(product,"CV drop interim.csv"))
      write.csv(crse21,paste(product,"VWCV drop interim.csv"))
      write.csv(crse22,paste(product,"R drop interim.csv"))
      write.csv(crse23,paste(product,"modal psw logit drop interim.csv"))
      write.csv(crse24,paste(product,"modal psw probit drop interim.csv"))
      write.csv(crse25,paste(product,"modal add size.csv"))
      
      a1 = stargazer(crse1)
      a2 = stargazer(crse2)
      a3 = stargazer(crse3)
      a4 = stargazer(crse4)
      a5 = stargazer(crse5)
      a6 = stargazer(crse6)
      a7 = stargazer(crse7)
      a8 = stargazer(crse8)
      a9 = stargazer(crse9)
      a10 = stargazer(crse10)
      a11 = stargazer(crse11)
      a12 = stargazer(crse12)
      a13 = stargazer(crse13)
      a14 = stargazer(crse14)
      a15 = stargazer(crse15)
      a16 = stargazer(crse16)
      a17 = stargazer(crse17)
      a18 = stargazer(crse18)
      a19 = stargazer(crse19)
      a20 = stargazer(crse20)
      a21 = stargazer(crse21)
      a22 = stargazer(crse22)
      a23 = stargazer(crse23)
      a24 = stargazer(crse24)
      a25 = stargazer(crse25)
      write(a1,paste(product,"modal.txt"))
      write(a2,paste(product,"CV.txt"))
      write(a3,paste(product,"VWCV.txt"))
      write(a4,paste(product,"R.txt"))
      write(a5,paste(product,"modal parallel.txt"))
      write(a6,paste(product,"modal drop interim.txt"))
      write(a7,paste(product,"modal placebo.txt"))
      write(a8,paste(product,"modal without features.txt"))
      write(a9,paste(product,"modal within karnataka.txt"))
      write(a10,paste(product,"modal state level.txt"))
      write(a11,paste(product,"modal state fixed.txt"))
      write(a12,paste(product,"modal psw logit.txt"))
      write(a13,paste(product,"modal psw probit.txt"))
      write(a14,paste(product,"modal day info.txt"))
      write(a15,paste(product,"modal state specific trend.txt"))
      write(a16,paste(product,"modal market specific trend.txt"))
      write(a17,paste(product,"modal state level num.txt"))
      write(a18,paste(product,"modal state fixed num.txt"))
      write(a19,paste(product,"modal lead lag.txt"))
      write(a20,paste(product,"CV drop interim.txt"))
      write(a21,paste(product,"VWCV drop interim.txt"))
      write(a22,paste(product,"R drop interim.txt"))
      write(a23,paste(product,"modal psw logit drop interim.txt"))
      write(a24,paste(product,"modal psw probit drop interim.txt"))
      write(a25,paste(product,"modal add size.txt"))
      
      re = rbind(re,crse1[nrow(crse1),])
      re = rbind(re,crse2[nrow(crse2),])
      re = rbind(re,crse3[nrow(crse3),])
      re = rbind(re,crse4[nrow(crse4),])
      re = rbind(re,crse5[nrow(crse5),])
      re = rbind(re,crse6[nrow(crse6),])
      re = rbind(re,crse7[nrow(crse7),])
      re = rbind(re,crse8[nrow(crse8),])
      re = rbind(re,crse9[nrow(crse9),])
      re = rbind(re,crse10[nrow(crse10),])
      re = rbind(re,crse11[nrow(crse11),])
      re = rbind(re,crse12[nrow(crse12),])
      re = rbind(re,crse13[nrow(crse13),])
      re = rbind(re,crse14[nrow(crse14),])
      re = rbind(re,crse15[rownames(crse15)=="inter1",])
      re = rbind(re,crse16[rownames(crse16)=="inter1",])
      re = rbind(re,crse17[nrow(crse17),])
      re = rbind(re,crse18[nrow(crse18),])
      re = rbind(re,crse19[(nrow(crse19)-9):nrow(crse19),])
      re = rbind(re,crse20[nrow(crse20),])
      re = rbind(re,crse21[nrow(crse21),])
      re = rbind(re,crse22[nrow(crse22),])
      re = rbind(re,crse23[nrow(crse23),])
      re = rbind(re,crse24[nrow(crse24),])
      re = rbind(re,crse25[(nrow(crse25)-3):nrow(crse25),])
      
      colnames(re) = c("Estimate","Std.Error","t value","Pr(>|t|)")
      
      trial = c("modal","CV","VWCV","R","modal parallel","modal drop interim","modal placebo","modal without features",
                "modal within karnataka","modal state level","modal state fixed","modal psw logit","modal psw probit",
                "modal day info","modal state specific trend","modal market specific trend","modal state level num",
                "modal state fixed num","lead","lead4","lead3","lead2","lead1","lag1","lag2","lag3","lag4","lag","CV drop interim","VWCV drop interim"
                ,"R drop interim","modal psw logit drop interim","modal psw probit drop interim","modal add size_large","modal add size_medium_large","modal add size_medium_small","modal add size_small")
      re = cbind(trial = trial,re)
      
      #calculate the effect of the coefficient by dividing by mean before treatment
      re = as.data.frame(re)
      effect = as.numeric(as.numeric(as.character(re$Estimate))/meanbefore)
      ree = cbind(trial = trial,effect = effect)
      
      addWorksheet(wb,product)
      addWorksheet(wbe,product)
      
      writeData(wb,sheet = product,re)
      writeData(wbe,sheet = product,ree)

    
  }
  saveWorkbook(wbe,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/",timemode," inter effects.xlsx",sep = ""),overwrite = T)
  saveWorkbook(wb,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/",timemode," inter coefficients.xlsx",sep = ""),overwrite = T)
}
