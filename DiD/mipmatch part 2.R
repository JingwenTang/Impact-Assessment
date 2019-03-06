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
  
  
  for (p in c(1,2,3,5,6,7)){
    
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
    mar = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/mipmatch/",product," selected markets.xlsx",sep = ""),colNames = F)$X1
    marketdat = marketdat[marketdat$market%in%mar,]
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
      marketdat[(marketdat$market==intervented1[i])&(temp$date>=(interdate1[i]+0)),"inter"] = 1
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
    
    
    
    
    
    setwd("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/mipmatch/")
    write.csv(crse1,paste(product,"modal.csv"))
   
    re = rbind(re,crse1[nrow(crse1),])
    
    
    colnames(re) = c("Estimate","Std.Error","t value","Pr(>|t|)")
    
    trial = c("mipmatch")    
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
  saveWorkbook(wbe,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/mipmatch/" ,"inter effects mipmatch.xlsx",sep = ""),overwrite = T)
  saveWorkbook(wb,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/mipmatch/", "inter coefficients mipmatch.xlsx",sep = ""),overwrite = T)
}
