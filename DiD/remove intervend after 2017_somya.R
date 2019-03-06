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
Timemode = c("10-17 week","10-17 week all","13-17 week common types","13-17 week all common types") #the time unit and lags chosen for the model in synthetic control
br="month"
for (tm in c(1,2,5,6)){
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
  colnames(intervention) = c("number","code","market","date")
  intervention$date = as.numeric(intervention$date)
  intervention$date = as.Date(intervention$date-25569, origin = "1970-01-01")
  after2017 = intervention$market[intervention$date>="2017-01-01"]
  
  
  for (p in 1:length(Product)){
    
    re = matrix(0,45,4)
    ree=matrix(0,45,1)
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
    leaddate1 = interdate1 %m+% weeks(-4)
    leaddate2 = interdate1 %m+% weeks(-8)
    leaddate3 = interdate1 %m+% weeks(-12)
    leaddate4 = interdate1 %m+% weeks(-16)
    
    wleaddate1 = interdate1 %m+% weeks(-1)
    wleaddate2 = interdate1 %m+% weeks(-2)
    wleaddate3 = interdate1 %m+% weeks(-3)
    wleaddate4 = interdate1 %m+% weeks(-4)
    
    lagdate1 = interdate1 %m+% months(1)
    lagdate2 = interdate1 %m+% months(2)
    lagdate3 = interdate1 %m+% months(3)
    lagdate4 = interdate1 %m+% months(4)
    lagdate5 = interdate1 %m+% months(5)
    lagdate6 = interdate1 %m+% months(6)
    marketdat$inter = 0
    marketdat$lead1 = 0
    marketdat$lead2 = 0
    marketdat$lead3 = 0
    marketdat$lead4 = 0
    marketdat$lead = 0
    marketdat$lag1 = 0
    marketdat$lag2 = 0
    
    marketdat$lead1_2 = 0
    marketdat$lead2_2 = 0
    marketdat$lead3_2 = 0
    marketdat$lead4_2 = 0
    marketdat$lead_2 = 0
    
    marketdat$lead_3 = 0
    
    #marketdat$lag3 = 0
    #marketdat$lag4 = 0
    #marketdat$lag = 0
    marketdat$date = as.Date(marketdat$date-25569,origin = "1970-01-01")
    for (i in 1:length(intervented1)){
      temp = marketdat[marketdat$market==intervented1[i],]
      marketdat[marketdat$market==intervented1[i],][temp$date>=(interdate1[i]+0),"inter"] = 1
      marketdat[marketdat$market==intervented1[i],][(temp$date>=leaddate1[i])&(temp$date<interdate1[i]),"lead1"] = 1
      marketdat[marketdat$market==intervented1[i],][(temp$date>=leaddate2[i])&(temp$date<leaddate1[i]),"lead2"] = 1
      marketdat[marketdat$market==intervented1[i],][(temp$date>=leaddate3[i])&(temp$date<leaddate2[i]),"lead3"] = 1
      marketdat[marketdat$market==intervented1[i],][(temp$date>=leaddate4[i])&(temp$date<leaddate3[i]),"lead4"] = 1
      marketdat[marketdat$market==intervented1[i],][temp$date<leaddate4[i],"lead"] = 1
      marketdat[marketdat$market==intervented1[i],][(temp$date<=lagdate6[i])&(temp$date>=interdate1[i]),"lag1"] = 1
      marketdat[marketdat$market==intervented1[i],][temp$date>lagdate6[i],"lag2"] = 1
      #weeks leads
      marketdat[marketdat$market==intervented1[i],][temp$date==wleaddate1[i],"lead1_2"] = 1
      marketdat[marketdat$market==intervented1[i],][temp$date==wleaddate2[i],"lead2_2"] = 1
      marketdat[marketdat$market==intervented1[i],][temp$date==wleaddate3[i],"lead3_2"] = 1
      marketdat[marketdat$market==intervented1[i],][temp$date==wleaddate4[i],"lead4_2"] = 1
      marketdat[marketdat$market==intervented1[i],][temp$date<wleaddate4[i],"lead_2"] = 1
      #marketdat[marketdat$market==intervented1[i],][temp$date==lagdate1[i],"lag1"] = 1
      #marketdat[marketdat$market==intervented1[i],][temp$date==lagdate2[i],"lag2"] = 1
      #marketdat[marketdat$market==intervented1[i],][temp$date==lagdate3[i],"lag3"] = 1
      #marketdat[marketdat$market==intervented1[i],][temp$date==lagdate4[i],"lag4"] = 1
      marketdat[marketdat$market==intervented1[i],][(temp$date<=lagdate6[i])&(temp$date>=interdate1[i]),"lag1"] = 1
      marketdat[marketdat$market==intervented1[i],][temp$date>lagdate6[i],"lag2"] = 1
      #marketdat[marketdat$market==intervented1[i],][temp$date==(interdate1[i]+0),"lead0"] = 1
      #not divide lead
      marketdat[marketdat$market==intervented1[i],][temp$date<interdate1[i],"lead_3"] = 1
      marketdat[marketdat$market==intervented1[i],][(temp$date<=lagdate6[i])&(temp$date>=interdate1[i]),"lag1"] = 1
      marketdat[marketdat$market==intervented1[i],][temp$date>lagdate6[i],"lag2"] = 1
      
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
    
    #####to remove the data points when only with treated or untreated markets
    marketdat$treat = (marketdat$state=="karnataka")
    marketdat$treat = as.factor(marketdat$treat)
    newmarketdat = c()
    da = unique(marketdat$date)
    for (l in 1:length(da)){
      temp = marketdat[marketdat$date == da[l],]
      if (length(unique(temp$treat))==2){
        newmarketdat = rbind(newmarketdat,temp)
      }
    }
    marketdat = newmarketdat[,-ncol(newmarketdat)]
    
    marketdat = marketdat[!marketdat$market%in%after2017,]
    
    
    ####for active quantile divide using all markets data:
    #market modal price monthly
    marketdat1 = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",timemode,"/",product," eleventh data.xlsx",sep = ""),sheet = 1)
    colnames(marketdat1)[1] = "date"
    marketdat1 = marketdat1[marketdat1$market!="virtual",]
    temp = intervented%in%unique(marketdat1$market)
    intervented1 = intervented[temp]
    interdate1 = interdate[temp]
    #lead and lags in month
    leaddate1 = interdate1 %m+% weeks(-4)
    leaddate2 = interdate1 %m+% weeks(-8)
    leaddate3 = interdate1 %m+% weeks(-12)
    leaddate4 = interdate1 %m+% weeks(-16)
    
    wleaddate1 = interdate1 %m+% weeks(-1)
    wleaddate2 = interdate1 %m+% weeks(-2)
    wleaddate3 = interdate1 %m+% weeks(-3)
    wleaddate4 = interdate1 %m+% weeks(-4)
    
    lagdate1 = interdate1 %m+% months(1)
    lagdate2 = interdate1 %m+% months(2)
    lagdate3 = interdate1 %m+% months(3)
    lagdate4 = interdate1 %m+% months(4)
    lagdate5 = interdate1 %m+% months(5)
    lagdate6 = interdate1 %m+% months(6)
    marketdat1$inter = 0
    marketdat1$lead1 = 0
    marketdat1$lead2 = 0
    marketdat1$lead3 = 0
    marketdat1$lead4 = 0
    marketdat1$lead = 0
    marketdat1$lag1 = 0
    marketdat1$lag2 = 0
    
    marketdat1$lead1_2 = 0
    marketdat1$lead2_2 = 0
    marketdat1$lead3_2 = 0
    marketdat1$lead4_2 = 0
    marketdat1$lead_2 = 0
    
    marketdat1$lead_3 = 0
    
    #marketdat1$lag3 = 0
    #marketdat1$lag4 = 0
    #marketdat1$lag = 0
    marketdat1$date = as.Date(marketdat1$date-25569,origin = "1970-01-01")
    for (i in 1:length(intervented1)){
      temp = marketdat1[marketdat1$market==intervented1[i],]
      marketdat1[marketdat1$market==intervented1[i],][temp$date>=(interdate1[i]+0),"inter"] = 1
      marketdat1[marketdat1$market==intervented1[i],][(temp$date>=leaddate1[i])&(temp$date<interdate1[i]),"lead1"] = 1
      marketdat1[marketdat1$market==intervented1[i],][(temp$date>=leaddate2[i])&(temp$date<leaddate1[i]),"lead2"] = 1
      marketdat1[marketdat1$market==intervented1[i],][(temp$date>=leaddate3[i])&(temp$date<leaddate2[i]),"lead3"] = 1
      marketdat1[marketdat1$market==intervented1[i],][(temp$date>=leaddate4[i])&(temp$date<leaddate3[i]),"lead4"] = 1
      marketdat1[marketdat1$market==intervented1[i],][temp$date<leaddate4[i],"lead"] = 1
      marketdat1[marketdat1$market==intervented1[i],][(temp$date<=lagdate6[i])&(temp$date>=interdate1[i]),"lag1"] = 1
      marketdat1[marketdat1$market==intervented1[i],][temp$date>lagdate6[i],"lag2"] = 1
      #weeks leads
      marketdat1[marketdat1$market==intervented1[i],][temp$date==wleaddate1[i],"lead1_2"] = 1
      marketdat1[marketdat1$market==intervented1[i],][temp$date==wleaddate2[i],"lead2_2"] = 1
      marketdat1[marketdat1$market==intervented1[i],][temp$date==wleaddate3[i],"lead3_2"] = 1
      marketdat1[marketdat1$market==intervented1[i],][temp$date==wleaddate4[i],"lead4_2"] = 1
      marketdat1[marketdat1$market==intervented1[i],][temp$date<wleaddate4[i],"lead_2"] = 1
      #marketdat1[marketdat1$market==intervented1[i],][temp$date==lagdate1[i],"lag1"] = 1
      #marketdat1[marketdat1$market==intervented1[i],][temp$date==lagdate2[i],"lag2"] = 1
      #marketdat1[marketdat1$market==intervented1[i],][temp$date==lagdate3[i],"lag3"] = 1
      #marketdat1[marketdat1$market==intervented1[i],][temp$date==lagdate4[i],"lag4"] = 1
      marketdat1[marketdat1$market==intervented1[i],][(temp$date<=lagdate6[i])&(temp$date>=interdate1[i]),"lag1"] = 1
      marketdat1[marketdat1$market==intervented1[i],][temp$date>lagdate6[i],"lag2"] = 1
      #marketdat1[marketdat1$market==intervented1[i],][temp$date==(interdate1[i]+0),"lead0"] = 1
      #not divide lead
      marketdat1[marketdat1$market==intervented1[i],][temp$date<=interdate1[i],"lead_3"] = 1
      marketdat1[marketdat1$market==intervented1[i],][(temp$date<=lagdate6[i])&(temp$date>=interdate1[i]),"lag1"] = 1
      marketdat1[marketdat1$market==intervented1[i],][temp$date>lagdate6[i],"lag2"] = 1
      
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
    thresh2 = sum(aa$amount)*0.5
    for (l in 1:nrow(aa)){
      aaa = sum(aa$amount[1:l])
      if (aaa<=thresh2) l2 = l
    }
    large = aa$market[1:l2]
    small = aa$market[l2:nrow(aa)]
    marketdat1$size = "small"
    
    
    marketdat1[marketdat1$market%in%small,"size"] = "small"
    marketdat1[marketdat1$market%in%large,"size"] = "large"
    
    #####to remove the data points when only with treated or untreated markets
    marketdat1$treat = (marketdat1$state=="karnataka")
    marketdat1$treat = as.factor(marketdat1$treat)
    newmarketdat1 = c()
    da = unique(marketdat1$date)
    for (l in 1:length(da)){
      temp = marketdat1[marketdat1$date == da[l],]
      if (length(unique(temp$treat))==2){
        newmarketdat1 = rbind(newmarketdat1,temp)
      }
    }
    marketdat1 = newmarketdat1[,-ncol(newmarketdat1)]
    
    marketdat1 = marketdat1[!marketdat1$market%in%after2017,]
    
    ########state level data
    
    
    dat = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",timemode,"/",product," eleventh data.xlsx",sep = ""),sheet = 1)
    colnames(dat)[1] = "date"
    dat = dat[dat$market!="virtual",]
    dat = dat[!dat$market%in%after2017,]
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
    origintable = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",timemode,"/original table.xlsx",sep = ""),sheet=1)
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
    
    
    
    for (l in c(7:11,15:17,21:35)){
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
    dat = na.omit(dat)
    
    
    
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
    crse19 = matrix(0,7,4)
    crse20 = t(rep(0,4))
    crse21 = t(rep(0,4))
    crse22 = t(rep(0,4))
    crse23 = t(rep(0,4))
    crse24 = t(rep(0,4))
    crse25 = matrix(0,4,4)
    crse26 = matrix(0,7,4)
    crse27 = matrix(0,3,4)
    crse28 = t(rep(0,4))
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
      
      d = na.omit(d)
      
      fit1 = lm(log(modal)~ market+date+annual+total_ar+lit_ru+ptmrkt+agrl_t+
                  Per.Capita.GSDP+production+yield+
                  log(amount)+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                +rainfall_lead10+rainfall_lead11+rainfall_lead12+inter+0,
                data=d)
      
      crse1 = clx(fit1,1, d$market)
      crse1
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
      
      d = na.omit(d)
      
      fit5 = lm(log(modal)~ Per.Capita.GSDP+production+yield+annual+total_ar+lit_ru+ptmrkt+agrl_t+
                  log(amount)+market+time  + date+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                +rainfall_lead10+rainfall_lead11+rainfall_lead12 + treat:time+0 ,
                data=d)
      crse5 = clx(fit5,1, d$market)
      crse5
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
      
      d = na.omit(d)
      
      fit7 = lm(log(modal)~ Per.Capita.GSDP+production+yield+annual+total_ar+lit_ru+ptmrkt+agrl_t+
                  log(amount)+market + date+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
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
      
      fit8 = lm(log(modal)~ market + date +inter +0,
                data=d)
      crse8 = clx(fit8,1, d$market)
      crse8
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
      
      d = na.omit(d)
      
      if (br=="week"){
        fit14 = lm(log(modal)~ market+week + month + date +production+yield+annual+total_ar+lit_ru+ptmrkt+agrl_t+
                     Per.Capita.GSDP+log(amount)+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                   +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                   +rainfall_lead10+rainfall_lead11+rainfall_lead12+inter+0 ,
                   data=d)
        
        crse14 = clx(fit14,1, d$market)
        crse14
      }
      if (br=="month"){
        fit14 = lm(log(modal)~ market + month + date +production+yield+
                     Per.Capita.GSDP+log(amount)+inter+0 ,
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
      d = na.omit(d)
      fit15 = lm(log(modal)~ state + state:timenumber +market+date +
                   Per.Capita.GSDP+production+yield+annual+total_ar+lit_ru+ptmrkt+agrl_t+
                   log(amount)+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
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
      
      d = na.omit(d)
      
      fit16 = lm(log(modal)~ market + market:timenumber +date+production+yield+
                   Per.Capita.GSDP+annual+total_ar+lit_ru+ptmrkt+agrl_t+
                   log(amount)+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                 +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                 +rainfall_lead10+rainfall_lead11+rainfall_lead12+ inter+0,
                 data=d)
      
      crse16 = clx(fit16,1, d$market)
      crse16
    })


    ###########################################
    #DiD for parallel with leads and lags lead not divide
    ###########################################
    print(27)
    try({
      d = marketdat
      d$date = as.factor(d$date)
      d = d[!is.na(d$Per.Capita.GSDP),]
      d = d[!is.na(d$production),]
      d = d[!is.na(d$yield),]
      d = d[!is.na(d$amount),]
      d = d[!is.na(d$inter),]
      
      d = na.omit(d)
      
      fit27 = lm(log(modal)~ market+date +production+yield+
                   Per.Capita.GSDP+annual+total_ar+lit_ru+ptmrkt+agrl_t+
                   log(amount)+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                 +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                 +rainfall_lead10+rainfall_lead11+rainfall_lead12+ lead_3+lag1+lag2 +0,
                 data=d)
      
      crse27 = clx(fit27,1, d$market)
      crse27
    })
    
    ###########################################
    #DiD for log amount
    ###########################################
    print(28)
    try({
      d = marketdat
      d$date = as.factor(d$date)
      d = d[!is.na(d$Per.Capita.GSDP),]
      d = d[!is.na(d$production),]
      d = d[!is.na(d$yield),]
      d = d[!is.na(d$amount),]
      d = d[!is.na(d$inter),]
      
      d = na.omit(d)
      
      fit28 = lm(log(amount)~ market+date+annual+total_ar+lit_ru+ptmrkt+agrl_t+
                  Per.Capita.GSDP+production+yield+
                 rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                +rainfall_lead10+rainfall_lead11+rainfall_lead12+inter+0,
                data=d)
      
      crse28 = clx(fit28,1, d$market)
      crse28
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
    write.csv(crse26,paste(product,"lead lag week.csv"))
    write.csv(crse27,paste(product,"lead lag lead not divide.csv"))
    write.csv(crse28,paste(product,"log amount regression.csv"))
    
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
    a26 = stargazer(crse26)
    a27 = stargazer(crse27)
    a28 = stargazer(crse28)
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
    write(a26,paste(product,"lead lag week.txt"))
    write(a27,paste(product,"lead lag lead not divide.txt"))
    write(a28,paste(product,"log amount regression.txt"))
    
    try({re[1,] = crse1[rownames(crse1)=="inter1",]})
    try({re[2,] = crse2[rownames(crse2)=="no.inter",]})
    try({re[3,] = crse3[rownames(crse3)=="no.inter",]})
    try({re[4,] = crse4[rownames(crse4)=="no.inter",]})
    try({re[5,] = crse5[rownames(crse5)=="time:treatTRUE",]})
    try({re[6,] = crse6[rownames(crse6)=="inter1",]})
    try({re[7,] = crse7[rownames(crse7)=="inter1",]})
    try({re[8,] = crse8[rownames(crse8)=="inter1",]})
    try({re[9,] = crse9[rownames(crse9)=="inter1",]})
    try({re[10,] = crse10[rownames(crse10)=="volume_fraction",]})
    try({re[11,] = crse11[rownames(crse11)=="volume_fraction",]})
    try({re[12,] = crse12[rownames(crse12)=="inter1",]})
    try({re[13,] = crse13[rownames(crse13)=="inter1",]})
    try({re[14,] = crse14[rownames(crse14)=="inter1",]})
    try({re[15,] = crse15[rownames(crse15)=="inter1",]})
    try({re[16,] = crse16[rownames(crse16)=="inter1",]})
    try({re[17,] = crse17[rownames(crse17)=="no.inter",]})
    try({re[18,] = crse18[rownames(crse18)=="no.inter",]})
    try({re[19,] = crse19[rownames(crse19)=="lead",]})
    try({re[20,] = crse19[rownames(crse19)=="lead4",]})
    try({re[21,] = crse19[rownames(crse19)=="lead3",]})
    try({re[22,] = crse19[rownames(crse19)=="lead2",]})
    try({re[23,] = crse19[rownames(crse19)=="lead1",]})
    try({re[24,] = crse19[rownames(crse19)=="lag1",]})
    try({re[25,] = crse19[rownames(crse19)=="lag2",]})
    try({re[26,] = crse20[rownames(crse20)=="no.inter",]})
    try({re[27,] = crse21[rownames(crse21)=="no.inter",]})
    try({re[28,] = crse22[rownames(crse22)=="no.inter",]})
    try({re[29,] = crse23[rownames(crse23)=="inter1",]})
    try({re[30,] = crse24[rownames(crse24)=="inter1",]})
    try({re[31,] = crse25[rownames(crse25)=="sizelarge:inter1",]})
    try({re[32,] = crse25[rownames(crse25)=="sizemedium_large:inter1",]})
    try({re[33,] = crse25[rownames(crse25)=="sizemedium_small:inter1",]})
    try({re[34,] = crse25[rownames(crse25)=="sizesmall:inter1",]})
    try({re[35,] = crse26[rownames(crse26)=="lead_2",]})
    try({re[36,] = crse26[rownames(crse26)=="lead4_2",]})
    try({re[37,] = crse26[rownames(crse26)=="lead3_2",]})
    try({re[38,] = crse26[rownames(crse26)=="lead2_2",]})
    try({re[39,] = crse26[rownames(crse26)=="lead1_2",]})
    try({re[40,] = crse26[rownames(crse26)=="lag1",]})
    try({re[41,] = crse26[rownames(crse26)=="lag2",]})
    try({re[42,] = crse27[rownames(crse27)=="lead_3",]})
    try({re[43,] = crse27[rownames(crse27)=="lag1",]})
    try({re[44,] = crse27[rownames(crse27)=="lag2",]})
    try({re[45,] = crse28[rownames(crse28)=="inter1",]})
    
    colnames(re) = c("Estimate","Std.Error","t value","Pr(>|t|)")
    
    trial = c("modal","CV","VWCV","R","modal parallel","modal drop interim","modal placebo","modal without features",
              "modal within karnataka","modal state level","modal state fixed","modal psw logit","modal psw probit",
              "modal day info","modal state specific trend","modal market specific trend","modal state level num",
              "modal state fixed num","lead","lead4","lead3","lead2","lead1","lag1","lag2","CV drop interim","VWCV drop interim"
              ,"R drop interim","modal psw logit drop interim","modal psw probit drop interim","modal add size_large","modal add size_medium_large","modal add size_medium_small","modal add size_small"
              ,"lead_2","lead4_2","lead3_2","lead2_2","lead1_2","lag1","lag2","lead_3","lag1","lag2","log mount")
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
  saveWorkbook(wbe,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/",timemode," inter effects removing one level removing after 2017.xlsx",sep = ""),overwrite = T)
  saveWorkbook(wb,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/",timemode," inter coefficients removing one level removing after 2017.xlsx",sep = ""),overwrite = T)
}
