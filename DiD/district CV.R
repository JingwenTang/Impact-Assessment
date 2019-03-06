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
  
  
  for (p in c(1:10,12:14)){
    
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
    
    ########district level data

    dat = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",timemode,"/",product," eleventh data.xlsx",sep = ""),sheet = 1)
    colnames(dat)[1] = "date"
    dat = dat[dat$market!="virtual",]
    rm = c()
    district = unique(dat$district)
    for (i in 1:length(district)){
      temp = dat[dat$district==district[i],]
      if (length(unique(temp$market))==1){
        rm = c(rm,district[i])
      }
    }
    dat = dat[!dat$district%in%rm,]
    
    
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
    Max = aggregate(dat$modal,by = list(dat$date,dat$district),max)
    colnames(Max) = c("date","district","max")
    Min = aggregate(dat$modal,by = list(dat$date,dat$district),min)
    colnames(Min) = c("date","district","min")
    m = aggregate(dat$modal,by = list(dat$date,dat$district),mean)
    colnames(m) = c("date","district","mean")
    sd = aggregate(dat$modal,by = list(dat$date,dat$district),sd)
    colnames(sd) = c("date","district","sd")
    dat = dat[!is.na(dat$amount),]
    wm = ddply(dat,.(date,district),summarise, wmean = weighted.mean(modal,amount))
    wsd = ddply(dat,.(date,district),summarise, wsd = weighted.sd(modal,amount))
    Dat = join(Max,Min,by = c("date","district"),type = "left")
    Dat = join(Dat,m,by = c("date","district"),type = "left")
    Dat = join(Dat,sd,by = c("date","district"),type = "left")
    Dat = join(Dat,wm,by = c("date","district"),type = "left")
    Dat = join(Dat,wsd,by = c("date","district"),type = "left")
    Dat = na.omit(Dat)
    Dat$R = (Dat$max - Dat$min)/Dat$mean
    Dat$CV = Dat$sd/Dat$mean
    Dat$VWCV = Dat$wsd/Dat$wmean
    newdat = as.data.frame(aggregate(dat$amount, list(dat$date,dat$district), sum,na.rm = T))
    colnames(newdat) = c("date","district","amount")
    Dat = join(Dat,newdat,by = c("date","district"),type = "left")
    
    
    
    for (l in c(7:11,15:17,20:33)){
      name = colnames(dat)[l]
      colnames(dat)[l] = "target"
      newdat = as.data.frame(ddply(dat,.(date,district),summarise, wmean = weighted.mean(target,amount,na.rm = T)))
      colnames(newdat) = c("date","district",name)
      Dat = join(Dat,newdat,by = c("date","district"),type = "left")
      colnames(dat)[l] = name
    }
    
    dat = Dat
    dat_1 = dat[dat$district=="karnataka",]
    dat_2 = dat[dat$district!="karnataka",]
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
    
    
    crse2 = t(rep(0,4))
    crse3 = t(rep(0,4))
    crse4 = t(rep(0,4))
    
    crse20 = t(rep(0,4))
    crse21 = t(rep(0,4))
    crse22 = t(rep(0,4))
    
    ###########################################
    #DiD for CV
    ###########################################
    
    print(2)
    try({
      fit2 = lm(CV~ district+ date  +
                  Per.Capita.GSDP+production+yield+
                  amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                +rainfall_lead10+rainfall_lead11+rainfall_lead12+ no.inter +0,
                data=dat)
      
      crse2 = clx(fit2,1,dat$district)
      crse2
    })
    
    
    ###########################################
    #DiD for VWCV
    ###########################################
    
    print(3)
    try({
      fit3 = lm(VWCV~ district+ date +production+yield+
                  Per.Capita.GSDP+
                  amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                +rainfall_lead10+rainfall_lead11+rainfall_lead12+ no.inter+0,
                data=dat)
      
      crse3 = clx(fit3,1,dat$district)
      crse3
    })
    
    
    
    ###########################################
    #DiD for Range
    ###########################################
    print(4)
    try({
      fit4 = lm(R~ district+ date +
                  Per.Capita.GSDP+production+yield+
                  amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                +rainfall_lead10+rainfall_lead11+rainfall_lead12+ no.inter+0 ,
                data=dat)
      
      crse4 = clx(fit4,1,dat$district)
      crse4
    })
    ###########################################
    #DiD for CV interim removed
    ###########################################
    
    print(20)
    try({
      dat = dat[(dat$date<min(interdate2))|(dat$date>=max(interdate2)),]
      fit20 = lm(CV~ district+ date  +
                   Per.Capita.GSDP+production+yield+
                   amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                 +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                 +rainfall_lead10+rainfall_lead11+rainfall_lead12+ no.inter +0,
                 data=dat)
      
      crse20 = clx(fit20,1,dat$district)
      crse20
    })
    
    
    ###########################################
    #DiD for VWCV interim removed
    ###########################################
    
    print(21)
    try({
      dat = dat[(dat$date<min(interdate2))|(dat$date>=max(interdate2)),]
      fit21 = lm(VWCV~ district+ date +production+yield+
                   Per.Capita.GSDP+
                   amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                 +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                 +rainfall_lead10+rainfall_lead11+rainfall_lead12+ no.inter+0,
                 data=dat)
      
      crse21 = clx(fit21,1,dat$district)
      crse21
    })
    
    
    
    ###########################################
    #DiD for Range interim removed
    ###########################################
    print(22)
    try({
      dat = dat[(dat$date<min(interdate2))|(dat$date>=max(interdate2)),]
      fit22 = lm(R~ district+ date +
                   Per.Capita.GSDP+production+yield+
                   amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                 +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                 +rainfall_lead10+rainfall_lead11+rainfall_lead12+ no.inter+0 ,
                 data=dat)
      
      crse22 = clx(fit22,1,dat$district)
      crse22
    })

    
    dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/district level/",timemode,sep = ""))
    dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/district level/",timemode,"/",product,sep = ""))
    setwd(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/district level/",timemode,"/",product,sep = ""))

    write.csv(crse2,paste(product,"CV.csv"))
    write.csv(crse3,paste(product,"VWCV.csv"))
    write.csv(crse4,paste(product,"R.csv"))
    write.csv(crse20,paste(product,"CV drop interim.csv"))
    write.csv(crse21,paste(product,"VWCV drop interim.csv"))
    write.csv(crse22,paste(product,"R drop interim.csv"))

    a2 = stargazer(crse2)
    a3 = stargazer(crse3)
    a4 = stargazer(crse4)

    a20 = stargazer(crse20)
    a21 = stargazer(crse21)
    a22 = stargazer(crse22)

    write(a2,paste(product,"CV.txt"))
    write(a3,paste(product,"VWCV.txt"))
    write(a4,paste(product,"R.txt"))
    write(a20,paste(product,"CV drop interim.txt"))
    write(a21,paste(product,"VWCV drop interim.txt"))
    write(a22,paste(product,"R drop interim.txt"))


    re = rbind(re,crse2[nrow(crse2),])
    re = rbind(re,crse3[nrow(crse3),])
    re = rbind(re,crse4[nrow(crse4),])
    re = rbind(re,crse20[nrow(crse20),])
    re = rbind(re,crse21[nrow(crse21),])
    re = rbind(re,crse22[nrow(crse22),])

    
    colnames(re) = c("Estimate","Std.Error","t value","Pr(>|t|)")
    
    trial = c("CV","VWCV","R","CV drop interim","VWCV drop interim"
              ,"R drop interim")
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
  saveWorkbook(wbe,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/district level/",timemode," inter effects.xlsx",sep = ""),overwrite = T)
  saveWorkbook(wb,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/district level/",timemode," inter coefficients.xlsx",sep = ""),overwrite = T)
}
