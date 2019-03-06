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
library(plyr)
library(ROCR)
library(stargazer)
library(miceadds)
library(multiwayvcov)
library(radiant.data)
library(stringi)
Timemode = c("13-17 month","13-17 month all","13-17 month common types","13-17 month all common types",
             "13-17 week","13-17 week all","13-17 week common types","13-17 week all common types") #the time unit and lags chosen for the model in synthetic control

Br = c("month","month","month","month","week","week","week","week") 
for (tm in c(3,5,7)){
  br=Br[tm]
  timemode = Timemode[tm]
  wb = createWorkbook() 
  wbe = createWorkbook()
  #f[[2]] = c("annual","total_ar","lit_t","male_saw")
  fmode = "CV"
  f = c("annual","total_ar","lit_t","male_saw","annmai","lit_ru","ptmrkt","lroad","agrl_t")
  
  
  
  dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/DiD/",fmode,sep = ""))
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
  

  
  for (p in 1:length(Product)){
    try({
    re = c()
    ree=c()
    product = Product[p]
    dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/DiD/",fmode,"/",product,sep = ""))
    
    features = Features[[p]]
    
    predictors = features
    
    
   
    
    #market modal price weekly
    marketdat = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/",product,"/",timemode,"/seventh data.xlsx",sep = ""),sheet = 1)
    colnames(marketdat)[1] = "date"
    marketdat = marketdat[marketdat$market!="virtual",]
    temp = intervented%in%unique(marketdat$market)
    intervented1 = intervented[temp]
    interdate1 = interdate[temp]
    marketdat$inter = 0
    marketdat$date = as.Date(marketdat$date-25569,origin = "1970-01-01")
    for (i in 1:length(intervented1)){
      temp = marketdat[marketdat$market==intervented1[i],]
      marketdat[marketdat$market==intervented1[i],][temp$date>(interdate1[i]+0),"inter"] = 1
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
      temp[(l+1):nrow(temp),"active"] = FALSE
      newd = rbind(newd,temp)
    }
    marketdat = newd
    marketdat$inter = as.factor(marketdat$inter)
    
    
    #add state and state features for marketdat
    ms = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/market_districts.xlsx",sheet=1)
    colnames(ms)[1:4] = c("market","dis1","dis2","state")
    ms$market = tolower(ms$market)
    ms$state = tolower(ms$state)
    ms$dis2 = tolower(ms$dis2)
    MS = ms
    ms = ms[,c(1,4)]
    
    d = marketdat
    d = join(d,ms,by = "market",type = "left")
    d = na.omit(d)
    for (l in 1:nrow(d)){
      if (d[l,"state"]=="india"){
        d[l,"state"] = MS[MS$market==d[l,"market"],3]
      }
      if (d[l,"state"]%in%c("hagaribommanahalli","hubballi")){
        d[l,"state"] = "karnataka"
      }
      if (d[l,"state"]%in%c("bhildi","jagdish nagar")){
        d[l,"state"] = "gujarat"
      }
      if (d[l,"state"]=="jadcherla"){
        d[l,"state"] = "telangana"
      }
      if (d[l,"state"]=="purani basti"){
        d[l,"state"] = "rajasthan"
      }
      if (d[l,"state"]%in%c("sampath nagar","ramanujapuram")){
        d[l,"state"] = "tamil nadu"
      }
      if (d[l,"state"]=="risia bazaar"){
        d[l,"state"] = "uttar pradesh"
      }
      
    }
    sf = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/state features/full state features.xlsx",sheet = 1)
    d$year = year(d$date)
    newd = join(d,sf,by = c("state","year"),type = "left")
    newd = as.data.frame(newd)
    for (l in 16:ncol(newd)){
      newd[,l] = as.numeric(newd[,l])
    }
    marketdat = newd
    
    marketdat = na.omit(marketdat)
    
    #calculate the average before treatment
    temp = marketdat[(marketdat$market%in%intervented)&(marketdat$inter==0),]
    meanbefore = mean(temp$modal,na.rm = T)
    
    #12-17 mon-qua marketwise daily
    dat = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/",product,"/",timemode,"/fifth data.xlsx",sep = ""),sheet = 1)
    dat = dat[dat$market!="virtual",]
    dat = join(dat,ms,by = "market",type = "left")
    dat = na.omit(dat)
    for (l in 1:nrow(dat)){
      if (dat[l,"state"]=="india"){
        dat[l,"state"] = MS[MS$market==dat[l,"market"],3]
      }
      if (dat[l,"state"]%in%c("hagaribommanahalli","hubballi")){
        dat[l,"state"] = "karnataka"
      }
      if (dat[l,"state"]%in%c("bhildi","jagdish nagar")){
        dat[l,"state"] = "gujarat"
      }
      if (dat[l,"state"]=="jadcherla"){
        dat[l,"state"] = "telangana"
      }
      if (dat[l,"state"]=="purani basti"){
        dat[l,"state"] = "rajasthan"
      }
      if (dat[l,"state"]%in%c("sampath nagar","ramanujapuram")){
        dat[l,"state"] = "tamil nadu"
      }
      if (dat[l,"state"]=="risia bazaar"){
        dat[l,"state"] = "uttar pradesh"
      }
      
    }
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
    
    
    
    for (l in 6:(ncol(dat)-1)){
      name = colnames(dat)[l]
      colnames(dat)[l] = "target"
      newdat = as.data.frame(ddply(dat,.(date,state),summarise, wmean = weighted.mean(target,amount)))
      colnames(newdat) = c("date","state",name)
      Dat = join(Dat,newdat,by = c("date","state"),type = "left")
      colnames(dat)[l] = name
    }
    
    dat = Dat
    
    
    
    
    #start to break by week
    
    
    dat$time = as.Date(cut(dat$date,br = br))
    Dat = aggregate(dat$amount,by = list(dat$time,dat$state),mean,na.rm = T)
    colnames(Dat) = c("date","state","amount")
    for (l in c(3:11,13:(ncol(dat)-1))){
      name = colnames(dat)[l]
      colnames(dat)[l] = "target"
      newdat = as.data.frame(ddply(dat,.(time,state),summarise, wmean = weighted.mean(target,amount)))
      colnames(newdat) = c("date","state",name)
      Dat = join(Dat,newdat,by = c("date","state"),type = "left")
      colnames(dat)[l] = name
    }
    notr$date = as.Date(cut(notr$date,br = br))
    notr1 = aggregate(notr$no.inter,by = list(notr$date),max)
    colnames(notr1) = c("date","no.inter")
    notr2 = aggregate(notr$volume_fraction,by = list(notr$date),max)
    colnames(notr2) = c("date","volume_fraction")
    notr = join(notr1,notr2,by = "date",type = "left")
    Dat_1 = Dat[Dat$state=="karnataka",]
    Dat_1 = join(Dat_1,notr,by = "date",type = "left")
    Dat_2 = Dat[Dat$state!="karnataka",]
    Dat_2$no.inter = 0
    Dat_2$volume_fraction = 0
    Dat = rbind(Dat_1,Dat_2)
    Dat$date = as.factor(Dat$date)
    Dat$state = as.factor(Dat$state)
    for (l in 3:ncol(Dat)){
      Dat[,l] = as.numeric(Dat[,l])
    }
    state = unique(Dat$state)
    newd = c()
    for (i in 1:length(unique(Dat$state))){
      temp = Dat[Dat$state==state[i],]
      temp = temp[order(temp$amount,decreasing = T),]
      thres = sum(temp$amount,na.rm = T)*0.9
      for (l in 1:nrow(temp)){
        if (sum(temp$amount[1:l],na.rm = T)>thres) break
      }
      temp[1:l,"active"] = TRUE
      temp[(l+1):nrow(temp),"active"] = FALSE
      newd = rbind(newd,temp)
    }
    Dat = newd
    Dat$active = as.factor(Dat$active)  
    
    Dat$year = year(Dat$date)
    Dat = join(Dat,sf,by = c("state","year"),type = "left")
    Dat = na.omit(Dat)
    dat = Dat
    
    
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
    
    
    
    
    
    ###########################################
    #DiD for modal prices(base)
    ###########################################
    print(1)
    d = marketdat
    d$date = as.factor(d$date)
    d = na.omit(d)
    fit1 = lm(modal~ market+date+ annual+total_ar+lit_ru+ptmrkt+agrl_t +
                Gross.State.Domestic.Product+
                amount+ inter+0,
              data=d)
    
    crse1 = clx(fit1,1, d$market)
    crse1
    

    ###########################################
    #DiD for CV
    ###########################################
    
    print(2)
    fit2 = lm(CV~ state+ date + annual+total_ar+lit_ru+ptmrkt+agrl_t +
                Gross.State.Domestic.Product+
                amount+ volume_fraction+0,
              data=dat)
    
    crse2 = clx(fit2,1,dat$state)
    crse2
    
    ###########################################
    #DiD for VWCV
    ###########################################
    
    print(3)
    fit3 = lm(VWCV~ state+ date + annual+total_ar+lit_ru+ptmrkt+agrl_t +
                Gross.State.Domestic.Product+
                amount+ volume_fraction+0,
              data=dat)
    
    crse3 = clx(fit3,1,dat$state)
    crse3
    
    
    ###########################################
    #DiD for Range
    ###########################################
    print(4)
    fit4 = lm(R~ state+ date + annual+total_ar+lit_ru+ptmrkt+agrl_t+
                Gross.State.Domestic.Product+
                amount+ volume_fraction+0 ,
              data=dat)
    
    crse4 = clx(fit4,1,dat$state)
    crse4
    
    ###########################################
    #DiD for modal prices parallel trend check
    ###########################################
    #because treated markets do not have enough data for period 2013-10 to 2013-12 so we remove this period
    #choose data before intervention
    print(5)
    d = marketdat[marketdat$date<min(interdate),]
    d$treat = d$market%in%intervented
    d = d[order(d$date),]
    d$time = as.numeric(as.factor((d$date)))
    d$date = as.factor(d$date)
    fit5 = lm(modal~ annual+total_ar+lit_ru+ptmrkt+agrl_t+
                Gross.State.Domestic.Product+
                amount+market+time  + date + treat:time+0 ,
              data=d)
    crse5 = clx(fit5,1, d$market)
    crse5
    
    
    ###########################################
    #DiD for modal prices remove interim
    ###########################################
    print(6)
    d = marketdat[marketdat$date<min(interdate1)|marketdat$date>max(interdate1),]
    d$treat = d$market%in%intervented
    d = d[order(d$date),]
    d$time = as.numeric(as.factor((d$date)))
    d$date = as.factor(d$date)
    fit6 = lm(modal~ annual+total_ar+lit_ru+ptmrkt+agrl_t+
                Gross.State.Domestic.Product+
                amount+market + date +inter+0 ,
              data=d)
    crse6 = clx(fit6,1, d$market)
    crse6
    
    ###########################################
    #DiD for modal prices placebo
    ###########################################
    print(7)
    d = marketdat[marketdat$date<min(interdate1),]
    set.seed(80)
    ind = sample(x = d$date,size = 1)
    d1 = d[d$date>ind,]
    d2 = d[d$date<=ind,]
    d1[d1$market%in%intervented,"inter"] = 1
    d = rbind(d1,d2)
    d$date = as.factor(d$date)
    fit7 = lm(modal~ annual+total_ar+lit_ru+ptmrkt+agrl_t+
                Gross.State.Domestic.Product+
                amount+market + date +inter+0 ,
              data=d)
    crse7 = clx(fit7,1, d$market)
    crse7
    
    
    ###########################################
    #DiD for modal prices without features
    ###########################################
    print(8)
    d = marketdat
    d$date = as.factor(d$date)
    fit8 = lm(modal~ market + date +inter +0,
              data=d)
    crse8 = clx(fit8,1, d$market)
    crse8
    
    
    ###########################################
    #DiD for modal prices within karnataka
    ###########################################
    #here volume fraction only depend on time and state
    #from the paper table 3 first column
    print(9)
    d = marketdat
    d = d[d$state=="karnataka",]
    d$date = as.factor(d$date)
    d = na.omit(d)
    fit9 = lm(modal~ market+date+ annual+total_ar+lit_ru+ptmrkt+agrl_t +
                Gross.State.Domestic.Product+
                amount+inter+0,
              data=d)
    
    crse9 = clx(fit9,1, d$market)
    crse9
    
    
    ###########################################
    #DiD for modal prices state level
    ###########################################
    #here volume_fraction depend on time and state
    print(10)
    fit10 = lm(wmean~ state+ date + annual+total_ar+lit_ru+ptmrkt+agrl_t+
                 Gross.State.Domestic.Product+
                 amount+ volume_fraction +0,
               data=dat)
    
    crse10 = clx(fit10,1, dat$state)
    crse10
    
    ###########################################
    #DiD for modal prices state fixed
    ###########################################
    #here volume_fraction depend on time and state
    print(11)
    d = marketdat
    
    d_1 = d[d$state=="karnataka",]
    d_1 = join(d_1,notr,by = "date",type = "left")
    d_2 = d[d$state!="karnataka",]
    d_2$no.inter = 0
    d_2$volume_fraction = 0
    d = rbind(d_1,d_2)
    d$date = as.factor(d$date)
    fit11 = lm(modal~ state+ date + annual+total_ar+lit_ru+ptmrkt+agrl_t+
                 Gross.State.Domestic.Product+
                 amount+ volume_fraction +0,
               data=d)
    
    crse11 = clx(fit11,1,d$state)
    crse11
    
    
    ###########################################
    #propensity score weighting (by logit model)
    ###########################################
    print(12)
    wei = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/propensity score weighting/weight/",product,timemode,"weight.xlsx"),sheet = 1)
    d = marketdat
    d$date = as.factor(d$date)
    d = na.omit(d)
    d = join(d,wei,by = "market",type = "left")
    fit12 = lm(modal~ market+date + annual+total_ar+lit_ru+ptmrkt+agrl_t
               +Gross.State.Domestic.Product+amount + inter +0,
               data=d,weights = w)
    
    crse12 = clx(fit12,1, d$market)
    crse12
    
    ###########################################
    #propensity score weighting (by probit)
    ###########################################
    print(13)
    wei = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/propensity score weighting/weight/",product,timemode,"weight probit.xlsx"),sheet = 1)
    d = marketdat
    d$date = as.factor(d$date)
    d = na.omit(d)
    d = join(d,wei,by = "market",type = "left")
    fit13 = lm(modal~ market+date + annual+total_ar+lit_ru+ptmrkt+agrl_t
               +Gross.State.Domestic.Product+amount+inter+0,
               data=d,weights = w)
    
    crse13 = clx(fit13,1, d$market)
    crse13
    
    ###########################################
    #modal add month/week
    ###########################################
    print(14)
    d = marketdat
    d$month = month(d$date)
    
    d$week = stri_datetime_fields(d$date)$WeekOfMonth
    d$week = as.factor(d$week)
    d$month = as.factor(d$month)
    d$date = as.factor(d$date)
    d = na.omit(d)
    if (br=="week"){
      fit14 = lm(modal~ market+week + month + date+ annual+total_ar+lit_ru+ptmrkt+agrl_t +
                   Gross.State.Domestic.Product+amount+inter+0 ,
                 data=d)
      
      crse14 = clx(fit14,1, d$market)
      crse14
    }
    if (br=="month"){
      fit14 = lm(modal~ market + month + date+ annual+total_ar+lit_ru+ptmrkt+agrl_t +
                   Gross.State.Domestic.Product+amount+inter+0 ,
                 data=d)
      
      crse14 = clx(fit14,1, d$market)
      crse14
    }
    
    ###########################################
    #modal state specific trend
    ###########################################
    print(15)
    d = marketdat
    d$date = as.factor(d$date)
    d$timenumber = as.numeric(d$date)
    d = na.omit(d)
    fit15 = lm(modal~ state + state:timenumber +market+date+ annual+total_ar+lit_ru+ptmrkt+agrl_t +
                Gross.State.Domestic.Product+
                amount+ inter+0,
              data=d)
    
    crse15 = clx(fit15,1, d$market)
    crse15
    
    ###########################################
    #modal market specific trend
    ###########################################
    print(16)
    d = marketdat
    d$date = as.factor(d$date)
    d$timenumber = as.numeric(d$date)
    d = na.omit(d)
    fit16 = lm(modal~ market + market:timenumber +date+ annual+total_ar+lit_ru+ptmrkt+agrl_t +
                 Gross.State.Domestic.Product+
                 amount+ inter+0,
               data=d)
    
    crse16 = clx(fit16,1, d$market)
    crse16
    
    ###########################################
    #DiD for modal prices state level  num
    ###########################################
    #here volume_fraction depend on time and state
    print(17)
    fit17 = lm(wmean~ state+ date + annual+total_ar+lit_ru+ptmrkt+agrl_t+
                 Gross.State.Domestic.Product+
                 amount+ no.inter +0,
               data=dat)
    
    crse17 = clx(fit17,1, dat$state)
    crse17
    
    ###########################################
    #DiD for modal prices state fixed  num
    ###########################################
    #here volume_fraction depend on time and state
    print(18)
    d = marketdat
    
    d_1 = d[d$state=="karnataka",]
    d_1 = join(d_1,notr,by = "date",type = "left")
    d_2 = d[d$state!="karnataka",]
    d_2$no.inter = 0
    d_2$volume_fraction = 0
    d = rbind(d_1,d_2)
    d$date = as.factor(d$date)
    fit18 = lm(modal~ state+ date + annual+total_ar+lit_ru+ptmrkt+agrl_t+
                 Gross.State.Domestic.Product+
                 amount+ no.inter +0,
               data=d)
    
    crse18 = clx(fit18,1,d$state)
    crse18
    
    
    dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/DiD/CV/",timemode,sep = ""))
    dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/DiD/CV/",timemode,"/",product,sep = ""))
    setwd(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/DiD/CV/",timemode,"/",product,sep = ""))
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
  
  
  colnames(re) = c("Estimate","Std.Error","t value","Pr(>|t|)")
  
  trial = c("modal","CV","VWCV","R","modal parallel","modal drop interim","modal placebo","modal without features",
            "modal within karnataka","modal state level","modal state fixed","modal psw logit","modal psw probit",
            "modal day info","modal state specific trend","modal market specific trend","modal state level num",
            "modal state fixed num")
  re = cbind(trial = trial,re)
 
  #calculate the effect of the coefficient by dividing by mean before treatment
  re = as.data.frame(re)
  effect = as.numeric(as.numeric(as.character(re$Estimate))/meanbefore)
  ree = cbind(trial = trial,effect = effect)
  
  addWorksheet(wb,product)
  addWorksheet(wbe,product)
  
  writeData(wb,sheet = product,re)
  writeData(wbe,sheet = product,ree)
    })
  
  }
  saveWorkbook(wbe,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/DiD/CV/",timemode," inter effects.xlsx",sep = ""),overwrite = T)
    saveWorkbook(wb,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/DiD/CV/",timemode," inter coefficients.xlsx",sep = ""),overwrite = T)
}









####insert to 233 row
dat1$treat = dat1$market%in%intervented
dat1$treat = as.factor(dat1$treat)
for (j in 2:(ncol(dat1)-1)){
  print(colnames(dat1)[j])
  fit01 = lm(dat1[,j]~dat1$treat)
  print(summary(fit01))
}
logit = glm(treat~annual + total_ar + lit_t + agrl_t + ptmrkt,data = dat1,family = "binomial")
probability = logit$fitted.values
dat1$treat = as.numeric(dat1$treat)

dat1$w = dat1$treat*probability + (1-dat1$treat)*(1-probability)
temp = (dat1$w>0.1)
dat1 = dat1[temp,]
dat1_ = dat1
for (j in 2:(ncol(dat1)-2)){
  dat1[,j] = as.numeric(dat1[,j])
  dat1[,j] = dat1[,j]/dat1$w
}
dat1$treat = as.factor(dat1$treat)
for (j in 2:(ncol(dat1)-2)){
  print(colnames(dat1)[j])
  fit01 = lm(dat1[,j]~dat1$treat)
  print(summary(fit01))
}


##      
mod1b <- miceadds::lm.cluster( data=dat, formula=modal~ annual+total_ar+lit_ru+ptmrkt+agrl_t+Population+ inter + market+ date ,
                               cluster=list(dat$market,dat$date))
print(summary(mod1b))



###
for (j in 2:(ncol(dat1)-1)){
  print(colnames(dat1)[j])
  fit01 = lm(dat1[,j]~dat1$treat)
  print(summary(fit01))
}
logit = glm(treat~annual+total_ar+lit_t+agrl_t+ptmrkt,data = dat1,family = "binomial")
probability = predict(logit,type = "response")
#library(gbm)
#gps = gbm(treat~annual+total_ar+lit_t+agrl_t+ptmrkt+annmai+lit_ru+male_saw+lroad,distribution = "bernoulli",
#data = dat1,n.trees = 100,interaction.depth = 4,train.fraction = 0.8,shrinkage = 0.0005)
#probability = c()
#for (l in 1:length(gps$fit)){
#probability[l] = exp(gps$fit[l])/(1+exp(gps$fit[l]))
#}
dat1$treat = as.numeric(dat1$treat)

dat1$w = dat1$treat*probability + (1-dat1$treat)*(1-probability)
temp = (dat1$w>0.1)
dat1 = dat1[temp,]
dat1_ = dat1
for (j in 2:(ncol(dat1)-2)){
  dat1[,j] = as.numeric(dat1[,j])
  dat1[,j] = dat1[,j]/dat1$w
}
dat1$treat = as.logical(dat1$treat)
for (j in 2:(ncol(dat1)-2)){
  print(colnames(dat1)[j])
  fit01 = lm(dat1[,j]~dat1$treat)
  print(summary(fit01))
}
##########3
mod1b <- miceadds::lm.cluster( data=d, formula=modal~ market+date + annual+total_ar+lit_ru+ptmrkt+agrl_t+ inter +
                                 Gross.State.Domestic.Product+
                                 amount,
                               cluster=d$market)
print(summary(mod1b))
