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
for (tm in 1:1){
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
      meanbefore = mean(marketdat$modal,na.rm = T)
      
      dat = marketdat
      temp = intervented%in%unique(dat$market)
      intervented2 = intervented[temp]
      interdate2 = interdate[temp]
      #dat$date = as.Date(dat$date-25569,origin = "1970-01-01")
      
      
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
      
      
      
      for (l in c(6:11,16)){
        name = colnames(dat)[l]
        colnames(dat)[l] = "target"
        newdat = as.data.frame(ddply(dat,.(date,state),summarise, wmean = weighted.mean(target,amount)))
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
      
      Dat = Dat[Dat$date<min(interdate2)|Dat$date>max(interdate2),]
      Dat$inter = 0
      Dat$inter[(Dat$date>max(interdate2))&(Dat$state=="karnataka")] = 1
      
      Dat$date = as.factor(Dat$date)
      Dat$state = as.factor(Dat$state)
      Dat$inter = as.factor(Dat$inter)
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
      #DiD for CV
      ###########################################
      
      print(2)
      fit2 = lm(CV~ state+ date + annual+total_ar+lit_ru+ptmrkt+agrl_t +
                  Gross.State.Domestic.Product+
                  amount+ inter+0,
                data=dat)
      
      crse2 = clx(fit2,1,dat$state)
      crse2
      
      ###########################################
      #DiD for VWCV
      ###########################################
      
      print(3)
      fit3 = lm(VWCV~ state+ date + annual+total_ar+lit_ru+ptmrkt+agrl_t +
                  Gross.State.Domestic.Product+
                  amount+ inter+0,
                data=dat)
      
      crse3 = clx(fit3,1,dat$state)
      crse3
      
      
      ###########################################
      #DiD for Range
      ###########################################
      print(4)
      fit4 = lm(R~ state+ date + annual+total_ar+lit_ru+ptmrkt+agrl_t+
                  Gross.State.Domestic.Product+
                  amount+ inter+0 ,
                data=dat)
      
      crse4 = clx(fit4,1,dat$state)
      crse4
      
      
      
      
      dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/DiD/CV/",timemode,sep = ""))
      dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/DiD/CV/",timemode,"/",product,sep = ""))
      setwd(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/DiD/CV/",timemode,"/",product,sep = ""))
      
      write.csv(crse2,paste(product,"CV monthly.csv"))
      write.csv(crse3,paste(product,"VWCV monthly.csv"))
      write.csv(crse4,paste(product,"R monthly.csv"))
      
      
      
      a2 = stargazer(crse2)
      a3 = stargazer(crse3)
      a4 = stargazer(crse4)
      
      write(a2,paste(product,"CV monthly.txt"))
      write(a3,paste(product,"VWCV monthly.txt"))
      write(a4,paste(product,"R monthly.txt"))
      
      
     
      re = rbind(re,crse2[nrow(crse2),])
      re = rbind(re,crse3[nrow(crse3),])
      re = rbind(re,crse4[nrow(crse4),])
      
      
      
      colnames(re) = c("Estimate","Std.Error","t value","Pr(>|t|)")
      
      trial = c("CV","VWCV","R")
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
  saveWorkbook(wbe,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/DiD/CV/",timemode," inter effects monthly dispersion.xlsx",sep = ""),overwrite = T)
  saveWorkbook(wb,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/DiD/CV/",timemode," inter coefficients monthly dispersion.xlsx",sep = ""),overwrite = T)
}


##combine all commodities
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
for (tm in 1:1){
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
  
  allprod = c()
  
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
      meanbefore = mean(marketdat$modal,na.rm = T)
      
      dat = marketdat
      temp = intervented%in%unique(dat$market)
      intervented2 = intervented[temp]
      interdate2 = interdate[temp]
      #dat$date = as.Date(dat$date-25569,origin = "1970-01-01")
      
      
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
      
      
      
      for (l in c(6:11,16)){
        name = colnames(dat)[l]
        colnames(dat)[l] = "target"
        newdat = as.data.frame(ddply(dat,.(date,state),summarise, wmean = weighted.mean(target,amount)))
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
      
      
      dat = Dat
      dat$prod = product
      allprod = rbind(allprod,dat)
    })
  } 
      
      
      
      
      
      
      
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
      
      
      
      
      
      
      dat = allprod
      
      ###########################################
      #DiD for CV
      ###########################################
      
      print(2)
      fit2 = lm(CV~ state+ prod + date + annual+total_ar+lit_ru+ptmrkt+agrl_t +
                  Gross.State.Domestic.Product+
                  amount+ no.inter+0,
                data=dat)
      
      crse2 = mclx(fit2,1,dat$state,dat$prod)
      crse2
      
      ###########################################
      #DiD for VWCV
      ###########################################
      
      print(3)
      fit3 = lm(VWCV~ state+ prod + date + annual+total_ar+lit_ru+ptmrkt+agrl_t +
                  Gross.State.Domestic.Product+
                  amount+ no.inter+0,
                data=dat)
      
      crse3 = mclx(fit3,1,dat$state,dat$prod)
      crse3
      
      
      ###########################################
      #DiD for Range
      ###########################################
      print(4)
      fit4 = lm(R~ state+ prod + date + annual+total_ar+lit_ru+ptmrkt+agrl_t+
                  Gross.State.Domestic.Product+
                  amount+ no.inter+0 ,
                data=dat)
      
      crse4 = mclx(fit4,1,dat$state,dat$prod)
      crse4
      
      
      
      
      dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/DiD/CV/",timemode,sep = ""))
      dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/DiD/CV/",timemode,"/all commodities",sep = ""))
      setwd(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/DiD/CV/",timemode,"/all commodities",sep = ""))
      
      write.csv(crse2,paste(product,"CV monthly.csv"))
      write.csv(crse3,paste(product,"VWCV monthly.csv"))
      write.csv(crse4,paste(product,"R monthly.csv"))
      
      
      
      a2 = stargazer(crse2)
      a3 = stargazer(crse3)
      a4 = stargazer(crse4)
      
      write(a2,paste(product,"CV monthly.txt"))
      write(a3,paste(product,"VWCV monthly.txt"))
      write(a4,paste(product,"R monthly.txt"))
      
      
      
      re = rbind(re,crse2[nrow(crse2),])
      re = rbind(re,crse3[nrow(crse3),])
      re = rbind(re,crse4[nrow(crse4),])
      
      
      
      colnames(re) = c("Estimate","Std.Error","t value","Pr(>|t|)")
      
      trial = c("CV","VWCV","R")
      re = cbind(trial = trial,re)
      
      #calculate the effect of the coefficient by dividing by mean before treatment
      re = as.data.frame(re)
      effect = as.numeric(as.numeric(as.character(re$Estimate))/meanbefore)
      ree = cbind(trial = trial,effect = effect)
      
      wb <- loadWorkbook(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/DiD/CV/",timemode," inter coefficients monthly dispersion.xlsx",sep = ""))
      addWorksheet(wb, "all commodities")
      writeData(wb,sheet = "all commodities",re)
      saveWorkbook(wb,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/DiD/CV/",timemode," inter coefficients monthly dispersion.xlsx",sep = ""),overwrite = T)
      wbe <- loadWorkbook(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/DiD/CV/",timemode," inter effects monthly dispersion.xlsx",sep = ""))
      addWorksheet(wbe, "all commodities")
      writeData(wbe,sheet = "all commodities",ree)
      saveWorkbook(wbe,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/DiD/CV/",timemode," inter effects monthly dispersion.xlsx",sep = ""),overwrite = T)
      
      
      
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
