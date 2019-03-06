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
Timemode = c("10-17 week","10-17 week all","13-17 month common types","13-17 month all common types",
             "13-17 week","13-17 week all","13-17 week common types","13-17 week all common types",
             "13-17 month without vdsa features","13-17 month all without vdsa features") #the time unit and lags chosen for the model in synthetic control
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
  
  
  
  for (p in 1:length(Product)){
    re = matrix(0,2,4)
    ree=matrix(0,45,1)
    
    product = Product[p]
    dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/DiD/",fmode,"/",product,sep = ""))
    
    features = Features[[p]]
    
    predictors = features
    
    
    
    
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
    thresh1 = sum(aa$amount)*0.25
    thresh2 = sum(aa$amount)*0.5
    thresh3 = sum(aa$amount)*0.75
    for (l in 1:nrow(aa)){
      aaa = sum(aa$amount[1:l])
      if (aaa<=thresh1) l1 = l
      if (aaa<=thresh2) l2 = l
      if (aaa<=thresh3) l3 = l
    }
    large = aa$market[1:l2]

    small = aa$market[l2:nrow(aa)]
    marketdat1$size = "small"
    

    small = small[!small%in%large]
    
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
    print(25)
    try({
      d = marketdat1
      d$date = as.factor(d$date)
      d = d[!is.na(d$Per.Capita.GSDP),]
      d = d[!is.na(d$production),]
      d = d[!is.na(d$yield),]
      d = d[!is.na(d$amount),]
      d = d[!is.na(d$inter),]
      
      d = na.omit(d)
      
      d <- within(d, inter <- relevel(inter, ref = "1"))
      fit25 = lm(log(modal)~ market+date+
                   Per.Capita.GSDP+production+yield+annual+total_ar+lit_ru+ptmrkt+agrl_t+
                   log(amount)+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                 +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                 +rainfall_lead10+rainfall_lead11+rainfall_lead12+size:inter+0,
                 data=d)
      
      crse25 = clx(fit25,1, d$market)
      crse25
    })
    
    
    
    dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/",timemode,sep = ""))
    dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/",timemode,"/",product,sep = ""))
    setwd(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/",timemode,"/",product,sep = ""))
    write.csv(crse25,paste(product,"modal add size.csv"))
    
    a25 = stargazer(crse25)
        
    write(a25,paste(product,"modal add size.txt"))


    try({re[1,] = crse25[rownames(crse25)=="sizelarge:inter1",]})

    try({re[2,] = crse25[rownames(crse25)=="sizesmall:inter1",]})

    
    colnames(re) = c("Estimate","Std.Error","t value","Pr(>|t|)")
    
    trial = c("modal add size_large","modal add size_medium_large","modal add size_medium_small","modal add size_small")
    re = cbind(trial = trial,re)
    
    
    #calculate the effect of the coefficient by dividing by mean before treatment
    re = as.data.frame(re)

    
    addWorksheet(wb,product)

    
    writeData(wb,sheet = product,re)

  }
  saveWorkbook(wb,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/",timemode," large_small removing one level.xlsx",sep = ""),overwrite = T)
}
