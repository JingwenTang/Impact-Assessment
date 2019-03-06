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
library(jtools)
Timemode = c("13-17 week all") #the time unit and lags chosen for the model in synthetic control

for (tm in 2:2){
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
    re = matrix(0,90,4)
    ree=matrix(0,90,1)
    
    product = Product[p]

    
    dat1 = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/district features.xlsx",sheet = 1)
    for (j in 2:(ncol(dat1))){
      dat1[,j] = as.numeric(as.character(dat1[,j]))
    }
    #to get the features by fraction
    dat1[,2:9] = dat1[,2:9]/dat1[,31]
    dat1[,15:25] = dat1[,15:25]/dat1[,14]
    fea = dat1
    
    fea$agrl_cult_t = fea$agrl_t + fea$cult_t
    temp = c("market","catl_t","lroad","pop_ru","male_saw","agrl_cult_t")
    fea = fea[,colnames(fea)%in%temp]
    
    
    #market modal price monthly
    marketdat = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",timemode,"/",product," eleventh data.xlsx",sep = ""),sheet = 1)
    colnames(marketdat)[1] = "date"
    
    marketdat = join(marketdat, fea,by = "market", type = "left")
    #marketdat = na.omit(marketdat)
    
    temp = intervented%in%unique(marketdat$market)
    intervented1 = intervented[temp]
    interdate1 = interdate[temp]

    marketdat$inter = 0

    marketdat$date = as.Date(marketdat$date-25569,origin = "1970-01-01")
    for (i in 1:length(intervented1)){
      temp = marketdat[marketdat$market==intervented1[i],]
      marketdat[marketdat$market==intervented1[i],][temp$date>=(interdate1[i]+0),"inter"] = 1
          }
    market = unique(marketdat$market)
    
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
    
    
    marketdat1 = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",timemode,"/",product," eleventh data.xlsx",sep = ""),sheet = 1)
    colnames(marketdat1)[1] = "date"
    marketdat1 = join(marketdat1, fea,by = "market", type = "left")
    temp = intervented%in%unique(marketdat1$market)
    intervented1 = intervented[temp]
    interdate1 = interdate[temp]
    marketdat1$inter = 0
    marketdat1$date = as.Date(marketdat1$date-25569,origin = "1970-01-01")
    for (i in 1:length(intervented1)){
      temp = marketdat1[marketdat1$market==intervented1[i],]
      marketdat1[marketdat1$market==intervented1[i],][temp$date>=((interdate1 %m+% months(3))[i]+0),"inter"] = 1
      
      
    }
    market = unique(marketdat1$market)
    
    #calculate the average before treatment
    temp = marketdat1[(marketdat1$market%in%intervented)&(marketdat1$inter==0),]
    meanbefore = mean(temp$modal,na.rm = T)
    
    marketdat1$inter = as.factor(marketdat1$inter)
    
    
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
    
    crse1 = t(rep(0,4))
    crse2 = t(rep(0,4))
    crse3 = t(rep(0,4))
    crse4 = t(rep(0,4))
    crse5 = t(rep(0,4))
    
    
    ###########################################
    #DiD for modal prices(base)
    ###########################################
    print(1)
    try({
      d = marketdat
      d$date = as.factor(d$date)
      d = na.omit(d)
      
      fit1 = lm(log(modal)~ market+date+
                  annual+total_ar+lit_ru+ptmrkt+agrl_cult_t+pop_ru+male_saw+lroad+catl_t+
                  Per.Capita.GSDP+production+yield+
                  log(amount)+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                +rainfall_lead10+rainfall_lead11+rainfall_lead12+inter+0,
                data=d)
      l1 = lm.cluster(log(modal)~ market+date+
                        annual+total_ar+lit_ru+ptmrkt+agrl_cult_t+pop_ru+male_saw+lroad+catl_t+
                        Per.Capita.GSDP+production+yield+
                        log(amount)+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                      +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                      +rainfall_lead10+rainfall_lead11+rainfall_lead12+inter+0,
                      data=d,cluster = d$market)
      crse1 = clx(fit1,1, d$market)
      crse1
    })
    
    
    
    ###########################################
    #3 months lag
    ###########################################
    
    print(2)
    try({
      d = marketdat1
      d$date = as.factor(d$date)
      d = na.omit(d)
      
      fit2 = lm(log(modal)~ market+date+
                  annual+total_ar+lit_ru+ptmrkt+agrl_cult_t+pop_ru+male_saw+lroad+catl_t+
                  Per.Capita.GSDP+production+yield+
                  log(amount)+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                +rainfall_lead10+rainfall_lead11+rainfall_lead12+inter+0,
                data=d)
      l2 = lm.cluster(log(modal)~ market+date+
                        annual+total_ar+lit_ru+ptmrkt+agrl_cult_t+pop_ru+male_saw+lroad+catl_t+
                        Per.Capita.GSDP+production+yield+
                        log(amount)+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                      +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                      +rainfall_lead10+rainfall_lead11+rainfall_lead12+inter+0,
                      data=d,cluster = d$market)
      crse2 = clx(fit2,1, d$market)
      crse2
    })
    
    
    ###########################################
    #DiD for VWCV
    ###########################################
    
    print(3)
    try({
      d = marketdat
      d$date = as.factor(d$date)
      d = na.omit(d)
      
      fit3 = lm(log(modal)~ market+date+state+
                  annual+total_ar+lit_ru+ptmrkt+agrl_cult_t+pop_ru+male_saw+lroad+catl_t+
                  Per.Capita.GSDP+production+yield+
                  log(amount)+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                +rainfall_lead10+rainfall_lead11+rainfall_lead12+inter+0,
                data=d)
      l3 = lm.cluster(log(modal)~ market+date+state+
                        annual+total_ar+lit_ru+ptmrkt+agrl_cult_t+pop_ru+male_saw+lroad+catl_t+
                        Per.Capita.GSDP+production+yield+
                        log(amount)+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                      +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                      +rainfall_lead10+rainfall_lead11+rainfall_lead12+inter+0,
                      data=d,cluster = d$market)
      crse3 = clx(fit3,1, d$market)
      crse3
    })
    
    
    
    ###########################################
    #DiD for Range
    ###########################################
    print(4)
    try({
      d = marketdat
      d$month = month(d$date)
      d$month = as.factor(d$month)
      d$date = as.factor(d$date)
      d = na.omit(d)
      
      fit4 = lm(log(modal)~ market+date+month+
                  annual+total_ar+lit_ru+ptmrkt+agrl_cult_t+pop_ru+male_saw+lroad+catl_t+
                  Per.Capita.GSDP+production+yield+
                  log(amount)+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                +rainfall_lead10+rainfall_lead11+rainfall_lead12+inter+0,
                data=d)
      l4 = lm.cluster(log(modal)~ market+date+month+
                        annual+total_ar+lit_ru+ptmrkt+agrl_cult_t+pop_ru+male_saw+lroad+catl_t+
                        Per.Capita.GSDP+production+yield+
                        log(amount)+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                      +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                      +rainfall_lead10+rainfall_lead11+rainfall_lead12+inter+0,
                      data=d,cluster = d$market)
      crse4 = clx(fit4,1, d$market)
      crse4
    })
    
    
    ###########################################
    #DiD for modal prices parallel trend check
    ###########################################
    #because treated markets do not have enough data for period 2013-10 to 2013-12 so we remove this period
    #choose data before intervention
    print(5)
    try({
      d = marketdat
      d = d[d$date>=as.Date("2013-01-01"),]
      d$date = as.factor(d$date)
      d = na.omit(d)
      
      fit5 = lm(log(modal)~ market+date+
                  annual+total_ar+lit_ru+ptmrkt+agrl_cult_t+pop_ru+male_saw+lroad+catl_t+
                  Per.Capita.GSDP+production+yield+
                  log(amount)+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                +rainfall_lead10+rainfall_lead11+rainfall_lead12+inter+0,
                data=d)
      l5 = lm.cluster(log(modal)~ market+date+
                        annual+total_ar+lit_ru+ptmrkt+agrl_cult_t+pop_ru+male_saw+lroad+catl_t+
                        Per.Capita.GSDP+production+yield+
                        log(amount)+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
                      +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
                      +rainfall_lead10+rainfall_lead11+rainfall_lead12+inter+0,
                      data=d,cluster = d$market)
      crse5 = clx(fit5,1, d$market)
      crse5
    })
    dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/add vdsa/",timemode,sep = ""))
    dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/add vdsa/",timemode,"/",product,sep = ""))
    setwd(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/add vdsa/",timemode,"/",product,sep = ""))
    write.csv(crse1,paste(product,"main.csv"))
    write.csv(crse2,paste(product,"3 month lag.csv"))
    write.csv(crse3,paste(product,"state specific trends.csv"))
    write.csv(crse4,paste(product,"month fixed effects.csv"))
    write.csv(crse5,paste(product,"13-17.csv"))
    a1 = stargazer(crse1)
    a2 = stargazer(crse2)
    a3 = stargazer(crse3)
    a4 = stargazer(crse4)
    a5 = stargazer(crse5)
    write(a1,paste(product,"main.txt"))
    write(a2,paste(product,"3 month lag.txt"))
    write(a3,paste(product,"state specific trends.txt"))
    write(a4,paste(product,"month fixed effects.txt"))
    write(a5,paste(product,"13-17.txt"))
    try({re[1,] = crse1[rownames(crse1)=="inter1",]})
    try({re[2,] = crse1[rownames(crse1)=="log(amount)",]})
    try({re[3,] = crse1[rownames(crse1)=="yield",]})
    try({re[4,] = crse1[rownames(crse1)=="production",]})
    try({re[5,] = crse1[rownames(crse1)=="annual",]})
    try({re[6,] = crse1[rownames(crse1)=="total_ar",]})
    try({re[7,] = crse1[rownames(crse1)=="lit_ru",]})
    try({re[8,] = crse1[rownames(crse1)=="ptmrkt",]})
    try({re[9,] = crse1[rownames(crse1)=="agrl_cult_t",]})
    try({re[10,] = crse1[rownames(crse1)=="male_saw",]})
    try({re[11,] = crse1[rownames(crse1)=="pop_ru",]})
    try({re[12,] = crse1[rownames(crse1)=="lroad",]})
    try({re[13,] = crse1[rownames(crse1)=="catl_t",]})
    try({re[14,] = crse1[rownames(crse1)=="Per.Capita.GSDP",]})
    try({re[15,] = crse1[rownames(crse1)=="annual",]})
    try({re[16,] = crse1[rownames(crse1)=="rainfall",]})
    try({re[17,1] = nobs(fit1)})
    try({re[18,1] = summary(l1$lm_res)$r.squared})
    
    try({re[19,] = crse2[rownames(crse2)=="inter1",]})
    try({re[20,] = crse2[rownames(crse2)=="log(amount)",]})
    try({re[21,] = crse2[rownames(crse2)=="yield",]})
    try({re[22,] = crse2[rownames(crse2)=="production",]})
    try({re[23,] = crse2[rownames(crse2)=="annual",]})
    try({re[24,] = crse2[rownames(crse2)=="total_ar",]})
    try({re[25,] = crse2[rownames(crse2)=="lit_ru",]})
    try({re[26,] = crse2[rownames(crse2)=="ptmrkt",]})
    try({re[27,] = crse2[rownames(crse2)=="agrl_cult_t",]})
    try({re[28,] = crse2[rownames(crse2)=="male_saw",]})
    try({re[29,] = crse2[rownames(crse2)=="pop_ru",]})
    try({re[30,] = crse2[rownames(crse2)=="lroad",]})
    try({re[31,] = crse2[rownames(crse2)=="catl_t",]})
    try({re[32,] = crse2[rownames(crse2)=="Per.Capita.GSDP",]})
    try({re[33,] = crse2[rownames(crse2)=="annual",]})
    try({re[34,] = crse2[rownames(crse2)=="rainfall",]})
    try({re[35,1] = nobs(fit2)})
    try({re[36,1] = summary(l2$lm_res)$r.squared})
    
    try({re[37,] = crse3[rownames(crse3)=="inter1",]})
    try({re[38,] = crse3[rownames(crse3)=="log(amount)",]})
    try({re[39,] = crse3[rownames(crse3)=="yield",]})
    try({re[40,] = crse3[rownames(crse3)=="production",]})
    try({re[41,] = crse3[rownames(crse3)=="annual",]})
    try({re[42,] = crse3[rownames(crse3)=="total_ar",]})
    try({re[43,] = crse3[rownames(crse3)=="lit_ru",]})
    try({re[44,] = crse3[rownames(crse3)=="ptmrkt",]})
    try({re[45,] = crse3[rownames(crse3)=="agrl_cult_t",]})
    try({re[46,] = crse3[rownames(crse3)=="male_saw",]})
    try({re[47,] = crse3[rownames(crse3)=="pop_ru",]})
    try({re[48,] = crse3[rownames(crse3)=="lroad",]})
    try({re[49,] = crse3[rownames(crse3)=="catl_t",]})
    try({re[50,] = crse3[rownames(crse3)=="Per.Capita.GSDP",]})
    try({re[51,] = crse3[rownames(crse3)=="annual",]})
    try({re[52,] = crse3[rownames(crse3)=="rainfall",]})
    try({re[53,1] = nobs(fit3)})
    try({re[54,1] = summary(l3$lm_res)$r.squared})
    
    try({re[55,] = crse4[rownames(crse4)=="inter1",]})
    try({re[56,] = crse4[rownames(crse4)=="log(amount)",]})
    try({re[57,] = crse4[rownames(crse4)=="yield",]})
    try({re[58,] = crse4[rownames(crse4)=="production",]})
    try({re[59,] = crse4[rownames(crse4)=="annual",]})
    try({re[60,] = crse4[rownames(crse4)=="total_ar",]})
    try({re[61,] = crse4[rownames(crse4)=="lit_ru",]})
    try({re[62,] = crse4[rownames(crse4)=="ptmrkt",]})
    try({re[63,] = crse4[rownames(crse4)=="agrl_cult_t",]})
    try({re[64,] = crse4[rownames(crse4)=="male_saw",]})
    try({re[65,] = crse4[rownames(crse4)=="pop_ru",]})
    try({re[66,] = crse4[rownames(crse4)=="lroad",]})
    try({re[67,] = crse4[rownames(crse4)=="catl_t",]})
    try({re[68,] = crse4[rownames(crse4)=="Per.Capita.GSDP",]})
    try({re[69,] = crse4[rownames(crse4)=="annual",]})
    try({re[70,] = crse4[rownames(crse4)=="rainfall",]})
    try({re[71,1] = nobs(fit4)})
    try({re[72,1] = summary(l4$lm_res)$r.squared})
    
    
    try({re[73,] = crse5[rownames(crse5)=="inter1",]})
    try({re[74,] = crse5[rownames(crse5)=="log(amount)",]})
    try({re[75,] = crse5[rownames(crse5)=="yield",]})
    try({re[76,] = crse5[rownames(crse5)=="production",]})
    try({re[77,] = crse5[rownames(crse5)=="annual",]})
    try({re[78,] = crse5[rownames(crse5)=="total_ar",]})
    try({re[79,] = crse5[rownames(crse5)=="lit_ru",]})
    try({re[80,] = crse5[rownames(crse5)=="ptmrkt",]})
    try({re[81,] = crse5[rownames(crse5)=="agrl_cult_t",]})
    try({re[82,] = crse5[rownames(crse5)=="male_saw",]})
    try({re[83,] = crse5[rownames(crse5)=="pop_ru",]})
    try({re[84,] = crse5[rownames(crse5)=="lroad",]})
    try({re[85,] = crse5[rownames(crse5)=="catl_t",]})
    try({re[86,] = crse5[rownames(crse5)=="Per.Capita.GSDP",]})
    try({re[87,] = crse5[rownames(crse5)=="annual",]})
    try({re[88,] = crse5[rownames(crse5)=="rainfall",]})
    try({re[89,1] = nobs(fit5)})
    try({re[90,1] = summary(l5$lm_res)$r.squared})
    
    colnames(re) = c("Estimate","Std.Error","t value","Pr(>|t|)")
    
    trial = rep(c("inter1","log(amount)","yield","production","annual","total_ar","lit_ru",
                  "ptmrkt","agrl_cult_t","male_saw","pop_ru","lroad","catl_t","Per.Capita.GSDP","annual","rainfall",
                  "no.observations","R-squared"),5)
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
  
  saveWorkbook(wbe,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/add vdsa/",timemode," inter effects.xlsx",sep = ""),overwrite = T)
  saveWorkbook(wb,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/add vdsa/",timemode," inter coefficients.xlsx",sep = ""),overwrite = T)
  
}
