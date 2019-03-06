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

for (tm in c(2)){
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

  
  ci = matrix(0,14,4)
  ci2 = matrix(0,14,4)
  
  for (p in 1:length(Product)){
    re = matrix(0,45,4)
    ree=matrix(0,45,1)
    
    product = Product[p]
    dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/DiD/",fmode,"/",product,sep = ""))
    
    features = Features[[p]]
    
    predictors = features
    
    
    
    
    #market modal price monthly
    marketdat = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",timemode,"/",product," second data.xlsx",sep = ""),sheet = 1)
    marketdat = na.omit(marketdat)
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
    
    marketdat$date = cut(marketdat$date, breaks = "week")
    newmarketdat = ddply(marketdat,.(date,market),summarise, modal = weighted.mean(modal,amount))
    marketdat = newmarketdat
    marketdat$inter = 0
    marketdat$date = as.Date(marketdat$date)
    for (i in 1:length(intervented1)){
      temp = marketdat[marketdat$market==intervented1[i],]
      marketdat[marketdat$market==intervented1[i],][temp$date>=(interdate1[i]+0),"inter"] = 1
         
    }
    market = unique(marketdat$market)
    
    #calculate the average before treatment
    
    marketdat$inter = as.factor(marketdat$inter)
    clx <-function(fm, dfcw, cluster){
      library(sandwich)
      library(lmtest)
      M <- length(unique(cluster))
      N <- length(cluster)
      dfc <- (M/(M-1))*((N-1)/(N-fm$rank))
      u <- apply(estfun(fm),2,function(x) tapply(x, cluster, sum))
      vcovCL <- dfc*sandwich(fm, meat=crossprod(u)/N)*dfcw
      coeftest(fm, vcovCL) }
    
    
    
    ###########################################
    #DiD for modal prices(base)
    ###########################################
    print(1)
    try({
      d = marketdat
      d$date = as.factor(d$date)

      
      d = na.omit(d)
      
      fit1 = lm(log(modal)~ market+date+inter,
                        data=d)
      crse1 = clx(fit1,1, d$market)
      
    })
    
    
    try({ci[p,] = crse1[rownames(crse1)=="inter1",]})

    
    
  }
  colnames(ci) = c("Estimate","Std.Error","t value","Pr(>|t|)")
  ci = cbind(product = Product,ci)
  write.xlsx(ci,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/",timemode," without features before outliers.xlsx",sep = ""))
  
}



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

for (tm in c(2)){
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
  
  ci = matrix(0,14,4)
  ci2 = matrix(0,14,4)
  
  for (p in 1:length(Product)){
    re = matrix(0,45,4)
    ree=matrix(0,45,1)
    
    product = Product[p]
    dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/DiD/",fmode,"/",product,sep = ""))
    
    features = Features[[p]]
    
    predictors = features
    
    
    
    
    #market modal price monthly
    marketdat = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",timemode,"/",product," third data.xlsx",sep = ""),sheet = 1)
    
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
    
    
    marketdat$date = cut(marketdat$date, breaks = "week")
    newmarketdat = ddply(marketdat,.(date,market),summarise, modal = weighted.mean(modal,amount))
    marketdat = newmarketdat
    marketdat$inter = 0
    marketdat$date = as.Date(marketdat$date)
    for (i in 1:length(intervented1)){
      temp = marketdat[marketdat$market==intervented1[i],]
      marketdat[marketdat$market==intervented1[i],][temp$date>=(interdate1[i]+0),"inter"] = 1
      
    }
    market = unique(marketdat$market)
    
   
    #calculate the average before treatment
    
    marketdat$inter = as.factor(marketdat$inter)
    clx <-function(fm, dfcw, cluster){
      library(sandwich)
      library(lmtest)
      M <- length(unique(cluster))
      N <- length(cluster)
      dfc <- (M/(M-1))*((N-1)/(N-fm$rank))
      u <- apply(estfun(fm),2,function(x) tapply(x, cluster, sum))
      vcovCL <- dfc*sandwich(fm, meat=crossprod(u)/N)*dfcw
      coeftest(fm, vcovCL) }
    
    
    
    ###########################################
    #DiD for modal prices(base)
    ###########################################
    print(1)
    try({
      d = marketdat
      d$date = as.factor(d$date)
      
      
      d = na.omit(d)
      
      fit1 = lm(log(modal)~ market+date+inter,
                data=d)
      crse1 = clx(fit1,1, d$market)
      
      
    })
    
    
    try({ci[p,] = crse1[rownames(crse1)=="inter1",]})
    
    
    
  }
  colnames(ci) = c("Estimate","Std.Error","t value","Pr(>|t|)")
  ci = cbind(product = Product,ci)
  write.xlsx(ci,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/",timemode," without features after outliers.xlsx",sep = ""))
  
}
