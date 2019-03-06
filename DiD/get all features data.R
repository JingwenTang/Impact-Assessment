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
Timemode = c("10-17 week","10-17 week all","13-17 month common types","13-17 month all common types",
             "13-17 week","13-17 week all","13-17 week common types","13-17 week all common types",
             "13-17 month without vdsa features","13-17 month all without vdsa features") #the time unit and lags chosen for the model in synthetic control

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
    
    
    #market modal price monthly
    marketdat = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",timemode,"/",product," eleventh data.xlsx",sep = ""),sheet = 1)
    colnames(marketdat)[1] = "date"
    
    marketdat = join(marketdat, fea,by = "market", type = "left")
    #marketdat = na.omit(marketdat)
    temp = intervented%in%unique(marketdat$market)
    intervented1 = intervented[temp]
    interdate1 = interdate[temp]
    
    marketdat$inter = 0
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
    marketdat$inter_3_months_lag = 0
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
    marketdat$date = as.Date(marketdat$date-25569,origin = "1970-01-01")
    for (i in 1:length(intervented1)){
      temp = marketdat[marketdat$market==intervented1[i],]
      marketdat[marketdat$market==intervented1[i],][temp$date>=(interdate1[i]+0),"inter"] = 1
      marketdat[marketdat$market==intervented1[i],][temp$date>=((interdate1 %m+% months(3))[i]+0),"inter_3_months_lag"] = 1
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
    
    #calculate the average before treatment
    temp = marketdat[(marketdat$market%in%intervented)&(marketdat$inter==0),]
    meanbefore = mean(temp$modal,na.rm = T)
    marketdat$time = as.numeric(marketdat$date)
    marketdat$month = month(marketdat$date,label = T)
    marketdat$treat = (marketdat$state == "karnataka")
    write.xlsx(marketdat, paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/all features/",product," data.xlsx",sep = ""))
  }
  
  
}
