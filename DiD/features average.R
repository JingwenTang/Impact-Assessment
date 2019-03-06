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
library(dplyr)
Timemode = c("13-17 month","13-17 month all","13-17 month common types","13-17 month all common types",
             "13-17 week all","10-17 week all","13-17 week common types","13-17 week all common types",
             "13-17 month without vdsa features","13-17 month all without vdsa features") #the time unit and lags chosen for the model in synthetic control

for (tm in c(6)){
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
    
    re = c()
    ree=c()
    product = Product[p]
    dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/DiD/",fmode,"/",product,sep = ""))
    
    features = Features[[p]]
    
    predictors = features
    #market modal price monthly
    marketdat = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",timemode,"/",product," eleventh data.xlsx",sep = ""),sheet = 1)
    colnames(marketdat)[1] = "date"

    #####to remove the data points when only with treated or untreated markets
    #marketdat$treat = (marketdat$state=="karnataka")
    #marketdat$treat = as.factor(marketdat$treat)
    
    
    #newmarketdat = c()
    #da = unique(marketdat$date)
    #for (l in 1:length(da)){
    #temp = marketdat[marketdat$date == da[l],]
    #if (length(unique(temp$treat))==2){
    #newmarketdat = rbind(newmarketdat,temp)
    #}
    #}
    #marketdat = newmarketdat[,-ncol(newmarketdat)]
    
    ##marketdat = marketdat[!marketdat$market%in%after2017,]
    marketdat$treat = (marketdat$state=="karnataka")
    
    marketdat = marketdat[,-c(1,2,12,13,14,18,19,20)]
    dat = marketdat %>% 
      group_by(treat) %>%
      summarise_all("mean",na.rm = T)
    dat = cbind(colnames(dat),t(dat))
    #dat = aggregate(. ~ treat, data = marketdat, FUN = mean)
    addWorksheet(wb,product)
    writeData(wb,sheet = product,dat)
    
    
  }
  saveWorkbook(wb,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/average/",timemode," average.xlsx",sep = ""),overwrite = T)
  }
