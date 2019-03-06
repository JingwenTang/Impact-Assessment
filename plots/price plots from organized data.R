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
Timemode = c("10-17 month","10-17 month all","13-17 month common types","13-17 month all common types",
             "10-17 week","10-17 week all","13-17 week common types","13-17 week all common types",
             "13-17 month without vdsa features","13-17 month all without vdsa features") #the time unit and lags chosen for the model in synthetic control
br="month"
for (tm in c(6)){
  timemode = Timemode[tm]
  dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/plots/",timemode,sep = ""))
  setwd(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/plots/",timemode,sep = ""))
  
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
  
  intervention = intervention[,c(3,4)]
  colnames(intervention) = c("market","date")
  intervention$date = as.Date(as.numeric(intervention$date)-25569,origin = "1970-01-01")
  
  
  
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
    marketdat = marketdat[marketdat$market!="virtual",]
    temp = intervented%in%unique(marketdat$market)
    intervented1 = intervented[temp]
    interdate1 = interdate[temp]
    
    marketdat = marketdat[!is.na(marketdat$modal),]
    #marketdat = marketdat[!is.na(marketdat$amount),]
    #####to remove the data points when only with treated or untreated markets
    marketdat$treat = (marketdat$state=="karnataka")
    marketdat$treat = as.factor(marketdat$treat)
    newmarketdat = c()
    da = sort(unique(marketdat$date))
    for (l in 1:length(da)){
      temp = marketdat[marketdat$date == da[l],]
      if (length(unique(temp$treat))==2){
        newmarketdat = rbind(newmarketdat,temp)
      }
    }
    marketdat = newmarketdat
    marketdat$treat = (marketdat$state=="karnataka")
    marketdat$date = as.Date(marketdat$date-25569,origin = "1970-01-01")
    dat = marketdat
    
    wm = ddply(dat,.(date,treat),summarise, wmean = weighted.mean(modal,amount))
    wm$treat[wm$treat == T] = "treated"
    wm$treat[wm$treat == F] = "untreated"
    interday = as.Date(intervention[intervention$market%in%marketdat$market,"date"])
    
    d1 = min(interday)
    d3 = max(interday)
    tr = marketdat[marketdat$market%in%intervented,]
    s = sum(tr$amount)*0.5
    temp = aggregate(tr$amount,by = list(tr$market),sum,na.rm = T)
    colnames(temp) = c("market","amount")
    temp = join(temp,intervention,by = "market",type = "left")
    temp = temp[order(temp$date,decreasing = F),]
    for (l in 1:nrow(temp)){
      if (sum(temp$amount[1:l])>=s) {
        d2 = temp[l,"date"]
        break
        }
    }
    
    pdf(paste(product," ",timemode,".pdf",sep = ""),width = 14,height = 8)
    print(ggplot(data = wm)+
            geom_line(aes(x = date,y = wmean,group = treat,color = treat))+
            geom_vline(xintercept = c(d1,d2,d3),linetype = "dotted"))
    dev.off()
    
    addWorksheet(wb,product)
    writeData(wb,sheet = product,wm)
    
    
    
    
  }
  
  saveWorkbook(wb,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/plots/",timemode,".xlsx",sep = ""),overwrite = T)
  
  
  }
