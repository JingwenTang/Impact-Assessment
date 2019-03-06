rm(list = ls())
library(readr)
library(openxlsx)
library(plyr)
library(dplyr)

fMode = c("full","sub","sub2")
f = list()
f[[1]]= c("annual","total_ar","lit_t","male_saw","annmai","lit_ru","ptmrkt","lroad","agrl_t")
f[[2]] = c("annual","total_ar","lit_t","male_saw")
f[[3]] = c("annual","total_ar","lit_t","agrl_t","ptmrkt")
for (size in 3:3){
  fmode = fMode[size]
  f = f[[size]]
  dir.create(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/",fmode,sep = ""))
  Product = c("Groundnut","Jowar(Sorghum)","Maize","Sunflower","Arhar (Tur-Red Gram)","Cotton","Green Gram (Moong)","Arecanut(Betelnut-Supari)","Bengal Gram(Gram)","Black Gram (Urd Beans)","Dry Chillies","Copra","Kulthi(Horse Gram)","Paddy(Dhan)")
  Wpiproduct = c("Groundnut Seed","Jowar","Maize","Sunflower","Arhar","Cotton Seed","Moong","Betelnut/Arecanut","Gram","All commodities","Chillies (Dry)","Copra (Coconut)","All commodities","All commodities")
  Model = c("12-17 mon-mon","12-17 mon-qua","12-17 qua-qua","12-17 qua-half")
  Begin = c(2012,2012,2012,2012)
  Over = c(2017,2017,2017,2017)
  Br = c("month","month","3 months","3 months")
  early = as.Date(c("2012-04-01","2012-04-01","2012-04-01","2012-04-01"))
  late = as.Date(c("2018-01-01","2018-01-01","2018-01-01","2018-01-01"))
  Lagbr  = c("month","3 months","3 months","6 months")
  
  setpre = c(1,3,1,2)
  Features = list()
  Features[[1]] = c("gnut_tq",f)
  Features[[2]] = c("sorg_tq",f)
  Features[[3]] = c("maiz_tq",f)
  Features[[4]] = c("sunf_tq",f)
  for (j in 1:length(Product)){
    Features[[j]] = f
  }
  intervention= as.data.frame(read.xlsx("E:/Dropbox/jingwen_tasks/infrastructure_characteristics/Market Details.xlsx",sheet = "Market Live Dates"))[3:160,]
  intervention$X3 = tolower(as.character(intervention$X3))
  
  temp = read.csv("E:/Dropbox/jingwen_tasks/CombinedArrivals/intervented change to organized.csv",header = F)
  colnames(temp) = c("int","org")
  temp$int = as.character(temp$int)
  temp$org = as.character(temp$org)
  inind = intervention$X3%in%temp$int
  orind = match(intervention$X3[inind],temp$int)
  intervention$X3[inind] = temp$org[orind]
  intervented = tolower(as.character(intervention$X3))
  intervented = c(intervented,"virtual")
  for (p in 1:length(Product)){
    product = Product[p]
    dir.create(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/placebo/",fmode,sep = ""))
    dir.create(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/placebo/",fmode,"/",product,sep = ""))
        for (m in 2:2){
    temp = read.xlsx(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/",fmode,"/",product,"/",Model[m],"/data_details1.xlsx",sep = ""),sheet="Market_info")
    temp = temp[-1,]
    nonin = c()
    for (l in 1:ncol(temp)){
      num = sum(!is.na(temp[,l]))-2
      nonin = c(nonin,temp[1:num,l])
    }
    nonin = unique(nonin)
    
    nonin1 = c()
    dat = read.xlsx(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/",fmode,"/",product,"/",Model[m],"/common type price features.xlsx",sep = ""),sheet=1)
    dat = dat[!is.na(dat$modal),]
    for (l in 1:length(nonin)){
      temp = dat[dat$market==nonin[l],]
      
      print(nrow(temp))
      if (nrow(temp)>60){
        nonin1 = c(nonin1,nonin[l])
      }
    }
    
    write.xlsx(nonin1,paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/placebo/",fmode,"/",product,"/treat.xlsx",sep = ""))
    
    
    }
  }
}



