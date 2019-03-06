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
br="week"
for (tm in 5:5){
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
  
  VWCV = rep(0,length(Product))
  for (p in 1:length(Product)){
    
    re = c()
    ree=c()
    product = Product[p]
    dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/DiD/",fmode,"/",product,sep = ""))
    
    features = Features[[p]]
    
    predictors = features
 
    ########market level data
    dat = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",timemode,"/",product," eleventh data.xlsx",sep = ""),sheet = 1)
    colnames(dat)[1] = "date"
    dat = dat[dat$market!="virtual",]
    dat = dat[!is.na(dat$amount),]
    dat$month = as.Date(dat$month-25569,origin = "1970-01-01")
    rm = c()
    newdat = c()
    market = unique(dat$market)
    mon = unique(dat$month)
    for (mo in 1:length(mon)){
      temp1 = dat[dat$month==mon[mo],]
      for (i in 1:length(market)){
        temp = temp1[temp1$market==market[i],]
        if (nrow(temp)>1){
          newdat = rbind(newdat,temp)
        }
      }
    }
    dat = newdat
    
    
    temp = intervented%in%unique(dat$market)
    intervented2 = intervented[temp]
    interdate2 = interdate[temp]
    dat$date = as.Date(dat$date-25569,origin = "1970-01-01")
    origintable = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/",product,"/",timemode,"/original table.xlsx",sep = ""),sheet = 1)
    totalvolume = sum(origintable[origintable$market%in%intervented2,"amount"],na.rm = T)
    

    su = aggregate(dat$amount,by = list(dat$month,dat$market),sum)
    colnames(su) = c("date","market","amount")
    sd = aggregate(dat$modal,by = list(dat$month,dat$market),sd,na.rm = T)
    colnames(sd) = c("date","market","sd")
    temp1 = join(su,sd,by = c("market","date"))
    wsd1 = ddply(temp1,.(market),summarise, wsd1 = weighted.mean(sd,amount))
    ma = aggregate(temp1$amount,by = list(temp1$market),sum)
    colnames(ma) = c("market","ma")
    temp = join(wsd1,ma,by = "market",type = "left")
    VWCV[p] = weighted.mean(temp$wsd1,temp$ma)
    
  }
  VWCV = cbind(Product,VWCV)
  write.xlsx(VWCV,"/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/VWCV.xlsx")
  }
EFFECT = rep(0,length(Product))
for (p in 1:length(Product)){
  product = Product[p]
  dat = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/13-17 week inter coefficients.xlsx",sheet = product)
  if ((as.numeric(dat[1,5])<=0.1)&(as.numeric(dat[1,5])>0)) EFFECT[p] = as.numeric(dat[1,2])
}
EFFECT = cbind(Product,EFFECT)
EFFECT = as.data.frame(EFFECT)
EFFECT = EFFECT[EFFECT$EFFECT!=0,]
VWCV = as.data.frame(VWCV)
jo = join(EFFECT,VWCV,by = "Product",type = "inner",match = "first")
pdf("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/EFFECT v.s. VWCV.pdf",width = 14,height = 7)
ggplot(dat = jo,aes(x = VWCV,y = EFFECT,group = 1))+
  geom_line()
dev.off()
