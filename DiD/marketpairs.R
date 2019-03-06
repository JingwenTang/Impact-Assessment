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
library(utils)
Timemode = c("13-17 month","13-17 month all","13-17 month common types","13-17 month all common types",
             "10-17 week","10-17 week all","13-17 week common types","13-17 week all common types") #the time unit and lags chosen for the model in synthetic control

Br = c("month","month","month","month","week","week","week","week") 
for (tm in c(6)){
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
      #market modal price weekly
      marketdat = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",timemode,"/",product," seventh data.xlsx",sep = ""),sheet = 1)
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
      
      marketdat = newd
      
      marketdat = na.omit(marketdat)
      
      marketdat = marketdat[marketdat$market%in%intervented,]
      
      
      
      #the function to calculate the distance based on the latitude and longtitude
      getDistanceFromLatLonInKm = function (lat1,lon1,lat2,lon2) {
        R = 6371;
        dLat = deg2rad(lat2-lat1);  
        dLon = deg2rad(lon2-lon1); 
        a = sin(dLat/2) * sin(dLat/2) +
          cos(deg2rad(lat1)) * cos(deg2rad(lat2)) * 
          sin(dLon/2) * sin(dLon/2);
        c = 2 * atan2(sqrt(a), sqrt(1-a)); 
        d = R * c;
        return(d);
      }
      deg2rad = function (deg) {
        return(deg * (pi/180))
      }
      loc = read.csv("/Users/tangjingwen/Dropbox/jingwen_tasks/Market_Locations_Google_API.csv")[,c(3,9,10)]
      loc = na.omit(loc)
      colnames(loc) = c("market","lat","lng")
      loc$market = as.character(loc$market)
      loc = loc[!duplicated(loc$market),]
      locmar = unique(loc$market)
      
      #deal with different spelling
      namematch = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/google location to organized.xlsx",sheet = 1,colNames = F)
      colnames(namematch) = c("organized","location")
      for (l in 1:nrow(namematch)){
        loc[loc$market==namematch[l,2],"market"]=namematch[l,1]
      }
      
      pair = t(combn(unique(marketdat$market),m=2))
      marketdat = marketdat[,c("market","date","modal","inter")]
      newmarketdat = c()
      for (l in 1:nrow(pair)){
        try({
          pa = pair[l,]
          temp1 = marketdat[marketdat$market==pa[1],]
          temp2 = marketdat[marketdat$market==pa[2],]
          temp = join(temp1,temp2,by = "date",type = "inner")
          colnames(temp) = c("market1","date","modal1","inter1","market2","modal2","inter2")
          temp$market = paste(temp$market1,temp$market2,sep = "_")
          temp$modal = abs(temp$modal1-temp$modal2)
          temp$inter = temp$inter1*temp$inter2
          lat1 = loc[loc$market==pa[1],"lat"]
          lon1 = loc[loc$market==pa[1],"lng"]
          lat2 = loc[loc$market==pa[2],"lat"]
          lon2 = loc[loc$market==pa[2],"lng"]
          temp$dist = getDistanceFromLatLonInKm(lat1,lon1,lat2,lon2)
          newmarketdat = rbind(newmarketdat,temp)
        })
        
      }
      
      pairdat = newmarketdat[,c("market","dist")]
      pairdat = pairdat[!duplicated(pairdat$market),]
      
      addWorksheet(wb,product)
      
      writeData(wb,sheet = product,pairdat)
      
    })
    
  }
  saveWorkbook(wb,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/marketpairs/",timemode," all market pairs.xlsx",sep = ""),overwrite = T)
}



for (tm in 6){
  timemode = Timemode[tm]
  wbcal = createWorkbook() 
  for (t in c(20,40,60,80,100,120,140,160,180,200)){
    wb = createWorkbook() 
    cal = c()
    for (p in 1:length(Product)){
      aver = 0
      num = 0
      num90 = 0
      product = Product[p]
      
      try({
        dat = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/marketpairs/",timemode," all market pairs.xlsx",sep = ""),sheet = product)
        overall = c()
        for (l in 1:nrow(dat)){
          temp = dat[l,"market"]
          temp = strsplit(temp,"_")
          tempname1 = temp[[1]][1]
          tempname2 = temp[[1]][2]
          overall = c(overall,tempname1)
          overall = c(overall,tempname2)
        }
        overall = unique(overall)
        overallnum = length(overall)
        
        amount = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",timemode,"/original table.xlsx",sep = ""))
        amount = amount[order(amount$amount,decreasing = T),]
        thre = sum(amount$amount,na.rm = T)*0.9
        for (l in 1:nrow(amount)){
          if (sum(amount$amount[1:l],na.rm = T)>thre) break
        }
        marketname90 = amount$market[1:l]
        marketname90 = unique(marketname90)
        newmarketname90 = overall[overall%in%marketname90]
        num90 = length(newmarketname90)
        
        dat$dist = as.numeric(dat$dist)
        dat = dat[dat$dist<=t,]
        addWorksheet(wb,product)
        writeData(wb,sheet = product,dat)
        
        newdat = c()
        for (l in 1:nrow(dat)){
          temp = dat[l,"market"]
          temp = strsplit(temp,"_")
          tempname1 = temp[[1]][1]
          tempname2 = temp[[1]][2]
          if ((tempname1%in%marketname90)&(tempname2%in%marketname90)){
            newdat = rbind(newdat,dat[l,])
          }
        }
        dat = newdat
        
        dat$dist = as.numeric(dat$dist)
        aver = mean(dat$dist)
        marketname = c()
        for (l in 1:nrow(dat)){
          temp = dat[l,"market"]
          temp = strsplit(temp,"_")
          tempname1 = temp[[1]][1]
          tempname2 = temp[[1]][2]
          marketname = c(marketname,tempname1)
          marketname = c(marketname,tempname2)
        }
        marketname = unique(marketname)
        num = length(marketname)
        
      })
      
     
      
      cal = rbind(cal,c(overallnum,num90,num,aver))
    }
    colnames(cal) = c("overall markets","number of markets in top 90%","number of filtering markets","average distance")
    cal = cbind(product = Product,cal)
    saveWorkbook(wb,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/marketpairs/",timemode," all market pairs ",t,".xlsx",sep = ""),overwrite = T)
    addWorksheet(wbcal,paste(t))
    writeData(wbcal,sheet = paste(t),cal)
  }
  saveWorkbook(wbcal,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/marketpairs/",timemode," data output.xlsx",sep = ""),overwrite = T)
  
    
}


