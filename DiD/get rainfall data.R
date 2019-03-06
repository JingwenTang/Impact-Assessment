rm(list = ls())
library(openxlsx)
dat = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/infrastructure_characteristics/rainfall.xlsx")
dat = dat[dat$INDICATOR.DESCRIPTION=="Actual Rainfall",]
dat = dat[,c(4,9,10)]
colnames(dat) = c("district","date","rainfall")
dat$district = tolower(dat$district)
md = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/market_districts.xlsx")
length(unique(md$district))
length(unique(dat$district))
length(intersect(unique(md$district),unique(dat$district)))
production = unique(dat$district)
basic = unique(md$district)
namestab = c()
for (i in 1:length(basic)){
  production1 = c()
  word = basic[i]
  cand = c()
  if((nchar(word)-2)>0){
    for (l in 1:length(production)){
      if (substr(word,1,1) == substr(production[l],1,1)) production1 = c(production1,production[l])
    }
    for (k in 1:(nchar(word)-2)){
      subword = substr(word,k,(k+2))
      subcand = grep(subword,production1,fixed = T,value = T)
      cand = c(cand,subcand)
      
    }
    cand = cand[!duplicated(cand)]
    cand = cand[!cand%in%basic]
    cand = c(word,cand)
    namestab = qpcR:::rbind.na(namestab,cand)
  }
}
namestab = namestab[!is.na(namestab[,2]),]
write.xlsx(namestab,"/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/rainfall to organized districts.xlsx")


rainfall = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/infrastructure_characteristics/daily rainfall.xlsx",sheet = 1)#the rainfall information include all 14 commodities
rainfall = rainfall[rainfall$INDICATOR.DESCRIPTION=="Actual Rainfall",]#select the actual rainfall information
rainfall = rainfall[,c(4,9,10)]
colnames(rainfall) = c("district","month","rainfall")
rainfall$district = tolower(rainfall$district)
rainfall$month = as.Date(as.character(rainfall$month),format = "%Y%m%d")
rainfall$month = cut(rainfall$month,breaks = "week")
monthlyrainfall = aggregate(rainfall$rainfall,by = list(rainfall$district,rainfall$month), mean,na.rm = T)
colnames(monthlyrainfall) = c("district","month","rainfall")
rainfall = monthlyrainfall
write.xlsx(rainfall,"/Users/tangjingwen/Dropbox/jingwen_tasks/infrastructure_characteristics/rainfall week.xlsx")

rainfall = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/infrastructure_characteristics/rainfall week.xlsx",sheet = 1)#the rainfall information include all 14 commodities
rainfall$month = as.Date(rainfall$month)
temp = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/rainfall to organized districts.xlsx",sheet = 1,colNames = F)
for (l in 1:nrow(temp)){
  if(sum(rainfall$district%in%temp[l,-1])!=0){
    add = rainfall[rainfall$district%in%temp[l,-1],] 
    add$district = temp[l,1]
    rainfall = rbind(rainfall,add)
    #rainfall = rainfall[!rainfall$district%in%temp[l,-1],]
  }
}
a = unique(md$district)[!unique(md$district)%in%rainfall$district]
write.xlsx(a,"/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/lost districts from rainfall.xlsx")



production = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/lost districts from rainfall.xlsx",colNames = F)$X1
md = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/market_districts.xlsx")
basic = unique(md$district)
namestab = c()
for (i in 1:length(basic)){
  production1 = c()
  word = basic[i]
  cand = c()
  if((nchar(word)-2)>0){
    for (l in 1:length(production)){
      if (substr(word,1,1) == substr(production[l],1,1)) production1 = c(production1,production[l])
    }
    for (k in 1:(nchar(word)-2)){
      subword = substr(word,k,(k+2))
      subcand = grep(subword,production1,fixed = T,value = T)
      cand = c(cand,subcand)
      
    }
    cand = cand[!duplicated(cand)]
    cand = cand[!cand%in%basic]
    cand = c(word,cand)
    namestab = qpcR:::rbind.na(namestab,cand)
  }
}
namestab = namestab[!is.na(namestab[,2]),]
