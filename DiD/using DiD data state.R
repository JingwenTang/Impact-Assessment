rm(list = ls())
library(openxlsx)
library(Synth)
library(plyr)
Product = c("Groundnut","Jowar(Sorghum)","Maize","Sunflower","Arhar (Tur-Red Gram)","Cotton","Green Gram (Moong)","Arecanut(Betelnut-Supari)","Bengal Gram(Gram)","Black Gram (Urd Beans)","Dry Chillies","Copra","Kulthi(Horse Gram)","Paddy(Dhan)")
timemode = "13-17 month"
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

p=1
product = Product[p]

#1 to synthesize using state as a unit
dat = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",timemode,"/",product," eleventh data.xlsx",sep = ""),sheet = 1)
colnames(dat)[1] = "date"
dat$date = as.Date(dat$date-25569,origin = "1970-01-01")
dat = dat[dat$market!="virtual",]

temp = intervented%in%unique(dat$market)
intervented2 = intervented[temp]
interdate2 = interdate[temp]


m = aggregate(dat$modal,by = list(dat$date,dat$state),mean)
colnames(m) = c("date","state","mean")
newdat = as.data.frame(aggregate(dat$amount, list(dat$date,dat$state), sum,na.rm = T))
colnames(newdat) = c("date","state","amount")
Dat = join(m,newdat,by = c("date","state"),type = "left")

for (l in c(7:11,15:17,20:33)){
  name = colnames(dat)[l]
  colnames(dat)[l] = "target"
  newdat = as.data.frame(ddply(dat,.(date,state),summarise, wmean = weighted.mean(target,amount,na.rm = T)))
  colnames(newdat) = c("date","state",name)
  Dat = join(Dat,newdat,by = c("date","state"),type = "left")
  colnames(dat)[l] = name
}
colnames(Dat)[colnames(Dat)=="state"]="market"
#balance the panel
#mon = unique(Dat[Dat$market=="karnataka","date"])
#choose top 90 months
Temp = Dat[Dat$market=="karnataka",]
Temp = Temp[order(Temp$amount,decreasing = T),]
thres = sum(Temp$amount)*0.9
for (l in 1:nrow(Temp)){
  if (sum(Temp$amount[1:l])>thres) break
}
mon = Temp$date[1:(l-1)]

allmar = unique(Dat$market)
Dat=Dat[Dat$date%in%mon,]
mar = c()
for (m in 1:length(allmar)){
  if (nrow(Dat[Dat$market==allmar[m],])==length(mon)) mar = c(mar,allmar[m])
}
Dat = Dat[Dat$market%in%mar,]
Dat$marketno = as.numeric(as.factor(Dat$market))
Dat$Month = as.numeric(as.factor(Dat$date))
no = Dat[Dat$market=="karnataka","marketno"][1]
temp = unique(Dat$date)[which.min(abs(unique(Dat$date)-min(interdate2)))]
subintervention = Dat[Dat$date==temp,"Month"][1]
t0 = 1:(subintervention-1)
nonin = unique(Dat$marketno)[!unique(Dat$marketno)==no]
d=Dat

dataprep.out <-
  dataprep(foo = d,
           predictors = colnames(d)[c(4:9,11:13,26)] ,
           predictors.op = "mean" ,
           time.predictors.prior = (min(d$Month)):(subintervention-1) ,
           dependent = "modal",
           unit.variable = "marketno",
           unit.names.variable = "market",
           time.variable = "Month",
           treatment.identifier = "karnataka",
           controls.identifier = nonin,
           time.optimize.ssr = (min(d$Month)):(subintervention-1),
           time.plot = (min(d$Month)):(max(d$Month))
  )
synth.out <- synth(data.prep.obj = dataprep.out,method = "BFGS")
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                          synth.res = synth.out)
