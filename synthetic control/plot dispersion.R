rm(list = ls())
library(openxlsx)
library(plyr)
library(ggplot2)


region = read.xlsx("E:/Dropbox/jingwen_tasks/infrastructure_characteristics/Market Details.xlsx",sheet = " District & Taluk")
colnames(region) = c("region","district","market","taluk")
for (j in 1:ncol(region)){
  region[,j] = tolower(region[,j])
}
temp = read.csv("E:/Dropbox/jingwen_tasks/CombinedArrivals/intervented change to organized.csv",header = F)
colnames(temp) = c("int","org")
temp$int = as.character(temp$int)
temp$org = as.character(temp$org)
inind = region$market%in%temp$int
orind = match(region$market[inind],temp$int)
region$market[inind] = temp$org[orind]


rn = (1:nrow(region))[!is.na(region$region)]
for (l in 1:(length(rn)-1)){
  region$region[(rn[l]+1):(rn[l+1]-1)] = region$region[rn[l]]
}
l = length(rn)
region$region[(rn[l]+1):nrow(region)] = region$region[rn[l]]
region = region[,c("market","region")]

#dat = read.xlsx("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/Groundnut/12-17 mon-qua/common type price features.xlsx",sheet = 1)
dat = read.xlsx("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/Groundnut/13-17 week wo vw/seventh data.xlsx",sheet = 1)
treated = as.character(read.xlsx("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/Groundnut/treat.xlsx",sheet = 1,colNames = F)$X1)
dat = dat[dat$market%in%treated,]
dat = dat[dat$market!="virtual",]
dat$month = as.Date(dat$month-25569,origin = "1970-01-01")
dat = join(dat,region,by = "market",type = "left")


plt = ggplot(data = dat,aes(x = month,y = modal,group = market)) + 
  geom_line()
print(plt)
for (l in 1:length(unique(dat$region))){
  temp = dat[dat$region==unique(dat$region)[l],]
  print(ggplot(data = temp,aes(x = month,y = modal,group = market)) + 
          geom_line()+
          ggtitle(paste("region ", unique(dat$region)[l])) +
    xlab("week"))
}

sd = aggregate(dat$modal,by = list(dat$month),sd)
colnames(sd) = c("month","sd")
m = aggregate(dat$modal,by = list(dat$month),mean)
colnames(m) = c("month","m")
Dat = join(sd,m,by = c("month"),type = "left")
Dat$CV = Dat$sd/Dat$m

print(ggplot(data = Dat,aes(x = month,y = CV)) + 
        geom_line()+
        xlab("week"))

for (l in 1:length(unique(dat$region))){
  temp = dat[dat$region==unique(dat$region)[l],]
  sd = aggregate(temp$modal,by = list(temp$month),sd)
  colnames(sd) = c("month","sd")
  m = aggregate(temp$modal,by = list(temp$month),mean)
  colnames(m) = c("month","m")
  Temp = join(sd,m,by = c("month"),type = "left")
  Temp$CV = Temp$sd/Temp$m
  
  print(ggplot(data = Temp,aes(x = month,y = CV)) + 
          geom_line()+
          ggtitle(paste("region ", unique(dat$region)[l])) +
          xlab("week"))
  
}



##draw the cumuative plot for treated markets

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



tr1 = read.xlsx("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/Groundnut/12-17 mon-qua marketwise daily/original table.xlsx",sheet = 1)
Tr1 = tr1[tr1$market%in%intervented,]
tr1 = tr1[tr1$market%in%treated,]
for (l in 1:nrow(tr1)){
  tr1$cumu[l] = sum(tr1$amount[1:l])/sum(Tr1$amount,na.rm = T)
}
tr1$data_source = "organized"
tr2 = read.xlsx("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/Groundnut/combined data week wv/original table.xlsx",sheet = 1)
Tr2 = tr2[tr2$market%in%intervented,]
tr2 = tr2[tr2$market%in%treated,]
tr2$data_source = "combined"
tr21 = join(tr1,tr2,by = "market",type = "left")
tr2 = tr21[,c(1,5:6)]
for (l in 1:nrow(tr2)){
  tr2$cumu[l] = sum(tr2$amount[1:l])/sum(Tr2$amount,na.rm = T)
}
tr = rbind(tr1,tr2)
tr$vjust = c(2,-2)[(tr$data_source=="organized")+1]
plt1 = ggplot(data = tr,aes(x = reorder(market,cumu),y=cumu,group = data_source)) + 
  geom_line(aes(linetype=data_source))+
  geom_point(size = 0.5)+
  geom_text(aes(label=round(cumu,3), vjust=vjust),size=2)+
  theme(text = element_text(size=7),
        axis.text.x = element_text()) +
  xlab("market")+
  ylim (0,1.2)




pdf("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/Groundnut/cumulative plot.pdf",width = 8,height = 4)
print(plt1)
dev.off()
