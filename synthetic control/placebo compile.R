rm(list = ls())
library(openxlsx)
library(ggplot2)
subintervented = c( "raichur"     ,     "bagalakot"     ,   "hubli (amaragol)" ,"gadag"    ,       
                    "challakere"    ,   "laxmeshwar"   ,    "yadgir"    ,       "mundaragi"  ,     
                    "kottur"    ,       "bellary"     ,     "chitradurga"  ,    "davangere" ,      
                    "ramdurga"   ,      "savanur"  ,         "koppal"   ,       
                        "hiriyur" )
Qt = c()
for (s in 1:length(subintervented)){



treated = read.xlsx("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/Groundnut/result tables1.xlsx",sheet = "gaps_analysis")

untreated = read.xlsx(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/placebo/sub2/",subintervented[s],"/Groundnut/",subintervented[s],"result tables.xlsx",sep = ""),sheet = "gaps_analysis")
colnames(treated)[1:2] = c("market","model")
treated = treated[treated$market==subintervented[s],]
colnames(untreated)[1:2] = c("market","model")
treated = treated[treated$model=="12-17 mon-qua",c(1,15,16)]
untreated = untreated[untreated$model=="12-17 mon-qua",c(1,15,16)]
treated$ratio = treated$RMSPE_after/treated$RMSPE_before
untreated$ratio = untreated$RMSPE_after/untreated$RMSPE_before
treated$treat = "treated"
untreated$treat = "untreated"
treated = treated[order(treated$ratio,decreasing = T),]
untreated = untreated[order(untreated$ratio,decreasing = T),]
dat = rbind(treated,untreated)
dat$treat = factor(dat$treat)
qt = 1-ecdf(dat$ratio)(dat[dat$market==subintervented[s],"ratio"])
Qt = c(Qt,qt)
}

result = data.frame(market = subintervented,quantile = Qt)
result = result[order(result$quantile),]

supp = read.xlsx("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/Groundnut/result tables1.xlsx",sheet="gaps_analysis")
supp = supp[supp$V2=="12-17 mon-qua",c(1,12)]
colnames(supp)[1] = "market"
result = join(result,supp,by = "market",type = "left")
write.xlsx(result,"E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/placebo/sub2/all markets result.xlsx")

pdf("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/placebo/sub2/all markets result.pdf")
ggplot(result, aes(quantile))+
  geom_bar()
dev.off()



control = c()
for (s in 1:length(subintervented)){
  temp = read.xlsx(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/placebo/sub2/",subintervented[s],"/Groundnut/12-17 mon-qua/data_details.xlsx",sep = ""),sheet="Market_info")
  temp = temp[-1,]
  nonin = c()
  for (l in 1:ncol(temp)){
    num = sum(!is.na(temp[,l]))-2
    nonin = c(nonin,temp[1:num,l])
  }
  nonin = unique(nonin)
  control = c(control,nonin)
}
control = unique(control)
temp = read.xlsx("E:/Dropbox/jingwen_tasks/SCM/market_districts.xlsx",sheet=1)
colnames(temp)[1:4] = c("market","dis1","dis2","state")
temp$market = tolower(temp$market)
temp$state = tolower(temp$state)
temp = temp[temp$market%in%control,]
temp = temp[,c(1,4)]
table(temp$state)




table = read.xlsx("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/Maize/12-17 mon-quaoriginal table.xlsx",sheet = 1)
table = table[table$market%in%intervented,]
for (l in 1:nrow(table)){
  table$sum[l] = sum(table$amount[1:l])
}
table$portion = table$sum/table$sum[nrow(table)]
