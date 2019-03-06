rm(list = ls())
library(openxlsx)
Product = c("Groundnut","Jowar(Sorghum)","Maize","Sunflower","Arhar (Tur-Red Gram)","Cotton","Green Gram (Moong)","Arecanut(Betelnut-Supari)","Bengal Gram(Gram)","Black Gram (Urd Beans)","Dry Chillies","Copra","Kulthi(Horse Gram)","Paddy(Dhan)")
coln = c("modal","CV","VWCV","R","modal parallel","modal drop interim","modal placebo","modal without features",
         "modal within karnataka","modal state level","modal state fixed","modal psw logit","modal psw probit",
         "modal day info","modal state specific trend","modal market specific trend","modal state level num",
         "modal state fixed num","lead","lead4","lead3","lead2","lead1","lag1","lag2","lag3","lag4","lag","CV drop interim","VWCV drop interim","R drop interim",
         "modal psw logit drop interim","modal psw probit drop interim","modal add size_large","modal add size_medium_large","modal add size_medium_small","modal add size_small")
Res = c()
for (p in 1:length(Product)){
  dat = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/13-17 month inter coefficients.xlsx",sheet = Product[p])
  dat[,2] = as.numeric(dat[,2])
  dat[,5] = as.numeric(dat[,5])
  res = rep(NA,length(coln))
  for (l in c(1,6,8,9,10,11,12,13,14,15,16,17,18,24:28,32:37)){
    if ((is.na(dat[l,2])&(is.na(dat[l,5])))|(dat[l,5]==0)) res[l] = 0
    else if ((dat[l,2]>0)&(dat[l,5]<0.1)) res[l] = 1
  }
  for (l in c(2,3,4,29,30,31)){
    if ((is.na(dat[l,2])&(is.na(dat[l,5])))|(dat[l,5]==0)) res[l] = 0
    else if ((dat[l,2]<0)&(dat[l,5]<0.1)) res[l] = 1
  }
  for (l in c(5,7,19:23)){
    if (is.na(dat[l,5])|(dat[l,5]==0)) res[l] = 0
    else if (dat[l,5]>=0.1) res[l] = 1
  }
  Res = rbind(Res,res)
}
colnames(Res) = coln
Res = cbind(commodity = Product,Res)
s = rep(0,ncol(Res))
for (l in 2:ncol(Res)){
  s[l] = sum(Res[,l]==1,na.rm = T)
}
s[1] = "sum"
Res = rbind(Res,s)
rs = rep(0,(nrow(Res)-1))
for (l in 1:(nrow(Res)-1)){
  rs[l] = sum(Res[l,]==1,na.rm = T)
}
Res = cbind(Res,sum = c(rs," "))
write.xlsx(Res,"/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/top_90_result_summary.xlsx")



rm(list = ls())
library(openxlsx)
Product = c("Groundnut","Jowar(Sorghum)","Maize","Sunflower","Arhar (Tur-Red Gram)","Cotton","Green Gram (Moong)","Arecanut(Betelnut-Supari)","Bengal Gram(Gram)","Black Gram (Urd Beans)","Dry Chillies","Copra","Kulthi(Horse Gram)","Paddy(Dhan)")
coln = c("modal","CV","VWCV","R","modal parallel","modal drop interim","modal placebo","modal without features",
         "modal within karnataka","modal state level","modal state fixed","modal psw logit","modal psw probit",
         "modal day info","modal state specific trend","modal market specific trend","modal state level num",
         "modal state fixed num","lead","lead4","lead3","lead2","lead1","lag1","lag2","lag3","lag4","lag","CV drop interim","VWCV drop interim","R drop interim",
         "modal psw logit drop interim","modal psw probit drop interim","modal add size_large","modal add size_medium_large","modal add size_medium_small","modal add size_small")
Res = c()
for (p in 1:length(Product)){
  dat = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/13-17 month all inter coefficients.xlsx",sheet = Product[p])
  dat[,2] = as.numeric(dat[,2])
  dat[,5] = as.numeric(dat[,5])
  res = rep(NA,length(coln))
  for (l in c(1,6,8,9,10,11,12,13,14,15,16,17,18,24:28,32:37)){
    if ((is.na(dat[l,2])&(is.na(dat[l,5])))|(dat[l,5]==0)) res[l] = 0
    else if ((dat[l,2]>0)&(dat[l,5]<0.1)) res[l] = 1
  }
  for (l in c(2,3,4,29,30,31)){
    if ((is.na(dat[l,2])&(is.na(dat[l,5])))|(dat[l,5]==0)) res[l] = 0
    else if ((dat[l,2]<0)&(dat[l,5]<0.1)) res[l] = 1
  }
  for (l in c(5,7,19:23)){
    if (is.na(dat[l,5])|(dat[l,5]==0)) res[l] = 0
    else if (dat[l,5]>=0.1) res[l] = 1
  }
  Res = rbind(Res,res)
}
colnames(Res) = coln
Res = cbind(commodity = Product,Res)
s = rep(0,ncol(Res))
for (l in 2:ncol(Res)){
  s[l] = sum(Res[,l]==1,na.rm = T)
}
s[1] = "sum"
Res = rbind(Res,s)
rs = rep(0,(nrow(Res)-1))
for (l in 1:(nrow(Res)-1)){
  rs[l] = sum(Res[l,]==1,na.rm = T)
}
Res = cbind(Res,sum = c(rs," "))
write.xlsx(Res,"/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/result_summary.xlsx")
