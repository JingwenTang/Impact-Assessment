rm(list = ls())
Product = c("Groundnut","Jowar(Sorghum)","Maize","Sunflower","Arhar (Tur-Red Gram)","Cotton","Green Gram (Moong)","Arecanut(Betelnut-Supari)","Bengal Gram(Gram)","Black Gram (Urd Beans)","Dry Chillies","Copra","Kulthi(Horse Gram)","Paddy(Dhan)")
re = c()
for (p in c(14,1,3,7,6,5)){
  product = Product[p]
  dat = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/average/10-17 week all average removing one level.xlsx",sheet = product)
  re1 = c()
  re1 = rbind(re1, dat[dat$V1 == "rainfall",])
  re1 = rbind(re1, dat[dat$V1 == "production",])
  re1 = rbind(re1, dat[dat$V1 == "area",])
  re1 = rbind(re1, dat[dat$V1 == "yield",])
  re1 = rbind(re1, dat[dat$V1 == "lit_ru",])
  re1 = rbind(re1, dat[dat$V1 == "Per.Capita.GSDP",])
  re1$tr = re1$V3
  re1$un = re1$V2
  re = cbind(re,re1$tr,re1$un)
}
write.xlsx(re,"/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/average/10-17 week all average removing one level (part).xlsx")