library(readr)
library(openxlsx)
library(plyr)
library(dplyr)

dat1 = read.xlsx("/Users/somyasinghvi/Dropbox (MIT)/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/district features.xlsx",sheet = 1)
for (j in 2:(ncol(dat1))){
  dat1[,j] = as.numeric(as.character(dat1[,j]))
}
#to get the features by fraction
dat1[,2:9] = dat1[,2:9]/dat1[,31]
dat1[,15:25] = dat1[,15:25]/dat1[,14]
fea = dat1

fea$agrl_cult_t = fea$agrl_t + fea$cult_t
temp = c("market","catl_t","lroad","pop_ru","male_saw","agrl_cult_t","lit_ru","ptmrkt","total_ar")
fea = fea[,colnames(fea)%in%temp]
col_names = c("catl_t","lroad","pop_ru","male_saw","agrl_cult_t","lit_ru","ptmrkt","total_ar")
cor(fea[,colnames(fea)%in%col_names],use = "pairwise.complete.obs")
