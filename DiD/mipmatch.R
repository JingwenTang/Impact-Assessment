rm(list = ls())
# Load package
library("mipmatch")
library(Rcplex)
library(slam)
library(plyr)
is.real = function (x){
  return(is.double(x))
}
as.real = function(x){
  return(as.double(x))
}


library(openxlsx)
library(MatchIt)
library(cem)
clx <-function(fm, dfcw, cluster){
  library(sandwich)
  library(lmtest)
  M <- length(unique(cluster))
  N <- length(cluster)
  dfc <- (M/(M-1))*((N-1)/(N-fm$rank))
  u <- apply(estfun(fm),2,function(x) tapply(x, cluster, sum))
  vcovCL <- dfc*sandwich(fm, meat=crossprod(u)/N)*dfcw
  coeftest(fm, vcovCL) }
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
Product = c("Groundnut","Jowar(Sorghum)","Maize","Sunflower","Arhar (Tur-Red Gram)","Cotton","Green Gram (Moong)","Arecanut(Betelnut-Supari)","Bengal Gram(Gram)","Black Gram (Urd Beans)","Dry Chillies","Copra","Kulthi(Horse Gram)","Paddy(Dhan)")
wb = createWorkbook() 
#c(1,2,3,5,6,7,9)
#c(1,2,5,6,7)
for (p in 1:1){
  product = Product[p]
  marketdat = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/13-17 month/",product," eleventh data.xlsx",sep = ""),sheet = 1)
  marketdat$treat = marketdat$state=="karnataka"
  
  newdat = data.frame(market = unique(marketdat$market))
  for (l in c(3:11,15:17,20,33)){
    temp = aggregate(marketdat[,l],by = list(marketdat$market),mean,na.rm = T)
    colnames(temp) = c("market",colnames(marketdat)[l])
    newdat = join(newdat,temp,by = "market")
  }
  temp = marketdat[,c(2,34)]
  temp = temp[!duplicated(temp$market),]
  newdat = join(newdat,temp,by = "market",type = "inner")
  marketdat = newdat
  for (l in c(2:15)){
    marketdat[,l] = as.numeric(marketdat[,l])
  }
  # IMPORTANT: allmatch needs the data to be sorted in decreasing order by the treatment indicator
  marketdat = marketdat[order(marketdat$treat, decreasing = TRUE) , ]
  attach(marketdat)
  
  # Treatment indicator
  t_ind = treat
  
  # Matrix of covariates
  X_mat = cbind(annual, total_ar,lit_ru,ptmrkt,agrl_t,production,yield,rainfall,Per.Capita.GSDP)
  
  # Distance matrix
  dist_mat = distmat(t_ind, X_mat)
  
  # Number of matches
  n_matches = 5
  
  # Moment covariates: rubble, flats
  mom_covs = cbind(annual, total_ar,lit_ru,ptmrkt,agrl_t,production,yield,rainfall,Per.Capita.GSDP)
  # Weights for the moment covariates
  mom_weights = c(0.1,100,0.1,0.5,0.5,0.5,0.5,0.5,0.5)
  
  # Tolerances for the moment covariates
  mom_tols = NULL
  
  # Kolmogorov-Smirnov covariates
  ks_covs = NULL
  # Number of grid points for the Kolmogorov-Smirnov statistic
  ks_n_grid = 10
  # Weights for the Kolmogorov-Smirnov covariates
  ks_weights = NULL
  # Tolerances for the Kolmogorov-Smirnov covariates
  ks_tols = NULL
  
  # Covariates for near-exact matching, fine and near-fine balance
  exact_covs = NULL
  near_exact_covs = NULL
  near_exact_devs = NULL
  fine_covs = NULL
  near_fine_covs = NULL
  near_fine_devs = NULL
  
  # Whether specific controls need to be used
  use_controls_mat = NULL
  use_controls_totals = NULL 
  use_controls_signs = NULL
  
  # Enforce all the constraints
  enforce_constraints = FALSE
  
  # Find the match
  out = allmatch(dist_mat, t_ind, n_matches, 
                 mom_covs, mom_weights, mom_tols,
                 ks_covs, ks_n_grid, ks_weights, ks_tols,
                 exact_covs, 
                 near_exact_covs, near_exact_devs, 
                 fine_covs, 
                 near_fine_covs, near_fine_devs,
                 use_controls_mat, use_controls_totals, use_controls_signs,
                 enforce_constraints)
  
  # Indices of the treated units and matched controls
  t_id = which(t_ind==1)	
  c_id = out$c_id	
  
  balance = cbind(c("annual", "total_ar","lit_ru","ptmrkt","agrl_t","production","yield","rainfall","Per.Capita.GSDP)"),meantab(X_mat, t_ind, t_id, c_id, digits = 2))
  
  marketdat = marketdat[c(t_id,c_id),]
  market = as.character(unique(marketdat$market))
  write.xlsx(market,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/mipmatch/",product," selected markets.xlsx",sep = ""))
  write.xlsx(balance,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/mipmatch/",product," balance table.xlsx",sep = ""))
  
  
}
