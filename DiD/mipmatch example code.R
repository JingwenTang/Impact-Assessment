# Load package
library("mipmatch")
library(Rcplex)
library(slam)
is.real = function (x){
  return(is.double(x))
}
as.real = function(x){
  return(as.double(x))
}
# Load data
data(germancities)

# IMPORTANT: allmatch needs the data to be sorted in decreasing order by the treatment indicator
germancities = germancities[order(germancities$treat, decreasing = TRUE) , ]
attach(germancities)

# Treatment indicator
t_ind = treat

# Matrix of covariates
X_mat = cbind(log2pop, popgrowth1939, popgrowth3339, emprate, indrate, 
              rubble, rubblemiss, flats, flatsmiss, refugees)

# Distance matrix
dist_mat = distmat(t_ind, X_mat)

# Number of matches
n_matches = 1

# Moment covariates: rubble, flats
mom_covs = cbind(emprate, flats)
# Weights for the moment covariates
mom_weights = NULL
# Tolerances for the moment covariates
mom_tols = c(.1, .3)

# Kolmogorov-Smirnov covariates
ks_covs = cbind(refugees)
# Number of grid points for the Kolmogorov-Smirnov statistic
ks_n_grid = 10
# Weights for the Kolmogorov-Smirnov covariates
ks_weights = NULL
# Tolerances for the Kolmogorov-Smirnov covariates
ks_tols = c(.2)

# Covariates for near-exact matching, fine and near-fine balance
exact_covs = NULL
near_exact_covs = cbind(educat)
near_exact_devs = 1
fine_covs = cbind(publicat, busiservcat)
near_fine_covs = cbind(transcat, mineralcat)
near_fine_devs = 2

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

# Describe covariate balance: means
meantab(X_mat, t_ind, t_id, c_id, digits = 2)

# Describe covariate balance: marginal distributions of nominal covariates
finetab(educat, t_id, c_id)
finetab(publicat, t_id, c_id)
finetab(busiservcat, t_id, c_id)
finetab(transcat, t_id, c_id)
finetab(mineralcat, t_id, c_id)	

# Describe covariate balance: marginal dist. of the Kolmogorov-Smirnov covariate
x = ks_covs[, 1] 
xlim_min = min(x)
xlim_max = max(x)
par(mfrow = c(2, 1))
ecdfplot(x, t_id, which(t_ind == 0), "Before matching")
ecdfplot(x, t_id, c_id, "After matching")
