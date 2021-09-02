"
2021.08.26
DataCamp competition

loans pip

"
rm(list=ls())
v_target <- c("not_fully_paid", "purpose")[1]
source("loans_fn.R")
df <- load_data(as_df = TRUE)
df <- df %>% slice_sample(n=3000)
str(df)
summary(df)

# could do gower and predict, but there's neither missing data and almost all vars are numeric!
# could do pipelines
# could do both


#### compare nearest-neighbors methods ####
# data prep
df <- cast(df, type_from="character", type_to="factor") # purpose is a factor

# tt
tt <- ttsplit(df, .5)
# tv (to choose gower weights)
tv <- ttsplit(tt$train, .5)

# scale
tt <- scale_numeric_features_in_train_and_test(tt)
tv <- scale_numeric_features_in_train_and_test(tv)




#### gower with daisy: optimize gower weights ####

# loop to optimize weights
weights <- get_gower_weights(tv, min_vars = 3, n_combinations = 100)
metrics <- get_gower_metrics_for_weights(tv, weights_matrix = weights, eval_fn = yardstick::roc_auc)
weights_max <- get_gower_best_weights(weights, metrics, choose_by=c("accuracy","mean_metric","auc")[3])
weights_manual <- c(credit_policy=1, purpose=1, int_rate=0, installment=0, log_annual_inc=0, 
                    dti=0, fico=0, days_with_cr_line=0, revol_bal=0,
                    revol_util=0, inq_last_6mths=0, delinq_2yrs=0, pub_rec=1) %>% as.list %>% as.data.frame

get_gower_metrics_for_weights(tt, weights_matrix = weights_max, eval_fn = yardstick::roc_curve)
get_gower_metrics_for_weights(tt, weights_matrix = matrix(1)) # all weighted equally
# equivalent:
#get_metrics_with_dist(tt, fn=cluster::daisy, metric="gower", stand=TRUE, weights=rep(1, ncol(tt$train)-1)) # -1 bc target gets removed



# try optim using optim() ?!




#### roc curves for binary outcomes (not_fully_paid) ####
# to compare models
# NEED PROBABILITIES
# --> calc dist matrices, assign neighbor's target, 
# set p_yes to similarity (1-diss) if neighbor target is 1, or diss if neighbor is 0

#get dist
#add nn, new: add p var==categ
#get metrics: roc curve (start with new fn then combine?)

roc <- list()
roc$gower <- get_roc_curves_for_dist(tt, cluster::daisy, metric="gower", stand=TRUE)
roc$gower_best <- get_roc_curves_for_dist(tt, cluster::daisy, metric="gower", stand=TRUE, weights=weights_max)
roc$gower_manual <- get_roc_curves_for_dist(tt, cluster::daisy, metric="gower", stand=TRUE, weights=weights_manual)

# make factor into dummy for other distances (e.g. euclidian for kNN)
if(!is.factor(tt$train[,v_target])) tt_num <- lapply(tt, make_factors_into_dummies) else tt_num <- tt
roc$euclidian <- get_roc_curves_for_dist(tt_num, fn=stats::dist, method="euclidian")
roc$maximum <- get_roc_curves_for_dist(tt_num, fn=stats::dist, method="maximum")
roc$manhattan <- get_roc_curves_for_dist(tt_num, fn=stats::dist, method="manhattan")
roc$canberra <- get_roc_curves_for_dist(tt_num, fn=stats::dist, method="canberra")
roc$binary <- get_roc_curves_for_dist(tt_num, fn=stats::dist, method="binary")
roc$minkowski <- get_roc_curves_for_dist(tt_num, fn=stats::dist, method="minkowski")




#### roc curves to plot in single graph ####

roc <- list()
roc$gower <- get_metrics_with_dist(tt, cluster::daisy, metric="gower", stand=TRUE, eval_fn = yardstick::roc_curve)
roc$gower_best <- get_metrics_with_dist(tt, cluster::daisy, metric="gower", stand=TRUE, weights=weights_max, eval_fn = yardstick::roc_curve)
roc$gower_manual <- get_metrics_with_dist(tt, cluster::daisy, metric="gower", stand=TRUE, weights=weights_manual, eval_fn = yardstick::roc_curve)

# make factor into dummy for other distances (e.g. euclidian for kNN)
if(!is.factor(tt$train[,v_target])) tt_num <- lapply(tt, make_factors_into_dummies) else tt_num <- tt
roc$euclidian <- get_metrics_with_dist(tt_num, fn=stats::dist, method="euclidian", eval_fn = yardstick::roc_curve)
roc$maximum <- get_metrics_with_dist(tt_num, fn=stats::dist, method="maximum", eval_fn = yardstick::roc_curve)
roc$manhattan <- get_metrics_with_dist(tt_num, fn=stats::dist, method="manhattan", eval_fn = yardstick::roc_curve)
roc$canberra <- get_metrics_with_dist(tt_num, fn=stats::dist, method="canberra", eval_fn = yardstick::roc_curve)
roc$binary <- get_metrics_with_dist(tt_num, fn=stats::dist, method="binary", eval_fn = yardstick::roc_curve)
roc$minkowski <- get_metrics_with_dist(tt_num, fn=stats::dist, method="minkowski", eval_fn = yardstick::roc_curve)

# plot
roc <- make_list_of_roc_curves_into_single_table(roc)
get_roc_curves_in_same_plot(roc)
get_roc_curves_in_same_plot(roc %>% filter(model %in% c("gower", "gower_best")))







#### numerical distances ####

# make factor into dummy for other distances (e.g. euclidian for kNN)
if(!is.factor(tt$train[,v_target])) tt_num <- lapply(tt, make_factors_into_dummies) else tt_num <- tt
get_metrics_with_dist(tt_num, fn=stats::dist, method="euclidian")
get_metrics_with_dist(tt_num, fn=stats::dist, method="maximum")
get_metrics_with_dist(tt_num, fn=stats::dist, method="manhattan")
get_metrics_with_dist(tt_num, fn=stats::dist, method="canberra")
get_metrics_with_dist(tt_num, fn=stats::dist, method="binary")
get_metrics_with_dist(tt_num, fn=stats::dist, method="minkowski")
#lapply(get_metrics_with_dist(tt, fn=stats::dist, method="euclidian"), summary)



get_metrics_with_dist(tt_num, fn=stats::dist, method="euclidian", eval_fn=yardstick::conf_mat)





#### as feature(s) in random forest (i.e. feature engineering) ####




