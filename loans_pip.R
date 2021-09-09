"
2021.08.26
DataCamp competition

loans pip

"

#### load & inspect data ####
rm(list=ls())
v_target <- "not_fully_paid" # set target
source("loans_fn.R") # load custom functions to use in pipeline
df <- load_data(as_df = TRUE)
df <- df %>% slice_sample(n=3000) # to reduce computations in Workspace
head(df)
psych::describe(df)


#### compare nearest-neighbors methods ####
# data prep
df <- cast(df, type_from="character", type_to="factor") # purpose is a factor

# tt: train-test split
tt <- ttsplit(df, prop = .7) # list with tt$train and tt$test
# tv: train-validation split (to choose gower weights)
tv <- ttsplit(tt$train, prop = .7)

# scale
tt <- scale_numeric_features_in_train_and_test(tt)
tv <- scale_numeric_features_in_train_and_test(tv)




#### gower with daisy: optimize gower weights ####

# loop to optimize weights
weights <- get_gower_weights(tv, min_vars = 3, n_combinations = 10) # generate possible combinations of 0-1 weights for each variable
metrics <- get_gower_metrics_for_weights(tv, weights_matrix = weights, eval_fn = yardstick::roc_auc) # compute AUC for each combination of weights
weights_max <- get_gower_best_weights(weights, metrics, choose_by=c("accuracy","mean_metric","auc")[3]) # extract best set of weights

# we could also manually choose our own weights (why not?)
weights_manual <- c(credit_policy=1, purpose=1, int_rate=0, installment=0, log_annual_inc=0, 
                    dti=0, fico=0, days_with_cr_line=0, revol_bal=0,
                    revol_util=0, inq_last_6mths=0, delinq_2yrs=0, pub_rec=1) %>% as.list %>% as.data.frame




#### compare nearest-neighbors methods with ROC curves ####
roc <- list()

# Gower distances
roc$Gower <- get_metrics_with_dist(tt, cluster::daisy, metric="gower", stand=TRUE, eval_fn = yardstick::roc_curve)
roc$`Gower (Optimized Weights)` <- get_metrics_with_dist(tt, cluster::daisy, metric="gower", stand=TRUE, weights=weights_max, eval_fn = yardstick::roc_curve)
roc$`Gower (Manual Weights)` <- get_metrics_with_dist(tt, cluster::daisy, metric="gower", stand=TRUE, weights=weights_manual, eval_fn = yardstick::roc_curve)

# make factor into dummy for numeric distances (e.g. euclidian for kNN)
if(!is.factor(tt$train[,v_target])) tt_num <- lapply(tt, make_factors_into_dummies) else tt_num <- tt

# numeric distances
roc$Euclidian <- get_metrics_with_dist(tt_num, fn=stats::dist, method="euclidian", eval_fn = yardstick::roc_curve)
roc$Maximum <- get_metrics_with_dist(tt_num, fn=stats::dist, method="maximum", eval_fn = yardstick::roc_curve)
roc$Manhattan <- get_metrics_with_dist(tt_num, fn=stats::dist, method="manhattan", eval_fn = yardstick::roc_curve)
roc$Canberra <- get_metrics_with_dist(tt_num, fn=stats::dist, method="canberra", eval_fn = yardstick::roc_curve)
roc$Binary <- get_metrics_with_dist(tt_num, fn=stats::dist, method="binary", eval_fn = yardstick::roc_curve)
roc$Minkowski <- get_metrics_with_dist(tt_num, fn=stats::dist, method="minkowski", eval_fn = yardstick::roc_curve)

# plot
roc <- make_list_of_roc_curves_into_single_table(roc)
get_roc_curves_in_same_plot(roc) # comparing all methods
get_roc_curves_in_same_plot(roc %>% filter(Model %in% c("Gower", "Gower (Optimized Weights)"))) # comparing Gower methods




#### Use nearest neighbor as feature(s) in random forest (i.e. feature engineering) ####
#### do ROC curve comparing w vs w/o additional features

# no neighbor
(rf_no_neighbor <- get_rf_roc_curve(tt))

# with gower
tt_gower <- add_neighbor_target_from_dist_matrix(
                        tt, 
                        dist = get_dist(tt, cluster::daisy, metric="gower", stand=TRUE, weights=weights_max), 
                        p_add = TRUE)
(rf_gower <- get_rf_roc_curve(tt_gower))

# compare
get_roc_curves_from_random_forests(list(`No Neighbor`=rf_no_neighbor, `Gower (Optimized Weights)`=rf_gower))




#### target as missing data ####
tt_imp <- add_mice_prediction(tt, m=20, maxit=5)
get_metrics(tt_imp)
get_metrics(tt_imp, eval_fn = roc_auc)
get_metrics(tt_imp, eval_fn = roc_curve)$test %>% autoplot()


