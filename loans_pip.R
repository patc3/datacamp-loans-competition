"
2021.08.26
DataCamp competition

loans pip

"
rm(list=ls())
v_target <- c("not_fully_paid", "purpose")[1]
source("loans_fn.R")
df <- load_data(as_df = TRUE)
df <- df %>% slice_sample(n=1000)
str(df)
summary(df)

# could do gower and predict, but there's neither missing data and almost all vars are numeric!
# could do pipelines
# could do both


#### compare nearest-neighbors methods ####
# data prep
df <- cast(df, type_from="character", type_to="factor") # purpose is a factor

# tt
tt <- ttsplit(df, .7)
# tv (to choose gower weights)
tv <- ttsplit(tt$train, .7)

# scale
tt <- scale_numeric_features_in_train_and_test(tt)
tv <- scale_numeric_features_in_train_and_test(tv)

#### gower with daisy: optimize gower weights ####

# loop to optimize weights
weights <- get_gower_weights(tv, min_vars = 3, n_combinations = 1000)
metrics <- get_gower_metrics_for_weights(tv, weights_matrix = weights)


# post
acc <- sapply(metrics, \(m) m$test %>% filter(.metric=="accuracy") %>% pull(.estimate))
acc[which(sapply(metrics, \(m) any(is.na(m$test$.estimate))))] <- NA # remove only one prediction


# avg train and test because all high values in test seem more normal in train
acc_tt <- sapply(metrics, \(m) mean(sapply(m, \(tbl) tbl %>% filter(.metric=="accuracy") %>% pull(.estimate))))
acc_tt[which(sapply(metrics, \(m) any(sapply(m, \(tbl) any(is.na(tbl$.estimate))))))] <- NA # remove only one prediction
summary(acc_tt)
hist(acc_tt)
sapply(weights[which(acc_tt>.78),], sum) # to find which vars are most and least common in this good lot



# need to compare to actual test set when using a set to choose weights
# ie should do tv (train valid) then tt
# choose weights and run with tt to get test metrics
weights_max <- weights[which.max(acc_tt),]
get_gower_metrics_for_weights(tt, weights_matrix = weights_max)
get_gower_metrics_for_weights(tt, weights_matrix = matrix(1)) # all weighted equally
# equivalent:
#get_metrics_with_dist(tt, fn=cluster::daisy, metric="gower", stand=TRUE, weights=rep(1, ncol(tt$train)-1)) # -1 bc target gets removed



#### roc curves for binary outcomes (not_fully_paid) ####
# to compare models







#### cluster::gower ####
# gower
tt_gower <- add_neighbor_target_gower(tt)
#lapply(tt_gower, \(df) xtabs(~nn_gower + get(v_target), data=df)/nrow(df))
#lapply(tt_gower, \(df) df %>% filter(get(v_target)==1) %>% summarise(metric=sum(nn_gower==1)/n())) # true pos; sensitivity

# need confusion matrix here
get_metrics(tt_gower, nn_var = "nn_gower")



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








