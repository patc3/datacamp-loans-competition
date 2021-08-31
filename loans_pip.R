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

# scale
tt <- scale_numeric_features_in_train_and_test(tt)


#### gower with daisy: optimize gower weights ####
# get dist
# daisy gower

get_metrics_with_dist(tt, fn=cluster::daisy, metric="gower", stand=TRUE, weights=rep(1, ncol(tt$train)-1)) # -1 bc target gets removed


# loop to optimize weights
metrics <- get_gower_metrics_for_weights(tt)


# post
acc <- sapply(metrics, \(m) m %>% filter(.metric=="accuracy") %>% pull(.estimate))
acc[which(sapply(metrics, \(m) any(is.na(m$.estimate))))] <- NA # remove only one prediction
hist(acc)
weights <- get_gower_metrics_for_weights(tt, return_weights=TRUE)
weights[which(acc>.84),]
weights[which(acc<.71),]
metrics[which(acc>.84)]



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
if(!is.factor(tt$train[,v_target])) tt <- lapply(tt, make_factors_into_dummies)
get_metrics_with_dist(tt, fn=stats::dist, method="euclidian")
get_metrics_with_dist(tt, fn=stats::dist, method="maximum")
get_metrics_with_dist(tt, fn=stats::dist, method="manhattan")
get_metrics_with_dist(tt, fn=stats::dist, method="canberra")
get_metrics_with_dist(tt, fn=stats::dist, method="binary")
get_metrics_with_dist(tt, fn=stats::dist, method="minkowski")
#lapply(get_metrics_with_dist(tt, fn=stats::dist, method="euclidian"), summary)








