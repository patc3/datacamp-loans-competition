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
weights <- get_gower_weights(tv, min_vars = 2, n_combinations = 1000)
metrics <- get_gower_metrics_for_weights(tv, weights_matrix = weights)
weights_max <- get_gower_best_weights(weights, metrics)


get_gower_metrics_for_weights(tt, weights_matrix = weights_max)
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

dist<-get_dist(tt, cluster::daisy, metric="gower", stand=TRUE)
dist<-get_dist(tt, cluster::daisy, metric="gower", stand=TRUE, weights=weights_max)
p_fn <- list(
  NL=\(d)normalize(log(d)),
  N=\(d)normalize(d),
  LL=\(d)logistic(log(d)),
  L=\(d)logistic(d)
  )
roc_list<-list()
for(fn_name in names(p_fn))
{
  tt_p <- add_neighbor_target_from_dist_matrix(tt = tt, dist = dist, 
                                               p_add = TRUE, p_fn = p_fn[[fn_name]])
  lapply(tt_p, \(df)hist(df$nn_p, main=fn_name))
  x<-tt_p$test$not_fully_paid
  tt_p$test$not_fully_paid[which(x==min(x))] <- 0
  tt_p$test$not_fully_paid[which(x==max(x))] <- 1
  tt_p$test$not_fully_paid <- factor(tt_p$test$not_fully_paid, levels=c("1","0"), labels=c("Yes", "No"))
  roc_list[[fn_name]] <- roc_curve(tt_p$test, truth=not_fully_paid, estimate=nn_p)
  #print(autoplot(roc))
  #dev.new()
}
lapply(seq_along(roc_list), \(i) autoplot(roc_list[[i]]) + ggtitle(names(roc_list)[i]))







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






