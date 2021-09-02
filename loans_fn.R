"
2021.08.26
DataCamp competition

loans fn

"
#### imports ####
library(dplyr)
library(cluster)
library(tidymodels)
library(vip)
req_pckgs <- c("fastDummies", "dataPreparation")
req_pckgs <- req_pckgs[which(!req_pckgs %in% rownames(installed.packages()))]
if(length(req_pckgs) != 0) install.packages(req_pckgs)

#### load data ####
load_data <- function(as_df=FALSE)
{
  df <- readr::read_csv("data/loans.csv.gz")
  if(as_df) class(df) <- "data.frame"
  return(df)
}

#### data prep ####
# make type_from variables into type_to
cast <- function(df, type_from, type_to)
{
  "
  Ex.: cast(df, 'numeric', 'factor') will make numeric variables in to factors
  
  "
  v <- sapply(df, \(x) inherits(x, type_from))
  v <- names(v)[which(v)]
  for(c in v) df[,c] <- do.call(paste0("as.", type_to), list(df[,c]))
  
  # out
  print(paste0("Made these variables from ", type_from, " to ", type_to, ":"))
  print(v)
  return(df)
}

# dummies
make_factors_into_dummies <- function(df, remove_first_dummy = TRUE, ignore_na = TRUE)
{
  df <- fastDummies::dummy_cols(df, remove_first_dummy = remove_first_dummy, ignore_na = ignore_na, remove_selected_columns = TRUE)
  
  # out
  print("Used fastDummies to make factors into dummies")
  return(df)
}


# normalize (range 0-1)
normalize <- function(x, replace_negative_Inf=TRUE) 
{
  if(replace_negative_Inf & -Inf %in% x)
  {
    ix <- which(x==-Inf)
    x[ix] <- NA
    x[ix] <- min(x, na.rm=TRUE)
  }
  (x-min(x, na.rm=TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))
}


# logistic
logistic <- function(x) {1/(1+exp(-x))}



#### train test ####
# ttsplit
ttsplit <- function(df, prop_train=.7)
{
  df_split <- list()
  ix <- sample(nrow(df), round(nrow(df)*prop_train), replace=F)
  df_split$train <- df[ix,]
  df_split$test <- df[-ix,]
  print(paste0("Split df into list with: train, test (proportion train = ", prop_train, ")"))
  return(df_split)
}


# scale numeric vars
scale_numeric_features_in_train_and_test <- function(tt)
{
  # scale numeric features train & test
  v_num <- colnames(tt$train)[which(sapply(tt$train, class) %in% c("numeric", "integer"))]
  scales <- dataPreparation::build_scales(data_set = tt$train, cols=v_num, verbose=TRUE)
  tt$train <- dataPreparation::fast_scale(data_set = tt$train, scales = scales, verbose = TRUE) %>% as.data.frame
  
  # test
  tt$test <- dataPreparation::fast_scale(data_set = tt$test, scales=scales, verbose=TRUE) %>% as.data.frame
  
  # out
  print("Created numerical scales using train set and rescaled numerical features in train and test")
  return(tt)
}



#### nearest neighbors ####

# generic fn to get distance matrix from selected fn
get_dist <- function(tt, fn, ...)
{
  # remove target
  tt_fv <- lapply(tt, \(df) {df[,v_target] <- NULL; return(df)})
  
  # get dist
  dist <- fn(do.call(rbind, tt_fv), ...)
  #dist <- daisy(do.call(rbind, tt_fv), metric="gower", stand=TRUE, weights=rep(1, ncol(tt_fv[[1]])))
  
  # out
  return(dist)
}


add_neighbor_target_from_dist_matrix <- function(tt, dist, p_add=TRUE, p_fn=normalize)
{
  "
  input:  tt is train-test list
          dist is distance or dissimilarity matrix
          p_add is whether to add probability (for ROC)
          p_fn is function to convert dist to probabilities (needs to be monotonic)
  output: df with neighbor added
  "
  # create the dist matrix
  dist <- as.matrix(dist)
  
  # remove (i,i) entries to remove self selection
  diag(dist) <- NA
  
  # transform to probabilities (no attempt to calibrate)
  if(p_add) dist <- p_fn(dist) # could take log as well; also logistic() with or without log
  
  # limit choice to train obs
  n_train <- nrow(tt$train)
  n_test <- nrow(tt$test) 
  dist <- dist[,1:n_train] # remove test columns
  
  # get index in train set
  ix <- apply(dist, 1, which.min)
  ix_train <- ix[1:n_train]
  ix_test <- ix[(n_train+1):length(ix)]
  
  # add target from train
  col <- "nn"#paste0("nn_", round(runif(n=1, min=9999, max=99999999), 0))
  tt$train[,col] <- tt$train[ix_train,v_target]
  tt$test[,col] <- tt$train[ix_test,v_target]
  
  # add p
  if(p_add)
  {
    p_col <- "nn_p"
    #set p_yes to similarity (1-diss) if neighbor target is 1, or diss if neighbor is 0
    # ???????
    min_dists <- sapply(seq_along(rownames(dist)), \(i) dist[i, ix[i]])
    ix_yes <- which(do.call(rbind, tt)[,col] %in% c(1, "1", max(tt$train[,v_target]))) # max() is because target is standardized
    min_dists[ix_yes] <- 1-min_dists[ix_yes]
    # add
    tt$train[,p_col] <- min_dists[1:n_train]
    tt$test[,p_col] <- min_dists[(n_train+1):length(min_dists)]
  }
  
  # out
  print(paste("Added column(s) to train and test:", col, if(p_add) p_col else NULL))
  return(tt)
}


# get metrics
get_metrics <- function(tt, eval_fn=yardstick::conf_mat, ...)
{
  "
  ... to be passed to conf_mat
  "
  
  # eval fn & estimate var
  if(is.null(eval_fn)) eval_fn <- yardstick::conf_mat
  if(isTRUE(all.equal(eval_fn, yardstick::conf_mat))) nn_var <- "nn" else nn_var <- "nn_p"
  
  
  # make target and nn factors (conf_mat only takes factors)
  tt <- lapply(tt, cast, "numeric", "factor")
  
  # top level comes first (for AUC)
  tt <- lapply(tt, \(df) { df[,v_target] <- factor(df[,v_target], levels=sort(levels(df[,v_target]), decreasing = TRUE)) ; df } )
  #tt <- lapply(tt, \(df) { df[,v_target] <- factor(df[,v_target], labels=paste0("Cat",0:(length(unique(df[,v_target]))-1))); df } )
  # tt_p$test$not_fully_paid[which(x==min(x))] <- 0
  # tt_p$test$not_fully_paid[which(x==max(x))] <- 1
  # tt_p$test$not_fully_paid <- factor(tt_p$test$not_fully_paid, levels=c("1","0"), labels=c("Yes", "No")) # ensure first level is positive class
  
  # make sure nn and v_target have the same factor levels
  if(nn_var=="nn") tt <- lapply(tt, \(df){ df[,nn_var] <- factor(df[,nn_var], levels=levels(tt$train[,v_target])); df })
  
    # nn_p is numeric
  if(nn_var=="nn_p") tt <- lapply(tt, \(df) { df[,nn_var] <- as.numeric(as.character(df[,nn_var])) ; df } ) # sloppy: goes from num to factor to num

  
  # change to string levels
  #tt <- lapply(tt, \(df) { df[,v_target] <- factor(df[,v_target], labels=paste0("Cat",0:(length(unique(df[,v_target]))-1))); df } )
  #if(nn_var=="nn") tt <- lapply(tt, \(df) { df[,nn_var] <- factor(df[,nn_var], labels=paste0("Cat",0:(length(unique(df[,v_target]))-1))); df } ) # labels might be reversed :( sloppy, but will show in AUC<<.5
  
  
  # get metrics
  metrics <- list(
    train=eval_fn(tt$train, truth=!!v_target, estimate=!!nn_var),
    test=eval_fn(tt$test, truth=!!v_target, estimate=!!nn_var)
  )
  
  # print summary
  print(lapply(metrics, summary))
  
  # out
  return(metrics)
}


# pipeline to get metrics from tt and dist function
get_metrics_with_dist <- function(tt, fn=NULL, ..., eval_fn=NULL)
{
  dist <- get_dist(tt, fn=fn, ...)
  tt <- add_neighbor_target_from_dist_matrix(tt, dist)
  
  # metrics
  get_metrics(tt, eval_fn)
  
}


# get gower weights
get_gower_weights <- function(tt, min_vars=1, n_combinations=NULL)
{
  
  # arg check
  if(min_vars<1) stop("min_var needs to be >= 1")
  
  # define weight matrix
  weights <- list()
  for (i in 1:(ncol(tt$train)-1)) weights[[i]] <- c(0,1)
  weights <- expand.grid(weights)
  colnames(weights) <- (.n <- colnames(tt$train))[.n != v_target]
  
  # filtering
  weights <- weights[rowSums(weights) >= min_vars,] # remove row where all weights are 0
  if(!is.null(n_combinations)) weights <- weights %>% slice_sample(n=min(nrow(.), n_combinations))
  
  # out
  return(weights)
}


# optimize weights in gower
get_gower_metrics_for_weights <- function(tt, weights_matrix=NULL, eval_fn=yardstick::conf_mat)
{
  # define weight matrix
  weights <- if(is.null(weights_matrix)) get_gower_weights(tt) else weights_matrix
  
  # get metrics for all weights
  metrics <- list()
  for (i in 1:nrow(weights)) 
  {
    sink("NUL")
    m <- get_metrics_with_dist(tt, fn=cluster::daisy, metric="gower", stand=TRUE, weights=weights[i,], eval_fn=eval_fn)
    metrics[[i]] <- if(isTRUE(all.equal(eval_fn, yardstick::conf_mat))) lapply(m, summary) else m
    sink()
    #if(i%%100==0) print(round(i/nrow(weights)*100))
    print(i)
  }
  
  # out
  return(metrics)
}


# retrieve best weights for gower
get_gower_best_weights <- function(weights, metrics, choose_by=c("accuracy", "mean_metric", "auc"))
{
  "
  input:  weights used to get metrics list (from get_gower_weights())
          metrics is metrics list (from get_gower_metrics_for_weights())
  output: matrix with 1 row (best weights)
  "
  # post: test only
  #acc <- sapply(metrics, \(m) m$test %>% filter(.metric=="accuracy") %>% pull(.estimate))
  #acc[which(sapply(metrics, \(m) any(is.na(m$test$.estimate))))] <- NA # remove only one prediction
  
  if (choose_by == "accuracy")
  {
    # avg train and test because all high values in test seem more normal in train
    acc_tt <- sapply(metrics, \(m) mean(sapply(m, \(tbl) tbl %>% filter(.metric=="accuracy") %>% pull(.estimate))))
    acc_tt[which(sapply(metrics, \(m) any(sapply(m, \(tbl) any(is.na(tbl$.estimate))))))] <- NA # remove only one prediction
    #summary(acc_tt)
    #hist(acc_tt)
    #sapply(weights[which(acc_tt>.78),], sum) # to find which vars are most and least common in this good lot
  }
  
  if (choose_by == "mean_metric")
  {
    acc_tt <- sapply(metrics, \(m)sapply(m, \(tbl) mean(tbl$.estimate)))
    acc_tt <- colMeans(acc_tt)
    acc_tt[which(sapply(metrics, \(m) any(sapply(m, \(tbl) any(tbl$.estimate < 0)))))] <- NA # remove any weights combination that yield negative metrics estimates
  }
  
  if (choose_by == "auc")
  {
    acc_tt <- sapply(metrics, \(m)sapply(m, \(tbl) tbl %>% pull(.estimate)))
    acc_tt <- colMeans(acc_tt)
  }
  
  # need to compare to actual test set when using a set to choose weights
  # ie should do tv (train valid) then tt
  # choose weights and run with tt to get test metrics
  weights_max <- weights[which.max(acc_tt),]
  # (credit_policy), purpose, pub_rec
  
  # out
  return(weights_max)
}



# get roc curves using same function call as get_dist()
get_roc_curves_for_dist <- function(..., plot_hist=FALSE)
{
  "
  input:  ... is fn call to get_dist()
  output: list of ggplots ROC curves (also prints histograms of probabilities)
  "
  
  # get dist
  dist <- get_dist(...)
  
  # functions to convert dist to probabilities
  p_fn <- list(
    NL=\(d)normalize(log(d)),
    N=\(d)normalize(d),
    LL=\(d)logistic(log(d)),
    L=\(d)logistic(d)
  )
  
  # get histograms of probabilities and roc curves
  p_hist <- list()
  roc_list<-list()
  roc_auc <- list()
  for(fn_name in names(p_fn))
  {
    tt_p <- add_neighbor_target_from_dist_matrix(tt = tt, dist = dist, 
                                                 p_add = TRUE, p_fn = p_fn[[fn_name]])
    if(plot_hist) p_hist[[fn_name]] <- lapply(tt_p, \(df)hist(df$nn_p, plot=F))
    x<-tt_p$test$not_fully_paid
    tt_p$test$not_fully_paid[which(x==min(x))] <- 0
    tt_p$test$not_fully_paid[which(x==max(x))] <- 1
    tt_p$test$not_fully_paid <- factor(tt_p$test$not_fully_paid, levels=c("1","0"), labels=c("Yes", "No")) # ensure first level is positive class
    roc_auc[[fn_name]] <- roc_auc(tt_p$test, truth=not_fully_paid, estimate=nn_p)
    roc_list[[fn_name]] <- roc_curve(tt_p$test, truth=not_fully_paid, estimate=nn_p)
    #print(autoplot(roc))
    #dev.new()
  }
  if(plot_hist) lapply(seq_along(p_hist), \(i) lapply(p_hist[[i]], \(h) plot(h, main=names(p_hist)[i])))
  roc <- lapply(seq_along(roc_list), \(i) print(autoplot(roc_list[[i]]) + ggtitle(  paste0( names(roc_list)[i], " (AUC = ", round(roc_auc[[i]]$.estimate, 2), ")" )  )))
  return(roc_auc)
}


# several roc curves in same graph
# need the table roc curve table
# x=1-specificity, y=sensitivity, color=model
# https://sydykova.com/post/2019-03-12-make-roc-curves-tidyverse/

make_list_of_roc_curves_into_single_table <- function(roc)
{
  "
  input:  roc list (one per distance)
  output: single table of roc curves with added model column
  "
  roc <- lapply(roc, \(l) l$test)  
  roc <- lapply(seq_along(roc), \(i) { roc[[i]]$model <- names(roc)[i]; roc[[i]] } ) %>% bind_rows()
  return(roc)
}

get_roc_curves_in_same_plot <- function(roc_tbl)
{
  "
  input:  roc_tbl is single table with yardstick::roc_curve and bind_rows() with added model column
  output: prints plot
  "
  roc_tbl %>% 
    ggplot(aes(x=1-specificity, y=sensitivity, color=model)) +
    geom_line(size=1.5) +
    geom_abline(slope = 1, intercept = 0, size = 0.4, linetype="dashed") +
    coord_fixed() + # fixed aspect ratio
    theme_gray(base_size=24)
}


# get roc curve and variable importance from random forest
get_rf_roc_curve <- function(tt)
{
  "
  input:  tt is train-test list
  output: list with variable importance ($var_imp) and roc curve ($roc)
  "
  
  # make levels TRUE or FALSE
  tt <- lapply(tt, \(df) { df[,v_target] <- factor(df[,v_target], levels=sort(unique(df[,v_target]), decreasing = TRUE)) ; levels(df[,v_target]) <- c("TRUE", "FALSE") ; df } )
  
  # Random Forest with var imp. and ROC curve
  rand_forest() %>% 
    set_mode("classification") %>%
    set_engine("randomForest") %>% 
    fit(factor(not_fully_paid) ~ ., tt$train) %T>%
    { vip(.) ->> var_imp } %>% # save variable importance
    predict(tt$test, type="prob") %>% 
    bind_cols(tt$test) %>% 
    roc_curve(truth=not_fully_paid, estimate=.pred_TRUE) -> roc
  
  # out
  return(list(var_imp=var_imp, roc=roc))
}
