"
2021.08.26
DataCamp competition

loans fn

"
#### imports ####
req_pckgs <- c("dplyr", "cluster", "tidymodels", "vip", "psych", "randomForest",  "fastDummies", "dataPreparation")
req_pckgs <- req_pckgs[which(!req_pckgs %in% rownames(installed.packages()))]
if(length(req_pckgs) != 0) install.packages(req_pckgs)
library(dplyr)
library(cluster)
library(tidymodels)
library(vip)
library(mice)

#### helper fn ####
view <- utils::View

# see https://stackoverflow.com/a/8189441/2303302 ; in case of tie this returns the first one
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

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
normalize <- function(x) (x-min(x, na.rm=TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))


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
scale_numeric_features_in_train_and_test <- function(tt, exclude_target=TRUE, verbose=FALSE)
{
  # scale numeric features train & test
  v_num <- colnames(tt$train)[which(sapply(tt$train, class) %in% c("numeric", "integer"))]
  if(exclude_target) v_num <- v_num[which(v_num != v_target)]
  scales <- dataPreparation::build_scales(data_set = tt$train, cols=v_num, verbose=verbose)
  tt$train <- dataPreparation::fast_scale(data_set = tt$train, scales = scales, verbose = verbose) %>% as.data.frame
  
  # test
  tt$test <- dataPreparation::fast_scale(data_set = tt$test, scales=scales, verbose=verbose) %>% as.data.frame
  
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

  # make sure nn and v_target have the same factor levels
  if(nn_var=="nn") tt <- lapply(tt, \(df){ df[,nn_var] <- factor(df[,nn_var], levels=levels(tt$train[,v_target])); df })
  
    # nn_p is numeric
  if(nn_var=="nn_p") tt <- lapply(tt, \(df) { df[,nn_var] <- as.numeric(as.character(df[,nn_var])) ; df } ) # sloppy: goes from num to factor to num
 
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
  if (choose_by == "accuracy")
  {
    # avg train and test because all high values in test seem more normal in train
    acc_tt <- sapply(metrics, \(m) mean(sapply(m, \(tbl) tbl %>% filter(.metric=="accuracy") %>% pull(.estimate))))
    acc_tt[which(sapply(metrics, \(m) any(sapply(m, \(tbl) any(is.na(tbl$.estimate))))))] <- NA # remove only one prediction
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
  
  # out
  return(weights_max)
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
  roc <- lapply(seq_along(roc), \(i) { roc[[i]]$Model <- names(roc)[i]; roc[[i]] } ) %>% bind_rows()
  return(roc)
}

get_roc_curves_in_same_plot <- function(roc_tbl)
{
  "
  input:  roc_tbl is single table with yardstick::roc_curve and bind_rows() with added model column
  output: prints plot
  "
  roc_tbl %>% 
    ggplot(aes(x=1-specificity, y=sensitivity, color=Model)) +
    geom_line(size=1.5) +
    geom_abline(slope = 1, intercept = 0, size = 0.4, linetype="dashed") +
    #coord_fixed() + # fixed aspect ratio
    theme_gray(base_size=18)
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
    bind_cols(tt$test) %T>% 
    { roc_curve(., truth=not_fully_paid, estimate=.pred_TRUE) ->> roc } %>% # save ROC curve
    roc_auc(truth=not_fully_paid, estimate=.pred_TRUE) -> auc # save AUC
  
  # out
  return(list(var_imp=var_imp, roc=roc, auc=auc))
}

# get plots for several random forests
get_roc_curves_from_random_forests <- function(rf_list)
{
  rf_list <- lapply(seq_along(rf_list), \(i) { rf_list[[i]]$roc$Model <- names(rf_list)[i]; rf_list[[i]]$roc } )
  rf_list %>% 
    bind_rows() %>% 
    get_roc_curves_in_same_plot()
}



#### target as missing data ####
add_mice_prediction <- function(tt, ...)
{
  .tt <- tt; .tt$test[,v_target] <- NA # to impute
  df <- do.call(rbind, .tt)
  imp <- mice(df, ...)
  pred <- partial(apply, X=imp$imp[[v_target]], MARGIN=1) #apply(, mode) # mean for AUC
  
  # add to test
  tt$test$nn <- pred(mode)
  tt$test$nn_p <- pred(mean)
  
  # add to train
  tt$train$nn <- tt$train$nn_p <- tt$train[,v_target]
  
  # out
  print("Added variables nn and nn_p from mode and mean imputed target value to test set")
  return(tt)
}
