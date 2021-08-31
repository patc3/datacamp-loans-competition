"
2021.08.26
DataCamp competition

loans fn

"
#### imports ####
library(dplyr)
library(gower)
library(cluster)
library(tidymodels)
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

# add neighbor's target: gower
add_neighbor_target_gower <- function(tt)
{
  tt_gower <- lapply(tt, \(df) return(df[,!names(df) %in% c("id", v_target)]))
  tt_gower$topn <- list()
  tt_gower$nn_target <- list()

  # loop
  for (g in c("train", "test"))
  {
    print(g)
    neighbor <- c()
    nn_target <- tt[[g]][,v_target] # use actual target as placeholder to borrow the var's class (e.g. factor)
    g_train <- tt_gower[[g]]
    for (i in 1:nrow(g_train))
    {
      print(i)
      g_test <- tt_gower[["train"]]
      if(g=="train") g_test <- g_test[-i,]
      neighbor[i] <- gower_topn(x=g_train[i,], y=g_test, n=1, weights=rep(1, ncol(tt_gower$train)))$index[1,1] + ifelse(g=="train", 1, 0)
      nn_target[i] <- tt$train[,v_target][neighbor[i]]
    }
    tt_gower$topn[[g]] <- neighbor
    tt_gower$nn_target[[g]] <- nn_target
    
    # add to tt
    tt[[g]]$nn_gower <- tt_gower$nn_target[[g]]
  }
  
    
  # out
  print("Added variable nn_gower to train and test")
  return(tt)
}


add_neighbor_target_from_dist_matrix <- function(tt, dist)
{
  "
  input:  tt is train-test list
          dist is distance or dissimilarity matrix
  output: df with neighbor added
  "
  # create the dist matrix
  dist <- as.matrix(dist)
  
  # remove (i,i) entries to remove self selection
  diag(dist) <- Inf
  
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
  
  # out
  print(paste0("Added column to train and test: ", col))
  return(tt)
}

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




