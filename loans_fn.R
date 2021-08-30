"
2021.08.26
DataCamp competition

loans fn

"
#### imports ####
library(dplyr)
library(gower)
#library(cluster)

#### load data ####
load_data <- function(as_df=FALSE)
{
  df <- readr::read_csv("data/loans.csv.gz")
  if(as_df) class(df) <- "data.frame"
  return(df)
}

#### data prep ####
# make type into factors
cast <- function(df, type_from, type_to)
{
  v <- sapply(df, \(x) inherits(x, type_from))
  v <- names(v)[which(v)]
  for(c in v) df[,c] <- do.call(paste0("as.", type_to), list(df[,c]))
  
  # out
  print(paste0("Made these variables from ", type_from, " to ", type_to, ":"))
  print(v)
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

# prevent target leakage
replace_target_in_test_set_with_missing_and_add_ref_table_to_environment <- function(df, target, unique_id, ref_name)
{
  "
  input: df is train-test list; target and unique_id are names of target and unique ID vars; ref_name is name of new object that stores mapping ID and target
  output: df list with target replaced with NA in test, and new object ref_name in environment
  "
  ref_table <- data.frame(id=df$test[,unique_id], target=df$test[,target])
  colnames(ref_table)[1] <- unique_id # same as test set
  colnames(ref_table)[2] <- target # same as test set
  df$test[,target] <- NA
  print(paste0("Target column ", target, " replaced with NA in test set"))
  assign(ref_name, value=ref_table, envir = .GlobalEnv)
  print(paste0("Added reference table '", ref_name, "' to global environment"))
  return(df)
}

# add actual target back to test set
add_target_back_to_test_set_from_ref_table <- function(df, ref_table)
{
  "
  input: df is train-test list; ref_table is reference table with id and target column created with replace_target_in_test...()
  output: df list with target in train replaced with actual values
  "
  id_var <- names(ref_table)[1]
  target_var <- names(ref_table)[2]
  df$test[,target_var] <- ref_table[,target_var][match(x=df$test[,id_var], table=ref_table[,id_var])]
  print(paste0("Replaced values in target '", target_var, "' with values from reference table"))
  return(df)
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











