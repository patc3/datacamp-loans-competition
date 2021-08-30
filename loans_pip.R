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

# gower
tt <- add_neighbor_target_gower(tt)
lapply(tt, \(df) xtabs(~nn_gower + get(v_target), data=df)/nrow(df))
lapply(tt, \(df) df %>% filter(get(v_target)==1) %>% summarise(metric=sum(nn_gower==1)/n())) # true pos; sensitivity

# need confusion matrix here
conf_mat(tt$train %>% cast("numeric", "factor"), truth=v_target, estimate="nn_gower")
conf_mat(tt$test %>% cast("numeric", "factor"), truth=v_target, estimate="nn_gower")


# daisy gower
tt <- add_neighbor_target_from_dist_matrix(tt)
conf_mat(tt$train %>% cast("numeric", "factor"), truth=v_target, estimate="nn")
conf_mat(tt$test %>% cast("numeric", "factor"), truth=v_target, estimate="nn")

