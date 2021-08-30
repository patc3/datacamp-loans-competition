"
2021.08.26
DataCamp competition

loans pip

"
source("loans_fn.R")
df <- load_data(as_df = TRUE)
str(df)
summary(df)

# could do gower and predict, but there's neither missing data and almost all vars are numeric!
# could do pipelines
# could do both

sapply(df, \(x)if(is.factor(x)) levels(x))


#### compare nearest-neighbors methods ####
# data prep
df$id <- 1:nrow(df)
df <- cast(df, type_from="character", type_to="factor") # purpose is a factor

# tt
tt <- ttsplit(df, .7)
tt <- replace_target_in_test_set_with_missing_and_add_ref_table_to_environment(tt, target = "not_fully_paid", unique_id = "id", ref_name = "ref_target")



tt <- add_target_back_to_test_set_from_ref_table(tt, ref_target)