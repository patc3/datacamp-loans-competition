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
