"
2021.08.26
DataCamp competition

loans fn

"

#### load data ####
load_data <- function(as_df=FALSE)
{
  df <- readr::read_csv("data/loans.csv.gz")
  if(as_df) class(df) <- "data.frame"
  return(df)
}

