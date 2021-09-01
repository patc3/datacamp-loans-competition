df<-data.frame(
  y=factor(c("A", "A", "B", "B", "B", "B")),
  pred=factor(c("A", "B", "A", "B", "B", "B")),
  prob=c(.88,.65,.7,.22,.44,.13)
)

library(tidymodels)

target<-"y"
prediction<-"pred"
proba<-"prob"

yardstick::conf_mat(df, truth=target, estimate=prediction)
yardstick::roc_auc(df, truth=!!target, estimate=!!proba)

# roc_auc: 
#   estimate can be !! or nothing, but not get
#   target can be !! or get, but not nothing
# conf_mat: 
#   everything works


