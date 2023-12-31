rm(list = ls())
cat("\014")
set.seed(1)

install.packages("FrF2")
library(FrF2)

features = c(
  "halfacre",
  "masteronmain",
  "newkitchen",
  "GoogleFiber",
  "largeclosets",
  "historical",
  "culdesac",
  "openfloorplan",
  "goodschools",
  "fencedinyard"
)

FrF2(16, factor.names = features)