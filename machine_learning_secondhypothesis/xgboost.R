library(caret)
library(xgboost)
library(mice)
library(tidyverse)


readr::read_csv(file = "~/Documents/Thesis/Data/Properties/properties.csv") -> properties
na.omit(properties) -> properties

response <- properties$Type
independent <- properties%>%select(!c("Type"))
independent <- fastDummies::dummy_cols(independent)

cbind(independent,response) -> properties
properties <- properties%>%select(!c("Number_of_Components"))



properties_split <- rsample::initial_split(properties)
properties_train <- rsample::training(properties_split)
properties_test <- rsample::testing(properties_split)
