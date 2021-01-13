#load packages 
library(readr)

#read in data
juno_litters <- read_csv("juno_litters.csv")
izumo1_litters <- read_csv("izumo1_litters.csv")

#multiple linear regression with interaction analysis
MLR_juno=lm(litter_total~sex*genotype,data=juno_litters)
summary(MLR_juno)

MLR_izumo1=lm(litter_total~sex*genotype,data=izumo1_litters)
summary(MLR_izumo1)
