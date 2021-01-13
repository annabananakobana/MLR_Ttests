#R's t-test is Welch's by default, 
#therefore call var.equal=TRUE for students t-test

#load libraries

library(tidyverse)
library(dplyr)
library(magrittr)
library(readr)

#read .csv file format

spreadsheet_subset <- read_csv("~/OneDrive/Documents/5th_year/Masters/Research/MLR/spreadsheet_subset2.csv") 
male_data <- read_csv("~/OneDrive/Documents/5th_year/Masters/Research/Results/spreadsheet_subset_male.csv")

#assign the variables, note "1" refers to WT, "2" refers to heterozygous groups 
#sample sizes 

n1<-c("n1")
n2<-c("n2")
n3<-c("n3")

#mean litter size

m1<-c("m1")
m2<-c("m2")
m3<-c("n3")
#standard deviation 

s1<-c("s1")
s2<-c("s2")
s3<-c("s3")

#make each colum numeric
spreadsheet_subset %<>% mutate_if(is.character,as.numeric)
male_data %<>% mutate_if(is.character,as.numeric)

#check the structure
spreadsheet_subset %>% str()
male_data %>% str()

#t-test function for WT vs KO, Student's if variance is equal, Welch if not 

t.test_WTvsKO <- function(m1,m3,s1,s3,n1,n3, equal.variance=FALSE) 
  {
  #Welch
  if( equal.variance==FALSE ) {
    se <- sqrt( (s1^2/n1) + (s3^2/n3) )
    df <- ( (s1^2/n1 + s3^2/n3)^2 )/( (s1^2/n1)^2/(n1-1) + (s3^2/n3)^2/(n3-1) )
  } else {
    #Students
    se <- sqrt( (1/n1 + 1/n3) * ((n1-1)*s1^2 + (n3-1)*s3^2)/(n1+n3-2) ) 
    df <- n1+n3-2
  }
  t <- (m1-m3)/se 
  return(2*pt(-abs(t),df))
    }

#t-test function for WT vs het, Student's if variance is equal, Welch if not 

t.test_WTvshet <- function(m1,m2,s1,s2,n1,n2, equal.variance=FALSE) 
{
  #Welch
  if( equal.variance==FALSE ) {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else {
    #Students
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
    df <- n1+n2-2
  }
  t <- (m1-m2)/se 
  return(2*pt(-abs(t),df))
}

#t-test function for het vs KO, Student's if variance is equal, Welch if not 

t.test_hetvsKO <- function(m2,m3,s2,s3,n2,n3, equal.variance=FALSE) 
{
  #Welch
  if( equal.variance==FALSE ) {
    se <- sqrt( (s2^2/n2) + (s3^2/n3) )
    df <- ( (s2^2/n2 + s3^2/n3)^2 )/( (s2^2/n2)^2/(n2-1) + (s3^2/n3)^2/(n3-1) )
  } else {
    #Students
    se <- sqrt( (1/n2 + 1/n3) * ((n2-1)*s2^2 + (n3-1)*s3^2)/(n2+n3-2) ) 
    df <- n2+n3-2
  }
  t <- (m2-m3)/se 
  return(2*pt(-abs(t),df))
}

#extract a subset of the spreadsheet so we can avoid NAs
#female WT vs KO
a = spreadsheet_subset  %>%
  filter(!is.na(s1), !is.na(s3), !is.na(m1), !is.na(m3), !is.na(n1), !is.na(n3))

#female WT vs het
b = spreadsheet_subset  %>%
  filter(!is.na(s1), !is.na(s2), !is.na(m1), !is.na(m2), !is.na(n1), !is.na(n2))

#female het vs KO
c = spreadsheet_subset  %>%
  filter(!is.na(s2), !is.na(s3), !is.na(m2), !is.na(m3), !is.na(n2), !is.na(n3))

#male WT vs KO
d = male_data  %>%
  filter(!is.na(s1), !is.na(s3), !is.na(m1), !is.na(m3), !is.na(n1), !is.na(n3))

#male WT vs het
e = male_data  %>%
  filter(!is.na(s1), !is.na(s2), !is.na(m1), !is.na(m2), !is.na(n1), !is.na(n2))

#male het vs KO
f = male_data  %>%
  filter(!is.na(s2), !is.na(s3), !is.na(m2), !is.na(m3), !is.na(n2), !is.na(n3))

#finally, run the t-test!
#female WT vs KO
t.test_WTvsKO(a$m1, a$m3, a$s1, a$s3, a$n1, a$n3) 

#female WT vs het
t.test_WTvshet(b$m1, b$m2, b$s1, b$s2, b$n1, b$n2)

#female het vs KO
t.test_hetvsKO(c$m2, c$m3, c$s2, c$s3, c$n2, c$n3)

#male WT vs KO
t.test_WTvsKO(d$m1,d$m3, d$s1, d$s3, d$n1, d$n3)

#male WT vs het
t.test_WTvshet(e$m1, e$m2, e$s1, e$s2, e$n1, e$n2)

#male het vs KO
t.test_hetvsKO(f$m2, f$m3, f$s2, f$s3, f$n2, f$n3)

  