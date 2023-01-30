library(tidyverse)
library(dplyr)
my_mean = function(x){
  Sum = sum(x)
  N = length(x)
  return(Sum/N)
}

myVec = 1:7

my_mean(myVec)

prac_function = function(x){
  # Three Steps
  # 1. compute the range of vector x
  x_range = range(x)
  # 2. add 100 & 3. divide by 10
  return((x_range + 100)/10)
}

prac_function(myVec)

weather = read.csv('weather-1.csv')

weather_head = head(weather, 2)

select(weather_head, starts_with("d"))

mpg1 = select(mpg, cty, hwy)

head(mutate(mpg1, cty = cty * 1.6/3.8, hwy = hwy * 1.6/3.8), 5)

filter(mpg, cyl == 4)

admit_df = read_csv("LogReg-1.csv")
str(admit_df)

admit_df$rank = as.factor(admit_df$rank)

names = c("admit", "rank")
admit_df[,names] = lapply(admit_df[,names], factor)

(as.character(x = lapply(subset(admit_df, select = names), class)))

# Question : What is the average GPA of those admitted by school rank?
avgGPA_admitted = function(x){
  # 3 Steps
  # 1. group_by rank
  df_1 = group_by(x, as.factor(rank))
  # 2. Filter by 1, to get only those admitted
  df_2 = filter(df_1, x == 1)
  # 3. Find Average GPA
  df_3 = lapply(df_2$gpa, mean)
}

avgGPA_admitted(admit_df)
