rm(list = ls())

setwd("Y:/myBooks/quant/Statistics and Data Analysis for Financial Engineering")

library(tidyverse)
library(data.table)

list.files('./datasets')

dt <- fread('./datasets/Stock_Bond.csv')

dt

str(dt)
summary(dt)

par(mar = c(2,2,4,4), mfrow = c(1,2))
plot(dt$GM_AC)
plot(dt$F_AC)

GMRtn <- dt[-1,GM_AC] / dt[-.N, GM_AC] - 1
FRtn <- dt[-1,F_AC] / dt[-.N, F_AC] - 1
par(mfrow = c(1,1))
plot(GMRtn, FRtn, col = 'red')
library(ggplot2)
qplot(GMRtn, FRtn,color = 'steelblue', size = 0.2)
