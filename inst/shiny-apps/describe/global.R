#initialize
library(shiny)


aron1_5 <- as.data.frame(read.csv("datasets/arontable1_5.csv"))
mouse1 <- as.data.frame(read.csv("datasets/amount.csv"))
morning1 <- as.data.frame(read.csv("datasets/morning_person_combined2.csv"))
#names(morning1) <- c("Rating","Instructor")
faithful2 <- faithful
names(faithful2) <- c("Eruption Duration (min)","Eruption Interval (min)")

#source("eahist.R")


