#read outcome data
outcome <- read.csv("data/outcome-of-care-measures.csv")

#check data
head(outcome)
ncol(outcome)
names(outcome)

outcome[, 11] <- as.numeric(as.character(outcome[, 11]))
hist(outcome[,11])