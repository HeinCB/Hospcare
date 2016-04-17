best <- function(state,outcome)
{
## Read hospital compare data
data <- read.csv("outcome-of-care-measures.csv") 
aan <- c("heart failure","heart attack", "pneumonia")   

## Check that State and outcome are valid	
if (!(state %in% data[,7])) stop("invalid state")
if (!(outcome %in% aan)) stop("invalid outcome")

##Return hospital name in that state with lowest 30-day death
## Relevant columns are 11,17,23 for the numbers on the 30 day mortality rate belonging to the deseases of this exercise
if (outcome==aan[2]) {i <- 11}
if (outcome==aan[1]) {i <- 17}
if (outcome==aan[3]) {i <- 23}
##print(aan)
##print(i)
##second column in data is the Hospitalname
l <-c(2,i)
yy <- data[,l]
kk <- yy[complete.cases(yy),]
rr <- order(kk)
zz <- str(kk)
print (zz)


}