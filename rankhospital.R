rankhospital <- function(state, outcome, num="best")
{
    ## Read hospital compare data
    data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE ) 
    aan <- c("heart failure","heart attack", "pneumonia")   
    
    ## Check that State and outcome are valid	
    if (!(state %in% data[,7])) stop("invalid state")
    if (!(outcome %in% aan)) stop("invalid outcome")
    
    ##Return hospital name in that state with lowest 30-day death
    ## Relevant columns are 11,17,23 for the numbers on the 30 day mortality rate belonging to the deseases of this exercise
    if (outcome==aan[2]) {i <- 11}
    if (outcome==aan[1]) {i <- 17}
    if (outcome==aan[3]) {i <- 23}
    
    ## select only the data from the state that was given as input
    data <- subset(data, data[,7]==state)
    
    ##second column in data is the Hospitalname
    l <-c(2,i)
    ##make sure yy should have the column with name of the hospital and the outcome/disease to be investigated
    yy <- data[,l]
    
    yy[,2]<-as.numeric(yy[,2])
    
    
    ##kk should now only contain combinations of hospital name and outcome that are not empty
    yy <- yy[complete.cases(yy),]
    
    ## now order the list with the second column defining the outcome as first argument and name of the hospital as second argument
    ff <- yy[order(yy[,2],yy[,1]),]
    tel <- num
    if(num=="best") tel<-1
    if(num=="worst") tel<-nrow(yy)
    ##print(tel)
    ##this is just to format the output better. Else a Factor type of description would go along
    pp<-as.character(ff[tel,1])
    print(pp)
}