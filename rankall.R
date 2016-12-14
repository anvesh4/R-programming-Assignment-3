rankall <- function(outcome,num="best") {
    
    ## Read outcome data    
    data <- read.csv("outcome-of-care-measures.csv",na.strings="Not Available",stringsAsFactors=FALSE)
    
    ## subset the three columns
    outcomevector <- c("heart attack"=11, "heart failure"=17, "pneumonia"= 23)
    
    ##check the outcome is valid
    
    if(!(outcome %in%  c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    }
    
    
    df.NAs <- data[,c(2,7,outcomevector[outcome])]	
    names(df.NAs)<-c("hospital","state","outcome")
    #print(head(df.NAs))
    
    ##remove NAs
    nonarrangeddf<-na.omit(df.NAs)
    #print(head(nonarrangeddf))
    
    ## Order by state then outcome then hospital name
    library(plyr)
    dfNonSplit <- arrange(nonarrangeddf,state,outcome,hospital)
    #print(head(df))
    
    ##Split by state
    df <- split(dfNonSplit, dfNonSplit$state)
    #print(head(df,n=2)) here n=2 gives the data in 2 states. 
    
    ##Return hospital name in that state with lowest 30-day death rate
    # Run lapply
    # lapply(df,function(data)data[1,1])) 
    
    #This gives a named list where the list names are state and the
    #list values are hospital name (one for each state).
    lapply.result <- lapply(df,function(data)
                     if(num == "best") {
                      data[1,1]
                     }
                     
                     else if(num == "worst") {
                         
                         data[nrow(data),1]
                     }
                     
                     else {
                         data[num,1]
                     })
    #here you don't have to use unlist if you use sapply instead of lapply above            
    data.frame(hospital=unlist(lapply.result),state=names(lapply.result),
                         row.names = names(lapply.result))
    # note:here you dont have to give the row.names because by default
    # you get the state names as row names(probably because you have split
    # the data by state).you get the row names(states) and the hospital
    # names if you remove the row.names and the state=names(...) data.
     
}
    
    
    
