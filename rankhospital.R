rankhospital <- function(state, outcome,num="best") {
    
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
    
    
    
    ## Check that state and outcome are valid
    
    if(!(state %in% df.NAs$state)) {
        stop("invalid state")
    }
    
    ##remove NAs
    nonarrangeddf<-na.omit(df.NAs)
    #print(head(nonarrangeddf))
    
    ## Order by state then outcome then hospital name
    library(plyr)
    df <- arrange(nonarrangeddf,state,outcome,hospital)
    #print(head(df))
    
    ##Return hospital name in that state with lowest 30-day death rate
    
    subset.state <- df[df$state==state,]
    #print(subset.state)
    
    if(num == "best") {
    
    subset.state$hospital[1]
    # print(subset.state[1,1])  
        
    }
    else if(num == "worst") {
        
        #subset.state[nrow(subset.state$hospital),1]
        subset.state[nrow(subset.state),1]
        
    }
    else {
        subset.state[num,1]
        #subset.state$hospital[num]
    }
    
    
}
