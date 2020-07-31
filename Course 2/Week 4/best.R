best<-function(state,outcome){
  data<-read.csv("outcome-of-care-measures.csv")
  out<-c("heart attack","heart failure","pneumonia")
  state_names<-unique(data$State)
  print(state_names)
  print(state)
  print(outcome)
  if((state %in% state_names) && (outcome %in% out)){
    print("1")
    d<-subset(data,data$State==state)
    m<-match(outcome,out)
    if(m==1)
      a<-d[which.min(d[,11]),2]
    else if(m==2)
      a<-d[which.min(d[,17]),2]
    else if(m==3)
      a<-d[which.min(d[,23]),2]
    return(a)
  }
  else{
    s<-"invalid"
    return(s)
  }
}


