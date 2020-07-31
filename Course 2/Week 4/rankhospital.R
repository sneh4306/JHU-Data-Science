ranks<-function(new_d,num){
  #print(new_d$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  if(num=="best")
    a<-new_d[1,]
  else if(num=="worst")
    a<-new_d[nrow(new_d),]
  else
    a<-new_d[num,]
  return(a)
}

rankhospital<-function(state,outcome,num){
  data<-read.csv("outcome-of-care-measures.csv")
  out<-c("heart attack","heart failure","pneumonia")
  state_names<-unique(data$State)
  if((state %in% state_names) && (outcome %in% out)){
    d<-subset(data,data$State==state)
    m<-match(outcome,out)
    if(m==1){
      new<-d[order(d$Hospital.Name),]
      new<-new[order(as.numeric(new[,11])),]
      new_d<-subset(new,!(new$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack=="Not Available"))
      print(new_d$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
      #na.omit(new_d,cols=d[,11])
    }
    else if(m==2){
      new<-d[order(d$Hospital.Name),]
      new<-new[order(as.numeric(new[,17])),]
      new_d<-subset(new,!(new$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure=="Not Available"))
      print(new_d$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    }
    else{
      new<-d[order(d$Hospital.Name),]
      new<-new[order(as.numeric(new[,23])),]
      new_d<-subset(new,!(new$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia=="Not Available"))
      print(new_d$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Pneumonia)
    }
    #new_d<-new_d[complete.cases(new_d), ]
    z<-ranks(new_d,num)
    return(z$Hospital.Name)
  }
  else
    print("Invalid")
  
}