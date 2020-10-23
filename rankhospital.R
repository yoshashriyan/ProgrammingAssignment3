rankhospital <- function(state, oc, num = "best"){
  if(num == "best")
    num == 1
  if(num == "worst")
    num == -1
  if(state %in% snames && oc %in% poss_out)
  {
    s1 <- outcome[outcome$State == state, ]

    if(oc == "heart attack"){
      cnum = 11;
      srtd <- s1[order(s1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
    }
    if(oc == "heart failure"){
      cnum = 17;
      srtd <- s1[order(s1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
      }
    if(oc == "pneumonia"){
      cnum = 23;
      srtd <- s1[order(s1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
    }
    View(srtd)
    n <- nrow(s1)

    for(i in 1:n){
      if(is.na(srtd[i, cnum]) == T)
        break
    }
    x <- srtd[-c(i:n)]
    nn <- nrow(x)
    rank <- 1:nn
    result <- cbind(x$Hospital.Name,rank)
    #result <- sort(result)
    return(result)
  }
  else
    print("Invalid Input");

}
