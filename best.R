best <- function(state, oc){
  outcome <- read.csv("outcome-of-care-measures.csv")
  outcome[, 11] <- as.numeric(outcome[, 11])
  snames <- outcome[, 7]
  poss_out <- list("heart attack", "heart failure", "pneumonia")
  if(state %in% snames && oc %in% poss_out)
  {
    s1 <- outcome[outcome$State == state, ]

    if(oc == "heart attack")
      cnum = 11;
    if(oc == "heart failure")
      cnum = 17;
    if(oc == "pneumonia")
      cnum = 23;
    s1[, cnum] <- as.numeric(s1[, cnum])
    x <- na.omit(s1[, cnum])
    min_val <- min(x)
    n <- nrow(s1)
    #print(outcome$Hospital.Name[min_val])
    for (i in 1:n) {
      #print(s1[i,cnum])
      if(is.na(s1[i, cnum]) == T)
        next;
      if(s1[i, cnum] == min_val){
        print(s1[i,2])
      }
    }
  }
  else
    print("Invalid Input");

}
