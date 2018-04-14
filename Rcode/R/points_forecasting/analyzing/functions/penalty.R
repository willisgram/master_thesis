##########
# Penalty function based on transfers
##########


penalty <- function(i,ill_trans_round,wildcard,free_hit){
  if(i == 1){
    penalty <- 0
    return(penalty) 
  } else if(wildcard != 0){
    penalty <- 0
    return(penalty)
  } else if(free_hit != 0){
    penalty <- 0
    return(penalty)
  } else{
    penalty <- ill_trans_round*4
  }
}
