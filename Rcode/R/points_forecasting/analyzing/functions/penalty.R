##########
# Penalty function based on transfers
##########


penalty <- function(ill_trans,wildcard,free_hit){
  if(wildcard != 0){
    penalty <- 0
    return(penalty)
  } else if(free_hit != 0){
    penalty <- 0
    return(penalty)
  } else{
    penalty <- ill_trans*4
    return(penalty)
  }
}
