#if.na

if.na <- function(data){
  if(is.na(data)){
    output <- 0
  }else{
    output <- data
  }
  return(output)
}