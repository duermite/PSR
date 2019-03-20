#fn_no_zeros

fn_no_zeros <- function(citations){
  if(citations==0){
    new_cit <- 0.5
  } else{
    new_cit <- citations
  }
  return(new_cit)
}
fn_no_zeros(5) #test