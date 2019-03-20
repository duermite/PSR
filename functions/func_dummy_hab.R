#dummy_hab

#this function creates dummy variables from habitat
#0=freshwater
#1=euryhaline
#2=marine
dummy_hab <- function(aq_hab){
  if(aq_hab=="fw"){
    hab_new=0
  } else if (aq_hab=="euryhaline"){
    hab_new=1
  }else {
    hab_new=2
  }
  return(hab_new)
}
#testint
dummy_hab("fw")
dummy_hab("euryhaline")
dummy_hab("marine")