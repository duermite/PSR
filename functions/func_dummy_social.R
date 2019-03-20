#dummy_social
#creates dummy variable from sociality data
#0=solitary
#1=ontogenetic gregarious or repro gregarious
#2=gregarious

#fam_soc_score
levels(host_char$fam_soc_score)
dummy_social <- function(fam_soc_score){
  if(fam_soc_score=="solitary"){
    soc_score=0
  } else if (fam_soc_score=="gregarious"){
    soc_score=2
  } else {
    soc_score=1
  }
  return(soc_score)
}
#testing
dummy_social("solitary")
dummy_social("gregarious")
dummy_social("ontogenetic_gregarious")