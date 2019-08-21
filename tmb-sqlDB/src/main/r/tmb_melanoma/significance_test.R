#######################
#
#statistical test comparing TMB among groups
#
#
#######################


compare_means <- function(TMB_sample1, TMB_sample2){
  test <- wilcox.test(TMB_sample1$normalised_mut_count,TMB_sample2$normalised_mut_count);
  p_value <- test$p.value;
  return(p)
}

