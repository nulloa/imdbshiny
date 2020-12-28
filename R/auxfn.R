get_rating_score <- function(r, bnd){
  score <- rep(F, length(r))
  for (i in 1:(length(bnd)-1)){
    score[between(r, bnd[i], bnd[i+1])] <- c("Garbage", "Bad", "Average", "Great", "Perfect")[i]
  }
  return(score)
}

