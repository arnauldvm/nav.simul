# Returns a dataframe
parseLines = function(perlNamedPattern, linesVector) {
  match = regexpr(perlNamedPattern, linesVector, perl=TRUE)
  found_idx = as.vector(match)>0
  names = attr(match, "capture.names")
  start = attr(match, "capture.start")
  length = attr(match, "capture.length")
  end = start + length -1
  results_matrix = c()
  for (col in seq(1, length(names))) {
    results_matrix = rbind(results_matrix, substr(linesVector, start[,col], end[,col]))
  }
  results_df=data.frame(t(results_matrix))
  names(results_df) = names
  return(results_df[found_idx,])
}
