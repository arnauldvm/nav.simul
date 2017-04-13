angleOf = function(v1) {
  atan2(v1[2], v1[1])
}

# As per http://stackoverflow.com/a/24999820/318354

angleBetween = function(v1, v2) {
  dot.prod <- v1%*%v2
  norm.v1 <- norm(v1,type="2")
  norm.v2 <- norm(v2,type="2")
  theta <- acos(dot.prod / (norm.v1 * norm.v2))
  as.numeric(theta)
}
