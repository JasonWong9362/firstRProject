myFirstRFun <- function(n){
  sum <- 0
  for (i in seq(n-1, 2, -1)){
    if (i%%2 == 0){
      sum = sum + i
      next
    }
    if (i%%7 == 0){
      sum = sum + i
    }
  }
  return(sum)
}

mySum = myFirstRFun(1000)

