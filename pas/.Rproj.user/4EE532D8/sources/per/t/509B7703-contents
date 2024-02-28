GaussLepsi <- function(A,b){
  n <- nrow(A)
  Ab <- cbind(A,b)
  #primy chod
  for(k in 1:(n-1)){
    pivot <- which.max(abs(Ab[k:n,k]))+k-1
    if(k != pivot){
      pom <- Ab[k,]
      Ab[k,] <- Ab[pivot,]
      Ab[pivot,] <- pom
    }
    for(i in (k+1):n){
      m <- Ab[i,k]/Ab[k,k]
      Ab[i,(k+1):(n+1)] <- Ab[i,(k+1):(n+1)]-m*Ab[k,(k+1):(n+1)]
    }
  }
  #zpetny chod
  x <- Ab[,n+1]
  x[n] <- x[n]/Ab[n,n]
  for(i in (n-1):1){
    x[i] <- (x[i] - sum(Ab[i,(i+1):n]*x[(i+1):n]))/Ab[i,i]
  }
  return(x)
}


# Definice matice A
A <- matrix(numeric(100), nrow = 10, ncol = 10)
for (i in 1:10) {
  for (j in 1:10) {
    A[i, j] <- cos((i-1)*j) - j
  }
}

# Definice vektoru y
y <- sin(1:10)

x_sol <- GaussLepsi(A, y)
print(x_sol)

