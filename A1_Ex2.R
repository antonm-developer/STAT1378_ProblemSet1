set.seed(2048)##
N <- 100 ## Created a list X with N = 100 slots, and in each slot, creating a list of two elements called "name" and "vect"
X. <- vector("list", N)
for(i in 1:N){
  name <- paste0("n", i)
  vect <- rnorm(5, mean = 20, sd = 10)
  X.[[i]] <- list(name = name, vect = vect)
}
X.[[i]] ##Testing if it generates 100 slots properly
str(X.)##Structure of X.
sums_v <- sapply(X., function(flamingdragon) sum(flamingdragon$vect))
print(sums_v)##Sum of values vect 
M <- matrix(unlist(sapply(X., function(flamingdragon) flamingdragon$vect)), nrow = N, byrow = TRUE) ##Created a matrix M of size 100× 5 using the 100 vect objects in the slots of X
col_sums <- numeric(5)
for(i in 1:5){ ##Used a for loop to compute the sums by columns of M and show the result.
  col_sums[i] <- sum(M[, i])
  }
print(col_sums)

