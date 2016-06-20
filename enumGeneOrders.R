# enumerating gene orders
# Given: A positive integer n<=7.
# Return: The total number of permutations of length n, 
#         followed by a list of all such permutations (in any order).
# Sample input: 3
# Sample output:  
# 6
# 1 2 3
# 1 3 2
# 2 1 3
# 2 3 1
# 3 1 2
# 3 2 1
enumGeneOrders <- function(n){
  if(n==1){
    return(matrix(1))
  } else {
    sp <- permutations(n-1)
    p <- nrow(sp)
    A <- matrix(nrow=n*p,ncol=n)
    for(i in 1:n){
      A[(i-1)*p+1:p,] <- cbind(i,sp+(sp>=i))
    }
    return(A)
  }
}
n=3
gene_orders<-matrix(enumGeneOrders(n),ncol=n)
dim(gene_orders)[1]
gene_orders
