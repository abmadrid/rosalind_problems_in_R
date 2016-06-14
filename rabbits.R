# Rabbits and Recurrence Relations
# Given: Positive integers n<=40 and k<=5.
# Return: The total number of rabbit pairs that 
#         will be present after n months if we 
#         begin with 1 pair and in each generation, 
#         every pair of reproduction-age rabbits 
#         produces a litter of k rabbit pairs (instead of only 1 pair).
# Sample input: 5 3
# Sample output: 19
my_fibR <- function(n,k) {
  if (n == 0) return(0)
  if (n == 1) return(1)
  return (my_fibR((n - 1),k) + my_fibR((n - 2),k)*k)
}