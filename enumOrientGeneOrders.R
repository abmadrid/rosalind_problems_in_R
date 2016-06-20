# enumerating oriented gene orderings
# Given:  A positive integer n<=6
# Return: The total number of signed permutations of length n, 
#         followed by a list of all such permutations 
#         (you may list the signed permutations in any order).
# Sample input: 2
# Sample output:
# 8
# -1 -2
# -1 2
# 1 -2
# 1 2
# -2 -1
# -2 1
# 2 -1
# 2 1
mySignedPerm<-function(n){
  sv_size<-n*2
  sv<-c(seq(from=1,to=n,by=1),(seq(from=1,to=n,by=1))*-1)
  a_perm<-permutations(sv_size,n,sv)
  dups<-data.frame(t(apply(a_perm,1,function (x) {duplicated(abs(x))})))
  indices<-unlist(apply(dups,2,function(x) {which(x==TRUE)}))
  a_perm<-a_perm[-indices,]
  return(a_perm)
}
val<-2
res<-mySignedPerm(val)
tot_signedPerms<-dim(res)[1]
# write result to output file
outfile<-"Rosalind_signedPerm_out.txt"
write(tot_signedPerms,outfile)
write(t(res),outfile,sep = " ",ncolumns = val,append = TRUE)
