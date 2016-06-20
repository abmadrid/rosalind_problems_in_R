#################
# Finding a motif in DNA
# Given: Two DNA strings s and t (each of length at most 1 kbp).
# Return: All locations of t as a substring of s.
# Sample input: GATATATGCATATACTT
#               ATAT
# Sample output: 2 4 10
library(stringr)
findMotif<-function(s,t){
  num_it<-str_length(s)-str_length(t)
  res<-integer()
  for (i in 1:num_it){
    s_temp<-str_sub(s,start=i,end=str_length(s))
    loc<-str_locate(s_temp,t)[1,1]
    if( !is.na(loc) & loc == 1){
      res<-c(res,i)
    }
  }
  return(res)
}

motif_file<-"rosalind_subs.txt"
motif_dat<-scan(motif_file,what="character")
s<-motif_dat[1]
t<-motif_dat[2]
res<-findMotif(s,t)

