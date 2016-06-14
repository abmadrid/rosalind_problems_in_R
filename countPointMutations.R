#################
# counting point mutations
# Given: Two DNA strings s and t of equal length (not exceeding 1 kbp).
# Return: The Hamming distance dH(s,t).
# Sample input: GAGCCTACTAACGGGAT
#               CATCGTAATGACGGCCT
# Sample output: 7
countPtMutations<-function(s,t){
  return(length(which(s!=t)))
}

cpm_infile<-"rosalind_hamm.txt"
cpm_data<-scan(cpm_infile,what="character")
s<-unlist(strsplit(as.character(cpm_data[1]),""))
t<-unlist(strsplit(as.character(cpm_data[2]),""))
countPtMutations(s,t)
