# counting DNA nucleotides
# Given: A DNA string 
# Return: Four integers (separated by spaces) counting 
#         the respective number of times that the symbols 
#         'A', 'C', 'G', and 'T' occur in given string.
# Sample input: AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC
# Sample output: 20 12 17 21
countDNANucleotides<-function(dna_str){
  nt_data<-unlist(strsplit(dna_str,""))
  a_nt<-length(which(nt_data=="A"))
  c_nt<-length(which(nt_data=="C"))
  g_nt<-length(which(nt_data=="G"))
  t_nt<-length(which(nt_data=="T"))
  return(c(a_nt,c_nt,g_nt,t_nt))
}

ntfile<-"rosalind_dna.txt"
nt_data<-scan(ntfile,what="character")
countDNANucleotides(nt_data)
