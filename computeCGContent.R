# computing GC content
# Given: At most 10 DNA strings in FASTA format (of length at most 1 kbp each).
# Return: The ID of the string having the highest GC-content, 
#         followed by the GC-content of that string. 
# Sample input:
#   >Rosalind_6404
#   CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC
#   TCCCACTAATAATTCTGAGG
#   >Rosalind_5959
#   CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT
#   ATATCCATTTGTCAGCAGACACGC
#   >Rosalind_0808
#   CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC
#   TGGGAACCTGCGGGCAGTAGGTGGAAT
# Sample output:
#   Rosalind_0808
#   60.919540
library(stringr)

getSeq<-function(ids_ind){
  last_index<-length(ids_ind)
  seqs<-rep(NA,last_index)
  for (i in 1:(last_index-1)){
    seqs[i]<-paste(dna_strs[(ids_ind[i]+1):(ids_ind[i+1]-1)],sep = "",collapse = "")
  }
  seqs[last_index]<-paste(dna_strs[(ids_ind[last_index]+1):(length(dna_strs))],sep = "",collapse = "")
  return(seqs)
}

getCGContent<-function(dna_str){
  c_s<-str_count(dna_str,"C")
  g_s<-str_count(dna_str,"G")
  gc_content<-(c_s+g_s)/str_length(dna_str)
  return(gc_content)
}

getMaxGCContent<-function(df){
  max_id<-df$id[which(df$gc_content==max(df$gc_content))]
  max_content<-df$gc_content[which(df$gc_content==max(df$gc_content))]
  return(c(toString(max_id),(max_content*100)))
}

gc_file<-"rosalind_gc.txt"
dna_strs<-scan(gc_file,what="list", sep=">")
dna_strs<-dna_strs[which(dna_strs!="")]
ids_index<-grep("Rosalind*",dna_strs)
ids<-dna_strs[ids_index]

DNA<-getSeq(ids_index)

dna_strs.df<-data.frame(cbind(ids,DNA))
dna_strs.df$gc_content<-unlist(lapply(dna_strs.df$DNA,getCGContent))

res<-getMaxGCContent(dna_strs.df)
res
