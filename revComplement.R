# complementing a strand of DNA
# Given: A DNA string ss of length at most 1000 bp.
# Return: The reverse complement s_c of s.
# Sample input: AAAACCCGGT
# Sample output: ACCGGGTTTT
revComplement<-function(s){
  #s_c_1<-rev(s)
  s<-unlist(strsplit(s,""))
  s_c<-unlist(lapply(rev(s),function(x){
    if(x=="A"){ compl<-"T" }
    else if(x=="T"){ compl<-"A" }
    else if(x=="C"){ compl<-"G" }
    else ( compl<-"C" )
    return(compl)
  }))
  s_c<-paste(s_c,sep="",collapse = "")
  return(s_c)
}

revfile<-"rosalind_revc.txt"
rev_data<-scan(revfile,what="character")
revComplement(rev_data)
