# Translating RNA into Protein
# Given: An RNA string s corresponding to a strand of mRNA (of length at most 10 kbp).
# Return: The protein string encoded by s.
# Sample input: AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA
# Sample output: MAMAPRTEINSTRING
library(stringr)

makeCodonAAHash<-function(){
  codons<-scan("RNA_codons_tab.txt",what = "character")
  length(codons)
  codon_hash<-unlist(str_split(codons,","))
  length(which(codon_hash==""))
  codon_hash<-codon_hash[which(!codon_hash=="")]
  codon_df<-data.frame(codon_hash[seq(from=1,to=length(codon_hash),by=2)],codon_hash[seq(from=2,to=length(codon_hash),by=2)])
  names(codon_df)<-c("codons","amino_acids")
  return(codon_df)
}

getProteins<-function(s,codon_df){
  s<-substring(s,seq(1,str_length(s),3),seq(3,str_length(s),3))
  s_conv<-unlist(lapply(s,function(x) {codon_df$amino_acids[which(codon_df$codons==x)]}))
  proteins<-paste(s_conv[which(s_conv!="Stop")],sep="",collapse = "")
  return(proteins)
}
s<-scan("rosalind_prot.txt",what="charater")                                                                   
write(getProteins(s,makeCodonAAHash()),"proteinsFromRNA.txt")
