# transcribing DNA into RNA
# Given: A DNA string tt having length at most 1000 nt.
# Return: The transcribed RNA string of t
# Sample input: GATGGAACTTGACTACGTAAATT
# Sample output: GAUGGAACUUGACUACGUAAAUU
DNA_file<-"rosalind_rna.txt"
dna_data<-scan(DNA_file,what="character")
rna_data<-gsub("T","U",dna_data)
rna_data
