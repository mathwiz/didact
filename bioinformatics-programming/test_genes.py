import genes

print(genes.RNA_codon_table)

print(genes.rna_to_aa("UAG"))
print("UAG codes for something" if genes.rna_to_aa("UAG") else "UAG is Stop")
print(genes.rna_to_aa("AAG"))
print(genes.rna_to_aa("UUU"))
print(genes.rna_to_aa("GGC"))
print(genes.rna_to_aa("GAC"))
print(genes.rna_to_aa("ZZZ"))


print("done")
