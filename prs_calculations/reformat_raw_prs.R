#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

# test if there is three argument: if not, return an error
if (length(args)!=4) {
  stop("Two arguments must be supplied (input file)", call.=FALSE)
} 

file_in  <- args[1]
file_bim <- args[2]
file_refin <- args[3]
file_out <- args[4]

#file_in <- "/finngen/red/cabg_prs/data/prs_tmp/test_raw.tsv"
#file_bim <- "/finngen/red/cabg_prs/data/finngen_R6.hm3.rsid.bim"
#file_refin <- "/finngen/red/cabg_prs/data/gwas_sums/var_rsid.tsv"
#file_out <- "/finngen/red/cabg_prs/data/prs_tmp/test_weightsF.tsv"


#file_in <- "/finngen/red/cabg_prs/data/prs_tmp/ukb.I9_AF.both_sexes.prs_raw.tsv"
#file_in <- "/finngen/red/ukb-bp-prs/R7/ukb.sbp.both.prs_raw.tsv"
#file_bim <- "/finngen/red/cabg_prs/data/finngen_R6.hm3.rsid.bim"
#file_refin <- "/finngen/red/cabg_prs/data/gwas_sums/var_rsid.tsv"
#file_out <- "/finngen/red/cabg_prs/data/ukb_prs/test/SBP_testF.tsv"

library(data.table, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(magrittr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(stringi, warn.conflicts = FALSE)

#Changes rsid's to chromosome positions for alleles and removes generates permutated positions.
#This processes only one file. Looping at bash. These are large files and processing even one takes time.

### variant - rs mapping is read in
bim <- fread(file_bim, sep="\t") %>%
  rename(CHR=V1, SNP=V2, CM=V3, BP=V4, A1=V5, A2=V6) %>%
  select(SNP,BP)

### variant - rs mapping is read in
refin <- fread(file_refin) %>%
select(rsid, ref) %>%
rename(SNP=rsid)

df <- fread(file_in, sep="\t") %>%
  rename(CHR=V1, SNP=V2, BP=V3, A1=V4, A2=V5, WGHT=V6) %>%
  select(-BP) %>%
  left_join(bim, by = "SNP") %>%  
  left_join(refin, by = "SNP") %>%
  arrange(CHR, BP) %>%
  mutate(A3=stri_reverse(chartr("ATGC","TACG", A1)),
         A4=stri_reverse(chartr("ATGC","TACG", A2))) %>%
  unite(P1, c("CHR","BP","A1","A2"), sep = "_", remove = F) %>%
  unite(P2, c("CHR","BP","A2","A1"), sep = "_", remove = F) %>%
  unite(P3, c("CHR","BP","A3","A4"), sep = "_", remove = F) %>%
  unite(P4, c("CHR","BP","A4","A3"), sep = "_", remove = F) %>%
	mutate(A1=ref, A2=ref, A3=ref, A4=ref) %>%																			
  pivot_longer(c("P1", "P2", "P3", "P4", "A1", "A2", "A3", "A4"),
                      names_to = c(".value", "tmp"),
                      names_pattern = "(\\w)(\\d)") %>%
	mutate(tmp="chr") %>%
	unite(P, c("tmp","P"), sep = "") %>%
	distinct() %>%
  select(P, A, WGHT)

## New file is written out
fwrite(df, file_out, sep="\t", col.names = F)


