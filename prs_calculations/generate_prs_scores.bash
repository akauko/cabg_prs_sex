#!/bin/bash

in_dir="/finngen/pipeline/cromwell/workflows/prs_ukb3"
tmp_dir="/finngen/red/ukb-prs-others/data/prs_tmp"
prs_dir="/finngen/red/ukb-prs-others/data/ukb_prs"

name_list="name_list.txt"
finngenBim="/finngen/red/ukb-prs-others/data/finngen_R6.hm3.rsid.bim" 
plink_basepath="/finngen/library-red/finngen_R6/genotype_plink_1.0/data/finngen_R6"
refin="/finngen/red/ukb-prs-others/data/gwas_sums/var_rsid.tsv"

sexes=(both_sexes female male)


#Note: This has been run in reality as small pieces. Running all at once is not tested. 

# PROCESS PIPELINE OUTPUT
#-----------------------

#First all prs-scores are copied to one place.
#(Catenation done as separate step to be sure that files are correct; catenation is much faster than copying!)
find $in_dir -name "*.txt" -type f -exec cp {} $tmp_dir \;

#Chromosomes are catenated to one file (would have been easier to do at pipeline)
while read name; do
	echo $name
	find $tmp_dir -name "*$name*.txt" -type f -exec cat {} > "$tmp_dir/ukb.$name.prs_raw.tsv" \;
done <$name_list

#Rscript to 
# - Change format from rsids to chromosome position
# - Generate all allele permutations

for f in $tmp_dir/*ukb*raw.tsv; do
	out=$(basename $f | perl -pe 's/^(.*).prs_raw.tsv/$1.weights.tsv/') # *prs_raw.tsv -> *.weights.tsv
	echo -e "Reformatting $f\n to $prs_dir/$out\n"
 	Rscript reformat_raw_prs.R $f $finngenBim $refin $prs_dir/$out &
done
wait

#Renaming 20002_1471 to I9_AF (Would be better to have replacement table for all variables at preprocessing script)
ukbid="20002_1471"
newid="I9_AF"

for f in $prs_dir/*$ukbid*; do
	of=$(echo $f | perl -pe "s/$ukbid/$newid/")
	echo "mv $f $of"
	mv $f $of
done


# SCORES CALCULATED
#------------------

#Plink run in three batches - if all run at once there is problems accessing bim-file

run_plink() {
	local run=$1
	[ -e "$in_file" ] || continue

	#f=/finngen/red/ukb-prs-others/data/ukb_prs/ukb.K11_GIBLEEDING.male.weights.tsv
	out=$(basename $f | perl -pe 's/^(.*).weights.tsv/$1/') #remove .weights.tsv from the end of file name
	echo -e "Calculating scores for $f\n Output prefix $prs_dir/$out\n"

 	plink \
   	--bfile $plink_basepath \
   	--score $f 1 2 3 center \
   	--out "$prs_dir/$out" 

}

for sex in "${sexes[@]}"; do

	echo -e "Calculating scores for $prs_dir/ukb.*.$sex.weights.tsv"
	for f in $prs_dir/ukb.*.$sex.weights.tsv; do
		run_plink "$run" &
	done
	wait

done



#: ' #This is multiline comment hack
#'   #End of comment 

