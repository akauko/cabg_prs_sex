#!/bin/bash


# PREPROCESSING UKBB DATA
#-------------------------
#Note: This has been run in reality as small pieces. Running all at once is not tested. 

in_dir="/finngen/red/cabg_prs/data/gwas_sums"
sum_dir="/finngen/red/cabg_prs/data/gwas_sums_munged"

# Unzip the summary statistics & variant info
gzip -S .bgz -d $in_dir/*.tsv.bgz

# variants and rsid's picked up from variants.tsv - if contains 'rs'
awk -F'\t' '{ print $1 "\t" $4 "\t" $5 "\t" $6}' $in_dir/variants.tsv | grep rs > $in_dir/var_rsid.tsv
 
#Preprocessing input file
#		Run in fork for the purpose of parallelization


preprocess() {
	local run=$1
	[ -e "$in_file" ] || continue

	out_file=$(echo $in_file | perl -pe 's/.*?(\w+)\.\w+\.\w+\.(\w+.tsv)/ukb_$1_$2/')  #simplify name
	#out_file="$(basename -- "$in_file" .tsv)_mngd.tsv"   #Old version - just adds 'mngd'
	echo "Reformatting $in_file" 
	echo -e "to $sum_dir/$out_file... \n"
	Rscript my_prepare-ukb-data2.R $in_file $in_dir/var_rsid.tsv $sum_dir/$out_file
}

#It is not possible to run all at once. Batches of six files worked at 
#"rather big machine"

sexes=(female male both_sexes)

for sex in "${sexes[@]}"; do

	for in_file in $in_dir/*gwas*.$sex.tsv; do
		preprocess "$run" &
	done
	wait

done



#Renaming afterwards:
#for f in *.tsv; do
#	of=$(echo $f | perl -pe 's/(\w+)\.\w+\.\w+\.(\w+)_\w+.tsv/ukb.$1.$2.tsv/')  #simplify name
#	mv $f $of
#done

#: ' #This is multiline comment hack
#'   #End of comment 




