workflow prs_ukb3 {

  String dataName
  
  String prsPath = "ukb-prs-others/PRScs-master"
  String dataPath = "ukb-prs-others/data/gwas_sums_munged"
  String crwPath = "/cromwell_root/fg-production-sandbox-20_ivm"

  File python1 = "SANDBOX_RED/${prsPath}/gigrnd.py"
  File python2 = "SANDBOX_RED/${prsPath}/mcmc_gtb.py"
  File python3 = "SANDBOX_RED/${prsPath}/parse_genet.py"
  File pythonScript = "SANDBOX_RED/${prsPath}/PRScs.py"
  
	#Ld panel
  String ld_file_list   = "SANDBOX_RED/${prsPath}/ld_ref_file_list.txt"
  Array[File] ld_files  = read_lines(ld_file_list)
  File ldRefFolder      = "SANDBOX_RED/${prsPath}/ldblk_1kg_eur"
  File ldSnpInfo        = "SANDBOX_RED/${prsPath}/ldblk_1kg_eur/snpinfo_1kg_hm3"

	#Finngen bim prefix
	String fgBimPref = "ukb-prs-others/data/finngen_R6.hm3.rsid"

	#cases 'female', 'male' and 'both' are run for each chromosome
  scatter (chromosome in ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22"]) {
    call prsCS_f {input: chr=chromosome, dataName=dataName, dataPath=dataPath, prsPath=prsPath, crwPath=crwPath, py=pythonScript, p1=python1, p2=python2, p3=python3, ldnfo=ldSnpInfo, ldf=ldRefFolder, ld_files=ld_files, bim=fgBimPref}
 		call prsCS_m {input: chr=chromosome, dataName=dataName, dataPath=dataPath, prsPath=prsPath, crwPath=crwPath, py=pythonScript, p1=python1, p2=python2, p3=python3, ldnfo=ldSnpInfo, ldf=ldRefFolder, ld_files=ld_files, bim=fgBimPref}
 		call prsCS_b {input: chr=chromosome, dataName=dataName, dataPath=dataPath, prsPath=prsPath, crwPath=crwPath, py=pythonScript, p1=python1, p2=python2, p3=python3, ldnfo=ldSnpInfo, ldf=ldRefFolder, ld_files=ld_files, bim=fgBimPref}
	}

}


#Oikeasti olisi olemassa 'subworkflow', jolla tätä voisi käsitellä niinkuin aliohjelmaa, eikä tarvitsisi copypasteta jokaista taskia kolmeen kertaan... 
#Nyt ei lusikat riitä syntaksin selvittämiseen. Esimerkeissä subworkflow importataan eri tiedostosta.

task prsCS_f {
  
  String chr
  Int n = 194174
  String dataName
	String sex="female"

	String dataPath
	String prsPath
	String crwPath
  
  File p1
  File p2
  File p3
  File py
  
  File ldnfo
  File ldf
  Array[File] ld_files

	String bim
  File bimFile =     "SANDBOX_RED/${bim}.bim"
  File sumStatFile = "SANDBOX_RED/${dataPath}/${dataName}.${sex}.tsv"

  command {
    python ${py} \
    --ref_dir="${crwPath}/${prsPath}/ldblk_1kg_eur" \
    --bim_prefix="${crwPath}/${bim}" \
    --sst_file="${crwPath}/${dataPath}/${dataName}.${sex}.tsv" \
    --n_gwas=${n} \
    --out_dir="/cromwell_root/${dataName}.${sex}" \
    --chrom=${chr}
  }

  output {
    File out1 = "${dataName}.${sex}_pst_eff_a1_b0.5_phiauto_chr${chr}.txt"
  }

  runtime {
    docker: "eu.gcr.io/finngen-sandbox-v3-containers/bioinformatics:1.0.1"
    cpu: 1
    memory: "10 GB"
    disks: "local-disk 50 HDD"
    zones: "europe-west1-b"
  }

}


task prsCS_m {
  
  String chr
  Int n = 167020
  String dataName
	String sex="male"

	String dataPath
	String prsPath
	String crwPath
  
  File p1
  File p2
  File p3
  File py
  
  File ldnfo
  File ldf
  Array[File] ld_files

	String bim
  File bimFile =     "SANDBOX_RED/${bim}.bim"
  File sumStatFile = "SANDBOX_RED/${dataPath}/${dataName}.${sex}.tsv"

  command {
    python ${py} \
    --ref_dir="${crwPath}/${prsPath}/ldblk_1kg_eur" \
    --bim_prefix="${crwPath}/${bim}" \
    --sst_file="${crwPath}/${dataPath}/${dataName}.${sex}.tsv" \
    --n_gwas=${n} \
    --out_dir="/cromwell_root/${dataName}.${sex}" \
    --chrom=${chr}
  }

  output {
    File out1 = "${dataName}.${sex}_pst_eff_a1_b0.5_phiauto_chr${chr}.txt"
  }

  runtime {
    docker: "eu.gcr.io/finngen-sandbox-v3-containers/bioinformatics:1.0.1"
    cpu: 1
    memory: "10 GB"
    disks: "local-disk 50 HDD"
    zones: "europe-west1-b"
  }

}


task prsCS_b {
  
  String chr
  Int n = 361194
  String dataName
	String sex="both_sexes"

	String dataPath
	String prsPath
	String crwPath
  
  File p1
  File p2
  File p3
  File py
  
  File ldnfo
  File ldf
  Array[File] ld_files

	String bim
  File bimFile =     "SANDBOX_RED/${bim}.bim"
  File sumStatFile = "SANDBOX_RED/${dataPath}/${dataName}.${sex}.tsv"

  command {
    python ${py} \
    --ref_dir="${crwPath}/${prsPath}/ldblk_1kg_eur" \
    --bim_prefix="${crwPath}/${bim}" \
    --sst_file="${crwPath}/${dataPath}/${dataName}.${sex}.tsv" \
    --n_gwas=${n} \
    --out_dir="/cromwell_root/${dataName}.${sex}" \
    --chrom=${chr}
  }

  output {
    File out1 = "${dataName}.${sex}_pst_eff_a1_b0.5_phiauto_chr${chr}.txt"
  }

  runtime {
    docker: "eu.gcr.io/finngen-sandbox-v3-containers/bioinformatics:1.0.1"
    cpu: 1
    memory: "10 GB"
    disks: "local-disk 50 HDD"
    zones: "europe-west1-b"
  }

}


