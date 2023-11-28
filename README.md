# cabg_prs_sex
Code for the project: "Sex-Specific Genetic Risks for Adverse Outcomes  After Coronary Revascularization Procedures "

We studied the sex-specific differences in associations of polygenic risk scores (PRSs) with atrial fibrillation, ischemic stroke, intracranial hemorrhage, myocardial infarction, and gastrointestinal hemorrhage in coronary revascularization patients. 

* Data: FinnGen https://www.finngen.fi/en
* Description of variables: https://risteys.finngen.fi/
* Description of registries: https://finngen.gitbook.io/finngen-analyst-handbook/finngen-data-specifics/finnish-health-registers-and-medical-coding
* Data access: https://site.fingenious.fi/en/
* PRS values were calculuted for FinnGen individuals using PRS-CS pipeline with default settings: https://github.com/getian107/PRScs
* We used GWAS summaries from UKBB GWAS v3: https://docs.google.com/spreadsheets/d/1kvPoupSzsSFBNSztMzl04xMoSC3Kcx3CrjVf4yBmESU/edit#gid=227859291

```
ht_prs_preg
├── README.md                   # Overview
├── cabg_af_prs_sex3.rmd        # R markdown for the analysis
├── scripts
  ├── functions2.R                     # Minor R functions for the analysis
  ├── prs_calculations                 # Directory: PRS calculations
	├── preprocess_ukb_for_prscs.bash	# Preprocesses ukb gwas summary statistics for PRS-CS
	├── prs_ukb3.wdl			# Runs PRS-CS at FinnGen for one endpoint and all sexes.
	├── generate_prs_scores.bash		# Processes PRS_CS output and calculates weights for FinnGen individuals
	├── reformat_raw_prs.R 			# Is required by 'generate_prs_scores.bash'

```
