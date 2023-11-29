
#  Plots
#---------

#Function for my km curves using ggsurvplot

my_ggkmplot <- function(myfit, title, labels, conf.int=F, fun="event", xlim=c(1,30), nr_col=5){
  
  
  labels=labels
  palette5 = c("#92C5DE","#0571B0","#756BB1","#CA0020", "#F4A582")
  palette4 = c("#0571B0","#92C5DE","#F4A582","#CA0020")
  palette3 = c("#0571B0","#756BB1","#CA0020")
  if(nr_col==5){palette=palette5}
  else if(nr_col==4){palette=palette4}
  else if(nr_col==3){palette=palette3}
  
  
  ggsurvplot(myfit,
             fun = fun, conf.int = conf.int, censor = F,
             title = title,  size = 0.5, xlim=xlim,
             palette = palette, ggtheme = theme_bw(), 
             legend = "right", legend.title = "PRS",  legend.labs=labels)
}


# Cox plot using ggsurvplot from survminer package

my_ggcoxplot <- function(fit, new_df, title, labels, conf.int=F, xlim=c(0,30), ylim=c(0,1), legend="right", 
                         ylab="Cumulative event", xlab="Years", break.time.by=5, break.y.by=.2, nr_col=5, legend.title="PRS"){
  
  labels=labels
  palette5 = c("#92C5DE","#0571B0","#756BB1","#CA0020", "#F4A582")
  palette4 = c("#0571B0","#92C5DE","#F4A582","#CA0020")
  palette3 = c("#0571B0","#756BB1","#CA0020")
  if(nr_col==5){palette=palette5}
  else if(nr_col==4){palette=palette4}
  else if(nr_col==3){palette=palette3}
  
  ggsurvplot(fit, data = new_df,
             fun = "event", conf.int = conf.int, censor = F, 
             xlim=xlim, break.time.by = break.time.by, ylim=ylim, break.y.by=break.y.by,
             title = title,  size = 0.5, 
             palette = palette, ggtheme = theme_bw(), 
             legend = legend, legend.title = legend.title,  legend.labs=labels,
             xlab = xlab, ylab = ylab, 
             font.tickslab=9.5, font.legend=9.5)
  
}

my_ggcoxplot_defcol <- function(fit, new_df, title, labels, conf.int=F, xlim=c(0,30), ylim=c(0,1), legend="right", 
                         ylab="Cumulative event", xlab="Years", break.time.by=5, legend.title="PRS"){
  
  labels=labels
  
  ggsurvplot(fit, data = new_df,
             fun = "event", conf.int = conf.int, censor = F, 
             xlim=xlim, break.time.by = break.time.by, ylim=ylim,
             title = title,  size = 0.5, 
             ggtheme = theme_bw(), 
             legend = legend, legend.title = legend.title,  legend.labs=labels,
             xlab = xlab, ylab = ylab, 
             font.tickslab=9.5, font.legend=9.5)
  
}


my_ggcoxplot2 <- function(fit, new_df, title, labels, conf.int=F, xlim=c(0,30), ylim=c(0,1), legend="right", 
                         ylab="Cumulative event", xlab="Years", break.time.by=5, legend.title="PRS"){
  
  palette3 = c("#0571B0","#756BB1","#CA0020")
  palette=c(palette3, palette3)
  linetype=c(rep("dashed",3), rep("solid",3))
  
  ggsurvplot(fit, data = new_df,
             fun = "event", conf.int = conf.int, censor = F, 
             xlim=xlim, break.time.by = break.time.by, ylim=ylim,
             title = title,  size = 0.5, 
             palette = palette, ggtheme = theme_bw(), 
             legend = legend, legend.title = legend.title,  legend.labs=labels,
             xlab = xlab, ylab = ylab, linetype = linetype,
             font.tickslab=9.5, font.legend=9.5)
  
}


gener_lgnd_myggcx_scr <- function(prs_q, scr_q, prs_name, scr_name ) {
  
  PRS <-c(prs_q, prs_q)
  CV  <-c(rep(scr_q[1],3), rep(scr_q[2],3))
  tmp <- data.frame(PRS, CV, x=c(1:12),y=c(c(1:6),c(2:7)))
  palette3 <- c("#0571B0","#756BB1","#CA0020")
  
  tmp_p <- 
    ggplot(tmp, aes(x,y, color = PRS, linetype=CV)) +
    geom_line() + 
    scale_color_manual(values=palette3) +
    labs(color = prs_name, linetype = scr_name)
  
  get_legend(tmp_p)
  
}


#Draws histograms of endpoints by age. Cases and controls are coloured separately-

my_endpoint_histograms <-  function(endpoints_all, endpoint_ages_all, df=df, ncol=4, xlab="age", title=""){

  df_long_ep <-   df %>%
    select("FINNGENID", all_of(endpoints_all)) %>%
    pivot_longer(cols=endpoints_all, names_to = "endpoint",values_to = "status")
  
  df_long <-   df %>%
    select("FINNGENID", all_of(endpoint_ages_all)) %>%
    pivot_longer(cols=endpoint_ages_all, names_to = "endpoint",values_to = "age") %>%
    mutate(endpoint = str_replace(endpoint,"(\\w+)_.+", "\\1")) %>%
    left_join(df_long_ep, by=c("FINNGENID", "endpoint"))
  
  ggplot(df_long, aes(x=age, color=as.factor(status), fill=as.factor(status)), xlab = xlab) +
    geom_histogram(alpha=0.1, position="identity") +
    theme(legend.title = element_blank()) +
    facet_wrap(~endpoint, ncol=ncol) +
    xlab(xlab) +
    labs(title=title)
  
}


#Draws histograms of endpoints by age. Cases and controls are coloured separately-

my_endpoint_densities <-  function(endpoints_all, endpoint_ages_all, df=df, ncol=4, xlab="age", title=""){
  
  df_long_ep <-   df %>%
    select("FINNGENID", all_of(endpoints_all)) %>%
    pivot_longer(cols=endpoints_all, names_to = "endpoint",values_to = "status")
  
  df_long <-   df %>%
    select("FINNGENID", all_of(endpoint_ages_all)) %>%
    pivot_longer(cols=endpoint_ages_all, names_to = "endpoint",values_to = "age") %>%
    mutate(endpoint = str_replace(endpoint,"(\\w+)_.+", "\\1")) %>%
    left_join(df_long_ep, by=c("FINNGENID", "endpoint"))
  
  ggplot(df_long, aes(x=age, color=as.factor(status), fill=as.factor(status)), xlab = xlab) +
    geom_density(alpha=0.1)+
    theme(legend.title = element_blank()) +
    facet_wrap(~endpoint, ncol=ncol) +
    xlab(xlab) +
    labs(title=title)
  
}

#Draws facet wrapped histograms

my_histo_wrap <-  function(var_names, df=df, xlab, ncol=4){

  df_long <-   df %>%
    select(all_of(var_names)) %>%
    pivot_longer(cols=all_of(var_names), names_to = "names", values_to = "values")
  
  ggplot(data=df_long, aes(values), xlab=xlab) +
    geom_histogram(alpha=0.85) +
    theme(legend.title = element_blank()) +
    facet_wrap(~names, ncol=4)
}


# Tables
#-------

#Creates summary table from subset dataframe.

my_subset_sum_table <- function(df, name_main){
  
df %>% 
  group_by(NAME) %>%
  summarise(n=sum(EP, na.rm=T), N=n(), 
            n_fem = sum(SEX_IMPUTED*EP, na.rm=T), fem_perc = sum(SEX_IMPUTED*EP, na.rm=T)/n*100, 
            oper_age = mean(get(str_glue("{name_main}_AGE"))), sd_oper_age = sd(get(str_glue("{name_main}_AGE"))), 
            age_diff=mean(AGE_DIFF), sd_age_diff=sd(AGE_DIFF)) %>%
    rename(!!name_main := NAME) %>%
    knitr::kable(digits=2)
}


my_char_table <- function(dfi, strata, myVars, catVars, rownames, name=""){
  
  df_tmp <- dfi
  df_tmp[[strata]] <- factor(as.factor(df_tmp[[strata]]), levels=c(1,0), labels=c(strata, str_glue("No {strata}")))
  
  chr_strat <- 
    CreateTableOne(vars=myVars, data=df_tmp, factorVars = catVars, strata=strata, test=F) %>%
    print(dropEqual=T, printToggle=F)
  
  chr_all <- 
    CreateTableOne(vars=myVars, data=df_tmp, factorVars = catVars, test=F) %>%
    print(dropEqual=T, printToggle=F)
  
  table <- cbind(chr_all, chr_strat)
  rownames(table) <- c(c(name, chr_rownames))
  table
  
  #cbind(chr_all, chr_strat) %>%
    #set_rownames(c(name, chr_rownames)) 
}


# Extract coefficients and ci's

my_extr_coef <- function(cx, mytitle="title", select){
  summary(cx)$coefficients[,c(2,5)] %>%
    cbind(row.names(.),.,summary(cx)$conf[,3:4]) %>%
    as_tibble() %>%
    rename(names=V1, est="exp(coef)", pval="Pr(>|z|)",  lower="lower .95", upper="upper .95") %>%
    mutate_at(c("est","pval","lower","upper"), as.numeric) %>%
    filter(str_detect(names, select)) %>%
    mutate(name=mytitle)
}




# Extracts number of cases and controls

extract_ns2 <- function(endpoint, score_name, df) {
  
  df %>%
    select(all_of(score_name), all_of(endpoint)) %>%
    rename_(quantile = "score_name", endpoint = "endpoint") %>%
    drop_na %>%
    group_by(quantile, endpoint) %>%
    summarise(n=n()) %>%
    spread(endpoint, n) %>%
    rename(cases = "1", controls="0") %>%
    ungroup() %>%
    mutate(endpoint = endpoint,
           score_name = score_name) %>%
    select(endpoint, score_name, quantile, controls, cases) #%>%
    #add_row(endpoint=endpoint, score_name=score_name, quantile=title, controls=NA, cases=NA, .before = 1)

}





#Takex list of cx-rsults and returns a tibble with main results


my_cxlist_to_hrtable <- function(cxlist, ep_title="Endpoint", n_pval=length(cxlist)){
  
  #Input: list of cx-results
  #Requires my_exr_coef() and my_tidy_table()
  
  names <- as.list(names(cxlist), n_pval=length(cxlist))
  
  tmp_list <- lapply(names, function(name){
    my_extr_coef(cxlist[[name]], mytitle=name , select=paste0(name,"_") )})
  
  bind_rows(tmp_list) %>%
    mutate(p.adj=p.adjust(pval, method = "fdr", n=n_pval)) %>%
    my_tidy_est2() %>%
    select(name, est_ci, pval, p.adj) %>%
    rename(!!ep_title := name, `HR (95% CI)` = est_ci, `P-value` = pval, FDR=p.adj)
}

#Takex list of cx-rsults and returns a tibble with selected results

my_cxlist_to_hrtable_selected <- function(cxlist, ep_title="Endpoint", select="_CAT", title="Variable"){
  
  #Input: list of cx-results
  #Requires my_exr_coef() and my_tidy_table()

    names <- as.list(names(cxlist))
    tmp_list <- lapply(names, function(name){
    my_extr_coef(cxlist[[name]], mytitle=name , select=select)%>%
    add_row(names="", est=NA, pval=NA, lower=NA, upper=NA, name="", .before=1)
    })
  
    
  bind_rows(tmp_list) %>%
    my_tidy_table(est_repl = "", p_repl="") %>%
    select(name, names, est, pval) %>%
    rename(!!ep_title := name, !!title := names, `HR (95% CI)` = est, `P-value` = pval)
}


my_cxlist_to_hrtable_cat <- function(cxlist, dflist, ep_title="Endpoint", select="_CAT"){
  
  #Input: list of cx-results
  #Requires my_exr_coef() and my_tidy_table()
 
  names_ep <- as.list(names(cxlist))
  
  lapply(names_ep, function(ep){
    
    cat_name <- str_glue("{ep}_CAT")

    tmp <- my_extr_coef(cxlist[[ep]], select= cat_name, mytitle=ep) %>%
      mutate(names = str_remove(names, cat_name))
    
    extract_ns2(ep, cat_name, dflist[[ep]]) %>%
      left_join(tmp, by = c("quantile" = "names")) %>%
      my_tidy_table() %>%
      select(endpoint, quantile, cases, controls, est, pval) %>% 
      add_row(endpoint="", quantile="", cases=NA, controls=NA, est="", pval=NA, .before=1)
    
  }) %>%
  bind_rows() %>%
  rename(!!ep_title := endpoint, PRS=quantile, `HR (95% CI)` = est, `P-value` = pval, Cases=cases, Controls=controls) %>%
  mutate_all(replace_na,"")

}



# Combines estimates and ci's to same column, rounds some values, replaces NA's etc. 
#   Input table 'res.df' must have columns "est", "lower", "upper", "pval"

my_tidy_table <- function(res.df, est_repl = "-", p_repl = "-", est_dec = 2){
  
  res.df %>%
    mutate_at(c("est", "lower", "upper", "pval"),  as.numeric) %>%
    mutate_at(c("est", "lower", "upper"), round, est_dec) %>%
    mutate_at("pval", signif, 2) %>%
    mutate(est = str_glue("{est} ({lower}-{upper})"),
           est = str_replace(est, "^N.*", est_repl),
           pval = if_else(pval < 1e-300, 0, pval), 
           #pval = replace_na(pval, p_repl),
           pval = str_replace(pval, "^0$", "<1e-300")
    )%>%
    #mutate_at(c("est", "lower", "upper", "pval"),  replace_na, "-") %>%
    select(-"lower",-"upper")
  
}


my_tidy_est2 <- function(table, digits = 2, sign =2){
  
  table %>%
    mutate_at(c("est", "lower", "upper", "pval", "p.adj"), as.numeric) %>%
    mutate_at(c("est","lower", "upper"), list(r=~round(.,digits))) %>%
    mutate_at(c("pval", "p.adj"), ~signif(., sign)) %>%
    mutate(est_ci = str_glue("{est_r} ({lower_r}-{upper_r})")) %>%
    select(-contains("_r")) %>%
    mutate_at(c("est_ci", "pval", "p.adj"), as.character)
  
}    


my_cx_to_hrtable_cat <-  function(cx, dfi, ep_name, select="_CAT", title=""){
  
  nN_tot <- dfi %>% 
    summarize(sum(get(ep)), n()-sum(get(ep))) %>%
    unite("nN", sep="/")
  
  cat_name <- str_glue("{ep_name}_CAT")
  
  tmp <- my_extr_coef(cx, select= select, mytitle=title) %>%
    mutate(names = str_remove(names, cat_name))
  
 test<-  extract_ns2(ep_name, cat_name, dfi) %>%
    left_join(tmp, by = c("quantile" = "names")) %>%
    my_tidy_table() %>%
    unite("nN", c("cases","controls"), sep="/") %>%
    select(quantile, est, pval, nN) %>% 
    add_row(quantile=title, nN=nN_tot[[1,1]], est="", pval=NA, .before=1) %>%
    rename(PRS=quantile, `HR (95% CI)` = est, `P-value` = pval, `Cases/Controls` = nN) %>%
    mutate_all(replace_na,"")
}


my_cxlist_to_hrtable_list_sex <- function(names_oper, names_ep, dflist, cxlist, 
                                          exclude="none", n_pval=length(names_oper)*length(names_ep)) {
  
  hr_table <-
    lapply(names_oper, function(oper){
      
      #n's extracted
      nN.f <- lapply(names_ep, function(ep){
        dflist[["f"]][[oper]][[ep]] %>% 
          summarise(n=sum(get(ep), na.rm = T), N=n()-n) %>%
          transmute(`n/N` =  str_glue("{n}/{N}")) %>%
          rename(`Cases/Controls` = "n/N")
      }) %>% do.call(rbind,.)
      
      nN.m <- lapply(names_ep, function(ep){
        dflist[["m"]][[oper]][[ep]] %>% 
          summarise(n=sum(get(ep), na.rm = T), N=n()-n) %>%
          transmute(`n/N` =  str_glue("{n}/{N}")) %>%
          rename(`Cases/Controls` = "n/N")
      }) %>% do.call(rbind,.)
      
      
      #Sex interactions 
      int.pvals <- lapply(names_ep, function(ep){
        my_extr_coef(cxlist[["i"]][[oper]][[ep]], select=":SEX")
      }) %>% 
        do.call(rbind,.) %>%
        select(pval) %>%
        mutate(p.adj = p.adjust(pval, method = "fdr", n=n_pval))%>%
        mutate(pval=signif(as.numeric(pval),2),pval=as.character(pval)) %>%
        mutate(p.adj=signif(as.numeric(p.adj),2),p.adj=as.character(p.adj)) %>%
        rename(`P-value` = pval, FDR=p.adj)
      
      title <- str_remove(oper, "^.*_")
      if(title == oper){title = " "}
      
      hrs.f <- my_cxlist_to_hrtable(cxlist[["f"]][[oper]], ep_title = title, n_pval=n_pval) %>% as.matrix() %>% as_tibble()
      hrs.m <- my_cxlist_to_hrtable(cxlist[["m"]][[oper]], ep_title = title, n_pval=n_pval) %>% as.matrix() %>% as_tibble()
      
      #When only one endpoint, this does not work
      #hrs.f <- hrs.f[title=(hrs.f[,1] != exclude), ] %>% as_tibble()
      #hrs.m <- hrs.m[title=(hrs.m[,1] != exclude), ] %>% as_tibble()
      
      #Sex specific HR
      cbind(hrs.f[,1], nN.f, hrs.f[,2:4], nN.m, hrs.m[,2:4], int.pvals)
      
    }) %>% do.call(rbind,.)
  
}



prettify_estimate <- function(estimate, stderr, r) {
  rounded_estimate <- signif(estimate, r)
  rounded_ci_low   <- signif(estimate - 1.96 * stderr, r)
  rounded_ci_high  <- signif(estimate + 1.96 * stderr, r)
  pretty_string    <- paste0(rounded_estimate, " (", rounded_ci_low, "-", rounded_ci_high, ")")
  return(pretty_string)
}


#Other functions
#---------------


#Creates sub datasets for each endpoint
#PREVALENT CASES REMOVED (endpoint before operation).
my_create_sub_dfs <- function(main_name, df_list_main, ep_name_list, covs){
  
  ep_df_list <- lapply(ep_name_list, function(name) {
    df_list_main[[main_name]] %>% 
      select(FINNGENID, contains(main_name), contains(unlist(name)), contains("CVSC"), contains("preop"), FU_END_AGE, all_of(covs)) %>%
      mutate(AGE_DIFF=get( str_glue("{name}_AGE"))-get(str_glue("{main_name}_AGE"))) %>%
      filter(AGE_DIFF > 0)
  })
  names(ep_df_list) <- ep_name_list
  ep_df_list
} #tästä voisi ottaa loopin pois ja korostaa funktion nimessä prevalenttien poistoa.
#parempi loopata pääskriptissä. 


my_generate_diffs_longit <- function(ep, oper, longit_df, df, covs){
  
  #Search next event after exposure from longitudial data and calculate time differences.
  longit_df %>%
    filter(ENDPOINT == "ENDPOINT" | ENDPOINT == ep) %>%
    select(-ENDPOINT) %>%
    rename(!!str_glue("{ep}_AGE") := EVENT_AGE, !!str_glue("{ep}_YEAR") := EVENT_YEAR) %>%
    #distinct() %>%
    left_join(select(df, FINNGENID, oper, str_glue("{oper}_AGE"))) %>%
    filter(get(oper) == 1) %>%
    mutate(AGE_DIFF = get(str_glue("{ep}_AGE")) - get(str_glue("{oper}_AGE"))) %>%
    filter(AGE_DIFF > 0) %>%
    group_by(FINNGENID) %>%
    summarise(AGE_DIFF=min(AGE_DIFF), !!str_glue("{ep}_AGE"):=min(get(str_glue("{ep}_AGE"))), 
              !!str_glue("{ep}_YEAR"):=min(get(str_glue("{ep}_YEAR")))) %>%
    right_join(select(df, FINNGENID, contains(oper), 
                      str_glue("{ep}_SCORE"), str_glue("{ep}_SCALED"),str_glue("{ep}_CAT"), all_of(covs), FU_END_AGE)) %>% 
    mutate(!!str_glue("{ep}_AGE"):= if_else(is.na(AGE_DIFF), FU_END_AGE, get(str_glue("{ep}_AGE"))), 
           !!ep := if_else(is.na(AGE_DIFF),  0, 1),
           AGE_DIFF = if_else(is.na(AGE_DIFF), (FU_END_AGE  - get(str_glue("{oper}_AGE"))), AGE_DIFF )) 
} 
#Kun laittaa silmukkaan, raskasta saman tiedoston moneen kertaan lukemista, parempi olisi lähteä hajottamaan longit_df:ää
#moneen osaan kerralla. Jos tekisi ensin sen taulukkoon tarvittavan long-format dataframen, jossa eri datasetit päällekäin
#ja sitä jakaisi.


#List of dataframes transformed to dataframe.
#Endpointnames are given name "EP"_* and new 'NAME' column is created.
my_list_to_df <- function(df_list){
  
  my_names <- names(df_list)
  tmp_list <- lapply(my_names, function(name){
    df_list[[name]] %>% 
      rename_at(vars(contains(name)), list(~str_replace(.,name, "EP"))) %>%
      mutate(NAME=as.factor(name))
  })
  do.call(rbind, tmp_list)
}


#Pairs quantile 20-80 with other quantiles for pairwise restricted mean survival time runs

my_extr_cats2 <- function(quant, cat, df, endpoints=c("I9_HYPTENS_AGE", "I9_HYPTENS")){
  
  tmp <- df %>% 
    rename_(CAT = cat) %>%
    filter(CAT == "20-80%") %>%
    mutate(CAT = 0)
  
  df %>%
    rename_(CAT = cat) %>%
    filter(CAT == quant) %>%
    mutate(CAT = 1) %>%
    bind_rows(tmp) %>%
    select(all_of(endpoints), CAT) %>%
    droplevels() %>%
    drop_na() 
  
} 



my_expand_covs <- function(df, batch) {
  
  expand.grid(
    batch = batch,
    BL_YEAR = round(mean(df$BL_YEAR)),
    PC1 = mean(df$PC1),
    PC2 = mean(df$PC2),
    PC3 = mean(df$PC3),
    PC4 = mean(df$PC4),
    PC5 = mean(df$PC5),
    PC6 = mean(df$PC6),
    PC7 = mean(df$PC7),
    PC8 = mean(df$PC8),
    PC9 = mean(df$PC9),
    PC10 = mean(df$PC10)
  ) 
}


my.kable <- function(my.table) {
  my.table %>%
    knitr::kable() %>%
    kable_classic() %>%
    row_spec(0,bold=TRUE)
  
}


