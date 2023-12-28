#Function to get  results (OLD!)
get_df <- function(model, desired_vars) {
  
  
  
  #First, get the predictors contained in the regression model
  model_terms <- model$formula %>% as.character %>% .[3] %>% str_split("\\+", simplify = T) %>% as.character()
  
  #Get the index for desired terms
  undesired_terms <- model_terms %>%
    str_detect(drugs %>% paste0(collapse = "|")) %>%
    {!.} %>%
    which
  
  
  #Get regression table and round numbers
  df <- regressionTable(model, noterms = undesired_terms) %>%
    summary %>% .[["rawTable"]] %>%
    mutate(Pvalue = Pvalue %>% p) %>% 
    filter(Variable %>% str_detect(wanted_drug)) %>%
    mutate(
           across(where(is.numeric), ~ round(., 2) %>% format(nsmall = 2))
  )
  
  #Remove useless units column
  df <- df %>% select(-Units)
  
  
  #Rename columns
  colnames(df) <- c("var", "or", "lci", "uci", "pval")
  
  
  #Create new column for es
  df %>% 
    mutate(es = paste0("ROR: ", or, " [", "95% CI: ", lci, " to ", uci, "]")) %>%
    mutate(table_es = es %>% str_remove_all("ROR: |95% CI: "))
  
}


get_df <- function(model, desired_var) {
  
  #Get P-value (it gets gone when you do confint)
  pval <- emmeans(model, spec = desired_var, contr = "pairwise")$contrasts %>% data.frame %>% spull(p.value)
  
  #Create list with desired order of drug
  desired_order <- list(variable = c(1, 0))
  names(desired_order) <- desired_var[1]
  
  #Run emmeans and do some prettifying
  emmeans(model, spec = desired_var, contr = "pairwise",
          at = desired_order
          )$contrasts %>%
    confint %>%
    data.frame %>%
    rename("or" = "estimate",
           "lci" = "asymp.LCL",
           "uci" = "asymp.UCL") %>%
    mutate(across(.cols = c(or, lci, uci),
                  ~ . %>% exp %>% r2)) %>%
    mutate(pval = pval %>% p,
           var = desired_var) %>% 
    mutate(es = paste0("ROR: ", or, " [", "95% CI: ", lci, " to ", uci, "]")) %>%
    mutate(table_es = es %>% str_remove_all("ROR: |95% CI: ")) %>%
    select(var, or, lci, uci, pval, es, table_es)
    
}

get_df_bint <- function(model, desired_var, int_var) {
  
  #Interaction formula
  int_formula <- paste0("pairwise ~ ", desired_var, "|", int_var) %>% formula
  
  #Get P-value (it gets gone when you do confint)
  pval <- emmeans(model, spec = int_formula)$contrasts %>% data.frame %>% spull(p.value)
  
  #Create list with desired order of drug
  desired_order <- list(variable = c(1, 0))
  names(desired_order) <- desired_var[1]
  
  #Run emmeans and do some prettifying
  emmeans(model, spec = int_formula,
          at = desired_order
          )$contrasts %>%
    confint %>%
    data.frame %>%
    rename("or" = "estimate",
           "lci" = "asymp.LCL",
           "uci" = "asymp.UCL") %>%
    mutate(across(.cols = c(or, lci, uci),
                  ~ . %>% exp %>% r2)) %>%
    mutate(pval = pval %>% p,
           var = desired_var) %>% 
    mutate(es = paste0("ROR: ", or, " [", "95% CI: ", lci, " to ", uci, "]")) %>%
    mutate(table_es = es %>% str_remove_all("ROR: |95% CI: ")) %>%
    select(var, int_var, or, lci, uci, pval, es, table_es)
  
}

#Tidy up strings (sort & unique)
sunique <- function(x, sep = ";") {
  x %>% str_split(sep) %>% lapply(., unique) %>% lapply(., sort) %>% sapply(., paste, collapse = ";")
}



#Calcualte overlap rate between 2 specific adverse events with a certain drug
overlap_n <- function(overlap_var) {
  x[get(drug) == 1 & get(var) == 1 &
      get(overlap_var) == 1] %>% nrow
}



#Get DF from the marginal effects package (MEP)
get_df_mep <- function(model, desired_var) {
  
  #Use marginaleffects to calculate the risk ratio
  df <- avg_comparisons(model, transform_pre = "lnratioavg", variables = var, transform = exp) %>% 
    summary %>%
    data.frame
  
  #Prettify
  df <- df %>%
    rename("rr" = "estimate",
           "lci" = "conf.low",
           "uci" = "conf.high",
           "pval" = "p.value") %>%
    mutate(across(.cols = c(rr, lci, uci),
                  ~ . %>% r2)) %>%
    mutate(var = desired_var) %>% 
    mutate(pval = pval %>% p) %>%
    mutate(es = paste0("RR: ", rr, " [", "95% CI: ", lci, " to ", uci, "]")) %>%
    mutate(table_es = es %>% str_remove_all("RR: |95% CI: ")) %>%
    select(var, rr, lci, uci, es, table_es, pval)
  
  #Get adjusted probability of mortality with and without event
  prob_df <- model %>% avg_predictions(variables = var) %>% data.frame
  df$event_mortality <- prob_df %>% filter(get(var) == 1) %>% spull(estimate) %>% pct_table
  df$noevent_mortality <- prob_df %>% filter(get(var) == 0) %>% spull(estimate) %>% pct_table

  #Return df
  df
  }


#Get props of grouped data
gprop <- function(data) {
  data %>%
    mutate(prop = (N/sum(N)) %>% pr1
                   )
}





#Read SMQ
read_smq <- function(filename) {

  
  #Determine N of rows to skip (some files have one extra row to skip)
  n_to_skip <- readxl::read_xlsx(path = paste0("C:/Ahmed's Stuff/ResearchStuff/FAERS/Meddra Terms/",
                                               filename, ".xlsx")) %>%
    tibble::rownames_to_column() %>%
    filter(...3 == "English" | ...2 == "English" ) %>%
    spull(rowname) %>% as.numeric %>% min
  
  #Read file
  f <- readxl::read_xlsx(path = paste0("C:/Ahmed's Stuff/ResearchStuff/FAERS/Meddra Terms/",
                           filename, ".xlsx"),
                         skip = n_to_skip
                    )
  
  #Clean
  f <- clean_names(f)
  
  #Filter to narrow scope
  f <- f %>% filter(scope == "Narrow")
  
  #Get unique terms
  unique(f$english)
}


#Get official name of SMQ term
smq_term <- function(varname, filename) {

  term <- read_xlsx(path = paste0("C:/Ahmed's Stuff/ResearchStuff/FAERS/Meddra Terms/",
                                               filename, ".xlsx")) %>%
    filter(if_all(1, ~ . %>% str_detect("English"))) %>%
    spull(1) %>%
    str_remove("English:") %>%
    str_trim
  
  data.frame(ae_vars, ae_full_vars) %>% 
    filter(ae_vars == varname) %>%
    cbind(., term)
}

nonsmq_term <- function(varname, filename) {
  
  term <- readxl::read_xlsx(path = paste0("C:/Ahmed's Stuff/ResearchStuff/FAERS/Meddra Terms/",
                                            filename, ".xlsx"), skip = 3) %>% 
    clean_names %>%
    spull(english) %>%
    unique %>%
    paste0('"', ., '"', collapse = ", ") 
  
  
  data.frame(ae_vars, ae_full_vars) %>% 
    filter(ae_vars == varname) %>%
    cbind(., term)
}

toolong_terms <- function(varname, term) {
  
  data.frame(ae_vars, ae_full_vars) %>% 
    filter(ae_vars == varname) %>%
    cbind(., term)
  
}

#Read non-SMQ (PT) files
read_nonsmq <- function(filename) {
  
  #Read file
  f <- readxl::read_xlsx(path = paste0("C:/Ahmed's Stuff/ResearchStuff/FAERS/Meddra Terms/",
                                       filename, ".xlsx"),
                         skip = 3
  )
  
  #Clean
  f <- clean_names(f)
  
  #Escape brackets
  f$english <- f$english %>%
    str_replace("\\(", "\\\\(") %>%
    str_replace("\\)", "\\\\)")
  
  #Get unique terms
  unique(f$english)
  
}
