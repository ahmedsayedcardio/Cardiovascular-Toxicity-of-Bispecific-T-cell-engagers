#Get knots
knots_locs <- rcs_knots(x$age_yr, n_knots = 5)

#List AEs
ae_vars_analysis <- c(ae_vars, "cvae",  "dcvae",  "ncvae")
ae_full_vars_analysis <- c(ae_full_vars, 
                           "CVAE", "Fatal CVAE",
                           "Non-CVAE")


sex_ints_df <- data.frame()
#Designate drug names
drugs <- c("bite", "bli", "tec", "glo", "mos")
full_drugs <- c("BiTE therapy", "Blinatumomab", "Teclistamab", "Glofitamab", "Mosunetuzumab")
#Designate disease
diseases <- c("ll_mm", "ll", "mm", "ll", "ll")

drug = "bite"
var = "bleed"
full_var = "Bleeding"
full_drug = "BiTE therapy"
disease = "ll_mm"

#Model 
dfs <- foreach(drug = rep(drugs, times = length(ae_vars_analysis)),
               var = rep(ae_vars_analysis, each = length(drugs)),
               full_var = rep(ae_full_vars_analysis, each = length(drugs)),
               full_drug = rep(full_drugs, times = length(ae_vars_analysis)),
               disease = rep(diseases, times = length(ae_vars_analysis))
) %dopar% {
  
  
  #May need to define functions here specifically?
  source("Specific Functions for FAERS.R", local = TRUE)
  
  
  #Create aggregated dataframe
  x[, .(.N), by = .(cvae_comorbid, anthra, btki, get(disease), age_yr, sex, get(drug), get(var))] %>% na.omit -> ans
  ans <- ans %>% rename(disease := get,
                        !!drug := get.1,
                        !!var := get.2)
  
  #factor diseasea
  ans$disease <- factor(ans$disease)
  #Create age_term
  age_term <- paste0("ns(age_yr, knots = ",  knots_locs %>% paste0(collapse = ", ") %>% paste0("c(", ., ")"), ")")
  #Set variables to adjust for
  adjustment <- paste0(" + disease*", age_term, " + sex*disease + btki + anthra + sex *", age_term)
  
  #Create formula for
  #Null model
  m0_formula <- paste0(var, " ~ ", adjustment) %>% formula
  #Standard model
  m1_formula <- paste0(var, " ~ ", drug, adjustment) %>% formula
  #Interactions model (age)
  m2_formula <- paste0(var, " ~ ", drug, " * ", age_term, adjustment) %>% formula
  #Interactions model (sex)
  m3_formula <- paste0(var, " ~ ", drug, "* sex", adjustment) %>% formula
  #Interactions model (sex)
  m4_formula <- paste0(var, " ~ ", drug, "* cvae_comorbid", adjustment) %>% formula
  
  #Run
  m0 <- glm(data = ans, formula = m0_formula, weights = N, family = binomial)
  m1 <- glm(data = ans, formula = m1_formula, weights = N, family = binomial)
  
  
  #Create data.frame
  df <- get_df(m1, desired_var = drug)
  
  #Create outcome & drug column
  df$outcome <- var
  df$outcome_full <- full_var
  df$drug_full <- full_drug
  
  # #Get total N of AEs
  df$n_ae <- x[get(drug) == 1 & get(var) == 1] %>% nrow
  df$mortality_denom <- x[get(drug) == 1 & get(var) == 1 & !is.na(death)] %>% nrow
  
  # #Get AE as a prop of total AEs
  df$ae_prop <- (df$n_ae/x[get(drug) == 1] %>% nrow) %>% pr1
  
  # #Add fatality rate
  df$n_death <- overlap_n("death")
  df$prop_death <- (x[get(drug) == 1 & get(var) == 1 & death == 1] %>% nrow
                    /x[get(drug) == 1 & get(var) == 1 & !is.na(death)] %>% nrow) %>%
    pr1
  
  #Add crs overlap
  df$n_crs <- overlap_n("crs")
  df$prop_crs <- (df$n_crs/df$n_ae) %>% pr1
  
  #Add CVAE comorbidity overlap
  df$n_comorbid_cvae <- overlap_n("cvae_comorbid")
  df$prop_comorbid_cvae <- (df$n_comorbid_cvae/df$n_ae) %>% pr1
  
  
  #Add infection overlap
  df$n_inf <- overlap_n("infection")
  df$prop_inf <- (df$n_inf/df$n_ae) %>% pr1

  
  #Assume no age/sex interaction (to be replaced later if there is)
  df$imp_age_int <- FALSE
  df$imp_sex_int <- FALSE
  df$age_int <- "N/A"
  df$sex_int <- "N/A"
  
  #Produce interaction plots if significant
  if(df$n_ae > 10 & df$or > 1 & var %in% c("cvae", "dcvae") & drug == "bite") {
    
    #Run age int model
    m2 <- glm(data = ans, formula = m2_formula, weights = N, family = binomial)
    #Compare using anova
    age_int_p <- anova(m2, m1, test = "LRT")$`Pr(>Chi)`[2]
    #Format P properly
    df$age_int <- age_int_p %>% p
    
    if(age_int_p %>% sig) {
      
      #Signify that this is an age interaction of interest
      df$imp_age_int <- TRUE
      
      
      #Set age limits
      age_limits <- x[get(drug) == 1, age_yr] %>% quantile(c(0.05, 0.95), na.rm = TRUE)
      
      #Get ORs across various levels of age_yr
      if(drug == "bli") {
        ors <- emmeans(m2, pairwise ~ bli|age_yr,
                       at = list(age_yr = seq(age_limits[1], age_limits[2], 0.5), bli = c(1, 0))) %>%
          confint
      } else if(drug == "tec") {
        ors <- emmeans(m2, pairwise ~ tec|age_yr,
                       at = list(age_yr =seq(age_limits[1], age_limits[2], 0.5), tec = c(1, 0))) %>%
          confint
      } else if(drug == "bite") {
        ors <- emmeans(m2, pairwise ~ bite|age_yr,
                       at = list(age_yr =seq(age_limits[1], age_limits[2], 0.5), bite = c(1, 0))) %>%
          confint
      }
      
      #Get the DF for ORs from this object
      ors <- ors$contrasts
      #Rename to make for easier referencing
      ors <- ors %>% rename("lci" = "asymp.LCL",
                            "uci" = "asymp.UCL")
      
      #Mutate to exponentiate
      ors <- ors %>% mutate(across(.cols = c(estimate, lci, uci),
                                   ~ . %>% exp))
      
      #Make a nice ggplot
      source("Interactions plot.R", local = TRUE)
      
    }
  }
  
  #Produce interaction plots if significant
  if(df$n_ae > 10 & df$or > 1 & var %in% c("cvae", "dcvae") & drug == "bite") {
    
    #Run sex int model
    m3 <- glm(data = ans, formula = m3_formula, weights = N, family = binomial)
    #Compare using anova
    sex_int_p <- anova(m3, m1, test = "LRT")$`Pr(>Chi)`[2]
    #Format P properly
    df$sex_int <- sex_int_p %>% p
    
    if(sex_int_p %>% sig) {
    
    
    #Signify that this is a sex interaction of interest
    df$imp_sex_int <- TRUE
    
    
    #Create sex_dfs dataframe
    sex_df <- get_df_bint(m3, desired_var = drug, int_var = "sex")
    
    #Create outcome & drug column
    sex_df$outcome <- var
    sex_df$outcome_full <- full_var
    sex_df$drug_full <- full_drug
    
    #Assign to a dataframe
    sex_ints_df <- bind_rows(sex_ints_df, sex_df)
    
    #Assign variable
    assign("sex_ints_df", sex_ints_df, envir = .GlobalEnv)
    }
    
    
  }
  
  #Print df
  df
  
  
  
}



#Main Results df
mres_df <- rbindlist(dfs)
mres_df %>% filter( (pval < 0.05 | pval == "<0.001") & or > 1 & outcome %in% c(cvae_vars, "dcvae"))  %>% View

#Convert sex ints to data.table
sex_ints_df <- as.data.table(sex_ints_df)
