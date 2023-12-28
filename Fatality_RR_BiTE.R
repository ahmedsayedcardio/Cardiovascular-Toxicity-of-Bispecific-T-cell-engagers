#Get DF of AEs which had at least 1 event
f_rr_df <- mres_df %>% filter(mortality_denom >= 10)
outcomes <- f_rr_df$outcome
outcomes_full <- f_rr_df$outcome_full
drugs <- f_rr_df$var
drugs_full <- f_rr_df$drug_full


#Model 
dfs <- foreach(drug = drugs,
               var = outcomes,
               full_var = outcomes_full,
               full_drug = drugs_full
) %do% {
  
  
  #May need to define functions here specifically?
  source("Specific Functions for FAERS.R", local = TRUE)
  
  
  #Create aggregated dataframe
  ans <- x[get(drug) == 1]
  ans <- ans[, .(.N), by = .(age_yr, sex, get(var), death)] %>% na.omit
  ans <- ans %>% rename(!!var := get)

  
  #Set variables to adjust for
  adjustment <- " + ns(age_yr, 5) + sex"
  
  #Create formula for
  #Null model
  m0_formula <- paste0("death", " ~ ", adjustment) %>% formula
  #Standard model
  m1_formula <- paste0("death", " ~ ", var, adjustment) %>% formula
  
  #Run
  m0 <- glm(data = ans, formula = m0_formula, weights = N, family = binomial)
  m1 <- glm(data = ans, formula = m1_formula, weights = N, family = binomial)
  
  
  #Run anova to get Pvalue
  anova_pval <- anova(m1, m0, test = "LRT")$`Pr(>Chi)`[2] %>% p
  
  #Create data.frame
  df <- get_df_mep(m1, desired_var = var)
  
  #Add pval
  df$anova_pval <- anova_pval
  
  #Add drug and AE data
  df$ae <- var
  df$ae_full <- full_var
  df$drug <- drug
  df$drug_full <- full_drug
  
  #Check for contradiction between marginal p and model p
  df$pc <- sig(df$pval) == sig(df$anova_pval)
  
  #Print it out
  df
}



#Main Results df
mort_rr_df <- rbindlist(dfs)
mort_rr_df %>% View
# 
# ans %>% filter(age_yr > 50) %>% group_by(bli, va) %>% summarise(n = sum(N)) %>% mutate(prop = n/sum(n)) -> data
# data
# glm(data = data, formula = hf ~ bli, weights = n, family = binomial) %>% tidy(exp = T)
# 
# glm(data = data, formula = hf ~ bli + sex + rcs(age_yr, 7), weights = n, family = binomial) %>% tidy(exp = T)
