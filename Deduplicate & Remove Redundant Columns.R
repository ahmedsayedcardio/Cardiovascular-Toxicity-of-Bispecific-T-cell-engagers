#Remove redundant columns
x[, c("event_dt", 
      "start_dt", "age_cod", "outc_cod", 
      "drug_therapy_link", "drug_seq",
      "caseid", "primaryid", "caseversion", 
      "pt", "indi_pt") := NULL]



#Get SS and run 2nd deduplicate
ss_with_age_sex_ind <- nrow(x) %>% comma #Equivalent to SS with age, sex, and indication available
x <- unique(x, by = c("age", "sex", "reporter_country", "event_date", "all_drugs", "ae")) 
pre_final_ss <- nrow(x) %>% comma

#Remove if AE == Off-label only OR AE = Indication OR AE = Death
x <- x[!(ae == (";Off label use;") | ae == inds)]
x <- x[ae != ";Death;"]
final_ss <- nrow(x) %>% comma
