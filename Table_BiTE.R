table_drugs <- c("BiTE therapy", "Blinatumomab", "Teclistamab")
tables <- foreach(full_drug = table_drugs,
                  spaces_n = 0:(length(table_drugs) - 1)
) %do% {
  
  #Prettify the desired columns
  rtable <- mres_df %>% 
    mutate_all(~ replace(., is.nan(.), "N/A")) %>%
    filter(outcome %in% c("cvae", "dcvae", cvae_vars)) %>%
    filter(drug_full == full_drug) %>%
    select(outcome_full, table_es, n_ae, prop_death, prop_crs, prop_comorbid_cvae) %>%
    mutate(across(.cols = c(table_es, prop_death, prop_crs), ~ ifelse(n_ae == 0, "N/A", .x)
                  )) %>%
    rename(Outcome = outcome_full,
           "Reporting Odds Ratio [95% CI]" = table_es,
           "N" = n_ae,
           "Proportion of events\nresulting in death" = prop_death,
           "Proportion of events\nco-occurring with CRS (%)" = prop_crs,
           "Proportion of events\nco-occurring with CV comorbidities (%)" = prop_comorbid_cvae)
  
  
  #Add extra space to prevent duplicate detection
  space <- strrep(" ", spaces_n)
  rtable <- rtable %>%
    rename_all(.funs = function(x) paste0(colnames(.), space))
  
  #Deselect first column if the iteration is 1+
  if(spaces_n > 0) {
    rtable <- rtable %>% select(-1)
  }
  
  rtable
}



#Bind 
rtable <- cbind(tables[[1]], tables[[2]], tables[[3]])


#Create character vector for desired arrangement of variables
desired_vars_arranged <- c(
  "CVAE", "Fatal CVAE",
  "Heart failure", "Myocarditis", "Thromboembolic disease",
  "Disseminated intravascular coagulation", "Bleeding",
  "Shock", "Hypotension", 
  "Coronary disease", "Myocardial infarction",
  "Tachyarrhythmia", "Atrial fibrillation or flutter",
  "Ventricular tachyarrhythmia", "Ventricular tachycardia",
  "Ventricular fibrillation", "Ventricular extrasystole",
  "QT Prolongation",
  "Sudden death", "Pericarditis",
  "Pericardial effusion", "Valvular disease",
  "Bradyarrhythmia", "Cytokine release syndrome",
  "Infection",
  "Neurotoxicity",
  "Seizure", "Speech disturbance", "Tremor", "Headache",
  "Anemia",
  "Thrombocytopenia", "Leukopenia"
  )

#Arrange
rtable <- rtable %>%
  filter(Outcome %in% desired_vars_arranged) %>%
  mutate(Outcome = Outcome %>% fct_relevel(desired_vars_arranged)) %>%
  arrange(Outcome)


#Get N of row before which to add headings
headings_before <- c(1, which(rtable$Outcome == "Cytokine release syndrome"))

#Add headings as false
rtable$heading <- FALSE

#Add them
rtable <- add_row(rtable, .before = headings_before[1],
        Outcome = c("Cardiovascular adverse events"),
        heading = TRUE)
# 
# rtable <- add_row(rtable, .before = headings_before[2] + 1 ,
#                   Outcome = c("Non-cardiovascular adverse events"),
#                   heading = TRUE)
  
#Replace the NAs in these rows with empty spaces (but convert to character beforehand)
rtable <- rtable %>% mutate_all(as.character)
rtable[c(headings_before[1], headings_before[2] + 1), colnames(rtable)[colnames(rtable) %>% {!. %in% c("Outcome", "heading")}]] <- ""

#Convert heading to logical
rtable$heading <- rtable$heading %>% as.logical


#Flextable
table1 <- rtable %>% select(-heading) %>% flextable::flextable()

#Table heading
table1 <- flextable::set_caption(table1, caption = "Table 1. Association between adverse cardiovascular events and the use of bispecific T-cell engagers.")

#Bold
table1 <- flextable::bold(table1, i = rtable$heading)

#Add line
table1 <- flextable::hline(table1, i = headings_before)
table1 <- flextable::hline(table1, i = headings_before[headings_before != 1] + 1)

#Table footnote
table1 <- flextable::add_footer_lines(table1, value = flextable::as_paragraph("\nCVAE: Cardiovascular adverse events; BiTE: Bispecific T-Cell Engagers"))

#Table font
table1 <- flextable::font(table1, fontname = "Times New Roman", part = "all")


#Add header rows
table1 <- flextable::add_header_row(x = table1, values = c("", table_drugs), colwidths = c(1, 5, 5, 5))

#Align everything centrally
table1 <- flextable::align(table1, align = "center", part = "all")

#Bold
table1 <- flextable::bold(table1, part = "header")

#Add Footnote
table1 <- flextable::footnote(table1, part = "header", i = 2, j = c(4, 8, 12),
                    ref_symbols = "*",
                    value = flextable::as_paragraph(
                      "These proportions are based on the subset of events which had outcome data available.")
                                         )
#Align footer to the left
table1 <- flextable::align(table1, align = "left", part = "footer")




#Set Table width
table1 <- flextable::width(table1, width = 1.5, unit = "in")

#Save
flextable::save_as_docx(table1, path = paste0(folder, "/Table 1.docx"))

