#Overall AE variables
cardio <- read_nonsmq("Cardio")
vascular <- read_nonsmq("Vascular")
neuro <- read_nonsmq("Neuro")
cvae <- c(cardio, vascular)

#For CV adverse event variables
hf <- read_smq("HF")
myo <- read_nonsmq("Myocarditis")
chd <- read_smq("IHD")
mi <- read_smq("MI")
af <- read_nonsmq("AF")
svt <- read_smq("SVT")
vtach <- read_nonsmq("VTachycardia")
vf <- read_nonsmq("VFib")
qt_prolong <- read_nonsmq("LQT")
pvc <- read_nonsmq("VPMB")
sd <- read_nonsmq("Sudden Death")
vta <- read_smq("VTachyarrhythmia")
brady <- read_smq("Bradyarrhythmias")
tachyarr <- read_smq("Tachyarrhythmias")
htn <- read_smq("Hypertension")
peri_eff <- read_nonsmq("Pericardial Effusion")
pericarditis <- read_nonsmq("Pericarditis")
endocarditis <- read_nonsmq("Endocarditis")
vasc <- read_smq("Vasculitis")
bleed <- read_smq("Haemorrhage")
te <- read_smq("TE")
ate <- read_smq("ATE")
vte <- read_smq("VTE")
cerebrovasc <- read_smq("Cerebrovascular")
shock <- read_smq("Shock")
hypotension <- read_nonsmq("Hypotension")
dic <- read_nonsmq("DIC")
dyslipid <- read_smq("Dyslipidemia")
valvular <- read_nonsmq("Valvular Disease")


#Disease-specific variables
ll <- c(read_nonsmq("Leukemias_Lymphomas"), "Philadelphia chromosome positive", "Philadelphia chromosome negative", "Blast cells present")
mm <- read_nonsmq("Multiple_Myeloma")

#Heme variables
crs <- read_nonsmq("CRS")
anemia <- read_nonsmq("Hematopoietic anemia")
leukopenia <- read_smq("Haematopoietic leukoepnia")
thrombocytopenia <- read_nonsmq("Thrombocytopenia")


#Neuro variables
ams <- read_nonsmq("AMS")
seizure <- read_smq("Seizures")
pn <- read_nonsmq("Peripheral Neuropathy")
speech <- read_nonsmq("Speech disturbance")
gait <- read_nonsmq("Gait disturbance")
headache <- read_nonsmq("Headache")
tremor <- read_nonsmq("Tremors")
gbs <- read_smq("GBS")

#Infection
infection <- read_nonsmq("Infections")


#Group labels
cvae_vars <- c("cardio", "vascular",
               "hf", "myo", "chd", "mi", "svt", "af", "qt_prolong", "vtach", "vf", "sd", "pvc", "vta", "brady", 
             "tachyarr", "htn", "dyslipid", "valvular", "peri_eff", "pericarditis", 
             "endocarditis", "vasc", "te", "ate", "vte", "bleed", "cerebrovasc", "shock", "hypotension",
             "dic")
cvae_labels <- c("Cardiac adverse events", "Vascular adverse events",
                 "Heart failure", "Myocarditis", "Coronary disease", "Myocardial infarction",
                 "Supraventricular Tachycardia", "Atrial fibrillation or flutter", "QT Prolongation", "Ventricular tachycardia",
                 "Ventricular fibrillation", "Sudden death", "Ventricular extrasystole", "Ventricular tachyarrhythmia", 
                 "Bradyarrhythmia", "Tachyarrhythmia", "Hypertension", 
                 "Dyslipidemia", "Valvular disease",
                 "Pericardial effusion", "Pericarditis", 
                 "Endocarditis", "Vasculitis", "Thromboembolic disease", "Arterial thromboembolism",
                 "Venous thromboembolism", "Bleeding", "Cerebrovascular disease", "Shock", "Hypotension",
                 "Disseminated intravascular coagulation")



ae_vars <- c(cvae_vars, "crs", "infection",  "ams", "seizure", "gbs",
             "speech", "gait", "headache", "tremor", 
              "anemia", "leukopenia", "thrombocytopenia", "neuro")
ae_full_vars <- c(cvae_labels,
                  "Cytokine release syndrome", "Infection", 
                   "Altered mental status", "Seizure", "GBS",
                  "Speech disturbance", "Gait disturbance", "Headache", "Tremor", 
                  "Anemia", "Leukopenia", "Thrombocytopenia", "Neurotoxicity")



for(variable in c(ae_vars, "ll", "mm")) {
  assign(paste0(variable), paste0(get(variable), collapse = ";|;") %>% paste0(";", ., ";"),
         envir = .GlobalEnv)
}



#Store definitions of AEs which nicely map onto SMQ terms
smq_defs <- mapply(smq_term, 
                   varname = c("hf", "chd", "mi", "svt", "vta", "brady", "tachyarr",
                               "htn", "vasc", "bleed", "te", "ate", "vte", "cerebrovasc",
                               "shock", "dyslipid", "seizure", "gbs"), #Specify all vars using SMQ terms
                   filename = c("HF", "IHD", "MI", "SVT", "VTachyarrhythmia", "Bradyarrhythmias",
                                "Tachyarrhythmias", "Hypertension", "Vasculitis", "Haemorrhage", "TE",
                                "ATE", "VTE", "Cerebrovascular", "Shock", "Dyslipidemia",
                                "Seizures", "GBS"), #Specify all SMQ terms
                   SIMPLIFY = F) %>%
  rbindlist



#For those that do not, use a different function
nonsmq_defs <-  mapply(nonsmq_term, 
                       varname = c("myo", "af", "vtach", "vf", "qt_prolong", "pvc", "sd",
                                   "peri_eff", "pericarditis", "endocarditis", "hypotension",
                                   "dic", "valvular", "crs",
                                   "ams",  "speech", "gait",
                                   "headache", "tremor"), #Specify all vars using Non-SMQ terms
                       filename = c("Myocarditis", "AF", "VTachycardia", "VFib",
                                    "LQT", "VPMB", "Sudden Death",
                                    "Pericardial Effusion", "Pericarditis",
                                    "Endocarditis", "Hypotension",
                                    "DIC", "Valvular Disease", "CRS",
                                    "AMS",
                                    "Speech disturbance", "Gait disturbance",
                                    "Headache", "Tremors"), #Specify all Non-SMQ terms
                       SIMPLIFY = F) %>%
  rbindlist


#For those that do not and are too long, use a yet different function
toolong_terms_defs <-  mapply(toolong_terms, 
                              varname = c("cardio",
                                          "vascular",
                                          "infection",
                                          "neuro"), #Specify all vars using Non-SMQ terms
                              term = c('All preferred terms corresponding to the System Organ Class "Cardiac Disorders"',
                                       'All preferred terms corresponding to the System Organ Class "Vascular disorders"',
                                       'All preferred terms corresponding to the System Organ Class "Infections and Infestations"',
                                       'All preferred terms corresponding to the System Organ Class "Nervous system disorders"'), #Specify all Non-SMQ terms
                              SIMPLIFY = F) %>%
  rbindlist


#Store data frame with AE definitions
ae_defs <- rbind(smq_defs, nonsmq_defs, toolong_terms_defs) %>%
  arrange(ae_full_vars)

#Save as a supplementary table
ae_defs %>%
  select(-1) %>%
  rename("Adverse event" = ae_full_vars,
         "Terms used for its identification" = term) %>%
  flextable::flextable() %>%
  flextable::set_caption(caption = "Supplementary Table 1. Medical Dictionary for Regulatory Activities (MedDRA) Terms used to identify adverse events.") %>%
  flextable::add_footer_lines(values = 'All of the above terms refer to terms within the "Preferred Terms" level of the MedDRA heirarchy unless noted otherwise. "Standardized Medical Queries" (SMQs) and "System Organ Classes" refer to higher-level concepts in the MedDRA heirarchy which multiple Preferred Terms corresponding to the adverse event are placed under.') %>%
  flextable::add_footer_lines(values = "SMQ: Standardized Medical Query") %>%
  flextable_aes(table_title = "Supplementary Table 1. Definitions of adverse events according to the Medical Dictionary for Regulatory Activities.") %>%
  flextable::save_as_docx(path = paste0(folder, "/Supplementary Table 1.docx"))


