#Declare df_names and folders
df_names <- c( "drug", "indication", "demo", "outcome", "reaction", "therapy")
folders <- c( "Drug", "Indications", "Demographics", "Outcomes", "Reaction",  "Therapy")


#Foreach loop
foreach(df_name = df_names,
        folder = folders,
        .packages = packs) %do% {
          
          #Declare df
          assign(df_name, data.frame())
          
          
          #State directory of files
          files <- dir(paste0("C:/Ahmed's Stuff/ResearchStuff/FAERS/Databases/", folder))
          
          
          #Filter by quarter during which the drug was approved
          starting_file <- which(files %>% str_detect(starting_quarter))
          files <- files[starting_file:length(files)]
          
          
          #Import files within the folder as a list
          df_list <- foreach(file = files,
                             .packages = packs) %do% {
                               
                               
                               #First, specify the columns you want
                               desired_columns <- if(df_name == "demo") {
                                 c("primaryid", "caseversion", "caseid", "age", "sex", "age_cod", "event_dt", "reporter_country", "occp_cod")
                               } else if(df_name == "drug") {
                                 c("primaryid", "drug_seq", "drugname", "role_cod")
                               } else if(df_name == "outcome") {
                                 c("primaryid", "outc_cod")
                               } else if(df_name == "reaction") {
                                 c("primaryid", "pt")
                               } else if(df_name == "indication") {
                                 c("primaryid", "indi_pt")
                               }  else if(df_name == "therapy") {
                                 c("primaryid", "dsg_drug_seq", "start_dt") 
                               }
                               
                               #Import file
                               imported_file <-  fread(file = paste0("C:/Ahmed's Stuff/ResearchStuff/FAERS/Databases/", folder, "/", file),
                                                       header = T, sep = "$", select = desired_columns)
                               
                               if(df_name == "drug") {
                                 
                                 #Save all desired drugs
                                 wanted_drug_only_file <- imported_file[drugname %>% str_detect(wanted_drug) &
                                                                          (role_cod == "PS")]
                                 
                                 
                                 
                                 #Save all non-desired drugs used in the same report as a desired drug (to adjust for concomitant 
                                 #cardiotoxic agents that may have been adminstered with the desired drug)
                                 wanted_drug_assoc_file <- imported_file[(role_cod != "PS") &
                                                                           primaryid %in% wanted_drug_only_file[, primaryid]]
                                 
                                 #Collapse associated file by primaryid
                                 wanted_drug_assoc_file <- wanted_drug_assoc_file[, .(assoc_drugs = paste0(drugname, collapse = ";")), by = .(primaryid)]
                                 
                                 #Assign PS drugs (drugname in PS file and NA in other file)
                                 wanted_drug_only_file$ps_drug <- wanted_drug_only_file$drugname
                                 
                                 #Now merge them together
                                 wanted_drug_file <- merge(wanted_drug_only_file[, c("primaryid", "ps_drug", "drug_seq")],
                                                           wanted_drug_assoc_file[, c("primaryid", "assoc_drugs")],
                                                           by = "primaryid", all.x = TRUE)
                                 
                                 #Save the "rest" (background comparison) PS file
                                 rest_ps_only_file <- imported_file[drugname %>% str_detect(wanted_drug, negate = T) &
                                                                      (role_cod == "PS")]
                                 
                                 #Set a seed for reproducibility to sample PS reports
                                 set.seed(100)
                                 
                                 #Take a random 20% sample to ease computation later on
                                 rest_ps_only_file <- rest_ps_only_file[sample(.N, sampling_rate * nrow(rest_ps_only_file))]
                                 
                                 #Save other associated drugs in the rest file
                                 rest_assoc_file <- imported_file[primaryid %in% rest_ps_only_file[, primaryid] &
                                                                    (role_cod != "PS")]
                                 
                                 #Collapse associated file by primaryid
                                 rest_assoc_file <- rest_assoc_file[, .(assoc_drugs = paste0(drugname, collapse = ";")), by = .(primaryid)]
                                 
                                 
                                 #Assign PS drugs (drugname in PS file and NA in other file)
                                 rest_ps_only_file$ps_drug <- rest_ps_only_file$drugname
                                 
                                 #Bind rest files
                                 rest_file <- merge(rest_ps_only_file[, c("primaryid", "ps_drug", "drug_seq")], 
                                                    rest_assoc_file[, c("primaryid", "assoc_drugs")],
                                                    by = "primaryid", all.x = TRUE)
                                 
                                 #Bind to recreate imported_file
                                 imported_file <- rbindlist(list(wanted_drug_file, rest_file))
                                 
                                 #Create all_drugs column
                                 imported_file$all_drugs <- paste0(";", imported_file$ps_drug, ";", imported_file$assoc_drugs, ";")
                                 
                                 #Print Imported File
                                 imported_file
                               }
                               
                               imported_file$primaryid <- as.character(imported_file$primaryid)
                               
                               imported_file
                               
                               
                             }
          
          #Use rbindlist to combine the dataframes within df_list into a single df
          assign(df_name, rbindlist(df_list, use.names = TRUE) %>% unique)
          
          #Choose relevant variables from each dataset, filter to chosen IDs, and group by ID for drugs/reactions
          if(df_name == "demo") {
            demo <- demo[primaryid %in% desired_ids]
            
          } else if(df_name == "drug") {
            # drug <- drug[,`:=`(drugname = paste0(drugname, collapse = ";"),
            #                    ps_drug = paste0(ps_drug, collapse = ";")), by = .(primaryid)]
            
            #Set desired ids
            assign("desired_ids", drug[, primaryid], envir = .GlobalEnv)
            
            #drug_ids is needed for the therapy file to get the dates for this drug specifically
            drug[ps_drug %>% str_detect(wanted_drug), drug_therapy_link := paste0(primaryid, drug_seq)]
            assign("drug_ids", drug[, drug_therapy_link], envir = .GlobalEnv)
          } else if(df_name == "outcome") {
            outcome <- outcome[primaryid %in% desired_ids]
            outcome <- outcome[, .(outc_cod = paste0(outc_cod, collapse = ";")), by = .(primaryid)]
          } else if(df_name == "reaction") {
            reaction <- reaction[primaryid %in% desired_ids]
            reaction <- reaction[, .(pt = paste0(pt, collapse = ";")), by = .(primaryid)]
          } else if(df_name == "indication") {
            indication <- indication[primaryid %in% desired_ids]
            indication <- indication[, .(indi_pt = paste0(indi_pt, collapse = ";")), by = .(primaryid)]
          } else if(df_name == "therapy") {
            
            therapy[, therapyid := paste0(primaryid, dsg_drug_seq)]
            
            
            therapy <- therapy[therapyid %in% drug_ids]
            
            therapy <- therapy[, .(start_dt = paste0(start_dt, collapse = ";")), by = .(primaryid)]
          } 
          
          #Print dataframe
          get(df_name) %>% print()
        }



#Create a list containing all included dataframes
included_dfs <- foreach(df_name = df_names) %dopar% {
  get(df_name)
}


#Remove dataframes that are now in a list
rm(demo, drug, outcome, reaction, indication, therapy, df_list)


#Merge x with the rest of the non-drug-based data.tables
x <- Reduce(function(...) merge(..., all = TRUE, by = "primaryid"),
            included_dfs)


#Remove list
rm(included_dfs)


#SS before deduplication
ss_before_caseid_dedup <- nrow(x) %>% comma

#Filter out duplicate cases by taking the latest case version of each
x <- x[order(caseversion), tail(.SD, 1), by = .(caseid)]

#SS after deduplication
ss_after_caseid_dedup <- nrow(x) %>% comma


#Correct age
x[, age_yr:= ifelse(age_cod == "DEC", age*10, 
                    ifelse(age_cod == "DY", age/365,
                           ifelse(age_cod == "HR", age/(365*24),
                                  ifelse(age_cod == "WK", age/(365/7),
                                         ifelse(age_cod == "MON", age/12,
                                                ifelse(age_cod == "YR", age, NA))))))]

x[, age_yr:= ifelse(age_yr < 0, NA, ifelse(age_yr > 100, NA, age_yr))]


#Correct Sex
x[, sex:= ifelse(sex == "M", "Male", ifelse(sex == "F", "Female", NA))]

#Check N of rows
ss_before_any_filters <- x %>% nrow %>% comma

#Filter out missing age and sex
x <- x[!is.na(sex) & !is.na(age_yr)]

#Check N of rows
ss_with_age_sex <- x %>% nrow %>% comma


#Correct outcome column
x[, outcome:= ifelse(outc_cod %>% str_detect("DE"), "Death", 
                     ifelse(outc_cod %>% str_detect("LT"), "Life-threatening",
                            ifelse(outc_cod %>% str_detect("HO"), "Hospitalization",
                                   ifelse(outc_cod %>% str_detect("DS|CA|RI|OT"), "Other", NA))))]

#Create death column
x[, death := ifelse(outcome == "Death", 1, 0)]

#Create year and filter out unlikely values
x[, year := event_dt %>% as.character %>% str_sub(1, 4) %>% as.numeric]
x[, year := ifelse(year < 1900, NA, ifelse(year > 2023, NA, year))]

#Lubridate
x[, event_date := event_dt %>% ymd] 
x[, start_date := start_dt %>% ymd]
x[, aetime := difftime(event_date, start_date, units = "days")]
x[, aetime := ifelse(aetime < 0, NA, ifelse(aetime > 1000, NA, aetime))]

#Clean up columns for AEs, drugs, and indications
x[, ae := pt %>% sunique]
x[, ae := paste0(";", ae, ";")]
x[, all_drugs := all_drugs %>% sunique] #The ";" step was already done above.
x[, inds := indi_pt %>% sunique]
x[, inds := paste0(";", inds, ";")]

#Create columns for AEs
foreach(ae_var = ae_vars) %do% {
  var_string <- get(ae_var)
  x[, (ae_var) := ifelse(ae %>% str_detect(var_string), 1, 0)]
}


#Create CVAEs
x[, cvae := rowSums(.SD), .SDcols = c("cardio", "vascular")]
x[, cvae := ifelse(cvae > 0, 1, 0)]
x[, dcvae := ifelse(cvae > 0 & death == 1, 1, 
                    ifelse(cvae > 0 & death == 0, 0,
                           ifelse(cvae == 0, 0, NA)))]



#Create NCVAEs
x[, ncvae := ifelse(cvae > 0, 0, 1)]


#Comorbidities
var_string <- get("cardio")
x[, ("cardio_comorbid") := ifelse(inds %>% str_detect(var_string), 1, 0)]
var_string <- get("vasc")
x[, ("vascular_comorbid") := ifelse(inds %>% str_detect(var_string), 1, 0)]

x[, cvae_comorbid := rowSums(.SD), .SDcols = c("cardio_comorbid", "vascular_comorbid")]
x[, cvae_comorbid := ifelse(cvae_comorbid > 0, 1, 0)]
