#Set folder to save output of analyses (replace with the directory you would like to use)
folder <- your_folder

#Load libraries
source("FAERS_Libs.R")
source("Syntax_BiTE.R")

#Set DT threads
setDTthreads(16)

#Specify wanted drug for str_detect
wanted_drug <- "(?i)blinatumomab|(?i)blincyto|(?i)teclistamab|(?i)tecvayli|(?i)epcoritamab|(?i)epkinly|(?i)mosunetuzumab|(?i)lunsumio|(?i)glofitamab|(?i)columvi"

#Quarter of approval
starting_quarter <- "14Q4" 

# #Desired Sampling Rate
sampling_rate <- 0.20

#Create function to create disease column and filter accordingly
create_filter_inds <- function(indication_df) {
  indication_df[, ll := ifelse(inds %>% str_detect(get("ll", envir = globalenv()) ), 1, 0)] 
  indication_df[, mm := ifelse(inds %>% str_detect(get("mm", envir = globalenv()) ), 1, 0)] 
  indication_df[, ll_mm := ifelse(ll + mm >= 1, 1, 0)] 
  indication_df[ll_mm > 0]
}

#Import Data
# source("Import_by_indication.R")
source("Import_by_drug.R")

#(Re)Set folder for analyses
folder <- "C:/Ahmed's Stuff/ResearchStuff/BITE_Cardiotoxicity/Manuscript/JACC Submission"


#Create columns for drug classes
x[, bite := ifelse(ps_drug %>% str_detect(wanted_drug), 1, 0)] 
x[, tec := ifelse(ps_drug %>% str_detect("(?i)teclistamab|(?i)tecvayli"), 1, 0)] 
x[, bli := ifelse(ps_drug %>% str_detect("(?i)blinatumomab|(?i)blincyto"), 1, 0)] 
x[, epc := ifelse(ps_drug %>% str_detect("(?i)epcoritamab|(?i)epkinly"), 1, 0)] 
x[, mos := ifelse(ps_drug %>% str_detect("(?i)mosunetuzumab|(?i)lunsumio"), 1, 0)] 
x[, glo := ifelse(ps_drug %>% str_detect("(?i)glofitamab|(?i)columvi"), 1, 0)] 
x[, anthra := ifelse(all_drugs %>% 
                       str_detect("(?i)daunorubicin|(?i)doxorubicin|(?i)adriamycin|(?i)mycocet|
                                  (?i)cerubidine|(?i)daunoxome|(?i)epirubicin|(?i)pharmorubicin|
                                  (?i)idarubicin|(?i)idamycin"), 1, 0)] 
x[, btki := ifelse(all_drugs %>% 
                       str_detect("(?i)zanubrutinib|(?i)brukinsa|(?i)acalabrutinib|
                       (?i)calquence|(?i)ibrutinib|(?i)imbruvica|(?i)pirtobrutinib|
                       (?i)jaypirca
"), 1, 0)] 

#Create indications
x[, ll := ifelse(inds %>% str_detect(get("ll", envir = globalenv()) ), 1, 0)] 
x[, mm := ifelse(inds %>% str_detect(get("mm", envir = globalenv()) ), 1, 0)] 
x[, ll_mm := ifelse(ll + mm >= 1, 1, 0)] 


var_string <- get("cardio")
x[, ("cardio_comorbid") := ifelse(inds %>% str_detect(var_string), 1, 0)]
var_string <- get("vasc")
x[, ("vascular_comorbid") := ifelse(inds %>% str_detect(var_string), 1, 0)]

x[, cvae_comorbid := rowSums(.SD), .SDcols = c("cardio_comorbid", "vascular_comorbid")]
x[, cvae_comorbid := ifelse(cvae_comorbid > 0, 1, 0)]


#Filter out bites that have a non ll/mm indication
x <- x[!(bite == 1 & ll_mm == 0)]


#Remove useless stuff
source("Deduplicate & Remove Redundant Columns.R")

#Final sample size
analytic_ss <- nrow(x) %>% comma

#Source
source("Main_Analysis_BiTE.R")
source("Table_BiTE.R")
source("Occurrence_BiTE.R")
source("BiTE_T2Event.R")
source("Bite_T2E_Data.R")
source("Fatality Bars BiTE.R")
source("Fatality_RR_BiTE.R")
source("Fatality_FP_BiTE.R")
source("CRS Bars.R")
source("BiTE_Baseline_CCs.R")


