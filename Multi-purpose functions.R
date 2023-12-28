#Round to 2 decimals appropriately
r1 <- function(x) {
  x %>% round(1) %>% format(nsmall = 1)
}


#Round to 2 decimals appropriately
r2 <- function(x) {
  x %>% round(2) %>% format(nsmall = 2)
}


#Multiply by 100 and round to 1 decimal place
pr1 <- function(x) {
  x %>%{. * 100} %>% round(1)
}

#Multiply by 100 and round to 1 decimal place
pr2 <- function(x) {
  x %>%{. * 100} %>% round(2)
}

#Multiply by 100 and round to 0 decimal place
pr0 <- function(x) {
  x %>%{. * 100} %>% round(0)
}


#Add sign (mostly used for relative change)
write_sign <- function(x) {
  
  #Define function to get sign
  get_sign <- function(x) {
    ifelse(x > 0, "+",
           ifelse(x < 0, "-", x))
  }
  
  #If  numeric get sign. If not, get numeric component and then get sign
  if(is.numeric(x)) {
    
    paste0(get_sign(x), x)
    
  } else(
    paste0(x %>% 
      str_extract_all("\\d+") %>% 
      as.numeric %>% 
      get_sign, x)
    
  )
}


#Trim empty space following sign
trimsign <- function(x) {
  x %>% str_replace_all("(\\+|\\-)\\s+", "\\1")
  }

#Convert to risk change (Formatted properly as numeric)
to_rc <- function(x) {
  
  #Convert RR to relative change
  x <- ifelse(x > 1, x - 1, ifelse(x < 1, -(1 - x), ifelse(x == 1, 0, NA)))
  
  #Return it after applying pct change
  x %>% pr2
  
}


#Put sign infront of number
put_sign <- function(x) {
  
 #Get sign
 sign <- ifelse(x > 0, "+", ifelse(x < 0, "", ifelse(x == 0, "", NA)))
 
 #Paste number with sign
 paste0(sign, x %>% format(nsmall = 1))

}

#Remove sign
remove_sign <- function(x) {
  x %>% str_replace_all("\\+|\\-", "")
}

#Convert to risk change (Formatted properly as character)
to_rc_fmt <- function(x) {
  
  #Get sign of relative change
  rc_sign <- ifelse(x > 1, "+", ifelse(x < 1, "-", ifelse(x == 1, "", NA)))
  #Convert RR to relative change
  x <- ifelse(x > 1, x - 1, ifelse(x < 1, 1 - x, ifelse(x == 1, 0, NA)))
  
  #Paste with sign
  paste0(rc_sign, x %>% pr1)
}

#Function to get ss/prop
n_props <- function(x) {
  x %>%
    summarise(n = n()) %>%
    mutate(prop = 100*n/sum(n))
}

#Function to add commas
comma <- function(x) {
  if(!is.numeric(x)) {
    x <- x %>% pull
  }
  x %>%
    format(big.mark = ",", trim = T)
}


#Function to round to convert to % and round
pct <- function(x) {
  x %>%
    {. * 100} %>%
    format(digits = 1, nsmall = 1) %>%
    paste0("%")
}


#Function to round to convert to % and round (TABLE FORMAT)
pct_table <- function(x) {
  x %>%
    {. * 100} %>%
    format(digits = 1, nsmall = 1, trim = TRUE)
}

#Function to format big Numbers properly
format_big <- function(big_number) {
  big_number %>%
    format(big.mark = ",")
}



#Function to format p's
p <- function(p) {
  ifelse(p < 0.001, "<0.001", ifelse(round(p, 2) == 0.05 | round(p, 2) == 0, round(p, 3) %>% format(nsmall = 3), round(p, 2) %>% format(nsmall = 2))) %>% as.character
  
}

#Create function to check significance
sig <- function(p) {
  if(as.numeric(p) < 0.05 | p == "<0.001") {
    TRUE
  } else {
    FALSE
  }
}

#Chain select and pull
spull <- function(df, ...) {
  df %>% dplyr::select(...) %>% pull
}


#Function to extract overall prop
get_overall_prop <- function(variable, response) {
  
  #Run svymean
  props <- svymean(design = sx, x = paste0("~ ", variable) %>% as.formula, na.rm = T)
  
  
  #Get desired proportion
  props %>% 
    data.frame %>% 
    rownames_to_column() %>% 
    cbind(confint(props)) %>%
    rename(category = rowname) %>%
    filter(str_detect(category, response)) %>%
    mutate(prop = paste0(mean %>% pct, " [95% CI: ", `2.5 %` %>% pct, " to ", `97.5 %` %>% pct, "]")) %>%
    select(prop) %>% pull
  
  
}


#Props according to strata
props_strata <- function(var, var_title, outcome, cont, cont_name) {
  
  
  #Svyby_Formula
  strata_formula <- paste0("~ ", var) %>% as.formula
  outcome_formula <- paste0("~ ", outcome) %>% as.formula
  
  #Compare DM vs No DM in terms of advice to quit smoking BY VARIABLE
  props_var <- svyby(design = sx, outcome_formula, strata_formula, svyciprop, na.rm = TRUE, method = "logit")
  props_var <- cbind(props_var, confint(props_var))
  
  #Select variables
  props_var <- props_var %>%
    remove_rownames %>%
    select(var, outcome, "2.5 %", "97.5 %") %>%
    rename(pe = outcome,
           lci = "2.5 %",
           uci = "97.5 %")
  
  
  
  #Check if cont is TRUE or FALSE
  if(cont == FALSE) {
    #Chisq formula
    glm_formula <- paste0(outcome, " ~ ", var) %>% as.formula
    
  } else if(cont == TRUE) {
    
    #Conduct T-test
    glm_formula <- paste0(outcome, " ~ ", cont_name) %>% as.formula
    
    
  }
  
  
  #Set base formula to compare univariable model to
  base_formula <- paste0(outcome, " ~ ", 1) %>% as.formula
  #Get P-value from svyglm
  glm <- svyglm(design = sx, formula = glm_formula, family = binomial)
  base_glm <- svyglm(design = sx, formula = base_formula, family = binomial)
  anova <- anova(glm, base_glm, method = "Wald")
  pval <- anova$p %>% p
  
  # #Calculate the reverse if needed
  # if(reverse == TRUE) {
  # props_var <- props_var %>%
  #   mutate(opp_prop = 1 - prop,
  #          opp_lci = 1 - uci,
  #          opp_uci = 1 - lci)}
  
  #Format properly and add overall column  
  nice_table <- props_var %>% 
    mutate(pe_ci = paste0(pe %>% pct_table, " [", lci %>% pct_table, " to ", uci %>% pct_table, "]"),
           pvalue = "") %>%
    mutate(across(.cols = c("lci", "pe", "uci"), ~ . * 100)) %>%
    rename("Prevalence (%) [95% CI]" = pe_ci,
           "Variable" = var,
           "P-value" = pvalue) %>%
    add_row(.before = 1,
            Variable = var_title,
            "P-value" = pval,
            "Prevalence (%) [95% CI]" = "")
  
  
  nice_table <- nice_table %>%
    mutate(outcome = outcome)
  
  #Print it out
  nice_table
}

#Calculate SE of a proportion given the proportion and N of trials
se_prop <- function(p, n) {
  sqrt((p * (1 - p))/n)
}

#Create function to get quantiles of a specified variable and convert to a dataframe
get_qs <- function(variable, desired_qs) {
  
  #Get quantiles
  qs <- svyquantile(design = sx, na.rm = TRUE, x = paste0("~ ", variable) %>% as.formula, quantiles = desired_qs)
  
  #Create function to convert to dataframe
  q_to_df <- function(x) {
    x[[1]] %>% data.frame %>% rownames_to_column() %>% rename(q = rowname, value = quantile)
  }
  
  
  #Convert to dataframe
  qs_df <- q_to_df(qs)
  
  assign(x = paste0(variable, "_qs"), value = qs_df, envir = globalenv())
}


#Get survey median and IQR
m_iqr <- function(variable) {
  
  #Paste name of object from which you want the median/IQR
  qs <- paste0(variable, "_qs")
  
  if(!exists(qs)) {
  #Create quantiles dataframe
  get_qs(variable, desired_qs)
  }
  
  #Get IQR
  q50 <- qs %>% get %>% filter(q == 0.5) %>% spull(value) %>% round(1)
  iqr <- qs %>% get %>% filter(q == 0.25 | q == 0.75) %>% spull(value) %>%
    {.[2] - .[1]} %>% round(1)
  
  paste0(q50, " (", iqr, ")")
}



#Make stuff numeric
make_numeric <- function(data, variables) {
  data %>% mutate(across(any_of(variables), ~ as.numeric(.)))
}


#Make stuff factor
make_factor <- function(data, variables) {
  data %>% mutate(across(any_of(variables), ~ as.factor(.)))
}


#Merge intvn and ctrl groups
merge_groups <- function(data, new_var, intvn_var, ctrl_var) {
  data %>% mutate(!!as.symbol(new_var) := 
                    (!!as.symbol(intvn_var) * (intvn_ss/(intvn_ss + ctrl_ss) )) + 
                    !!as.symbol(ctrl_var) * (ctrl_ss/(intvn_ss + ctrl_ss)))
  
}


#Clean wonder function
tidy_wonder <- function(x, year_filter = FALSE) {
  x <- x %>%
    clean_names %>%
    filter(notes != "Total" & !is.na(year) & !is.na(year_code)) %>%
    rename(any_of(c(
      cmr = "crude_rate",
      cmr_lci = "crude_rate_lower_95_confidence_interval",
      cmr_uci = "crude_rate_upper_95_confidence_interval",
      amr_lci = "age_adjusted_rate_lower_95_confidence_interval",
      amr_uci = "age_adjusted_rate_upper_95_confidence_interval",
      cmr_se = "crude_rate_standard_error",
      aamr = "age_adjusted_rate",
      aamr_se = "age_adjusted_rate_standard_error"
    ))) %>%
    mutate(across(.cols = any_of(c("population", "deaths", "cmr", "cmr_se", "aamr", "aamr_se")), ~ as.numeric(.x)))
  
  if(year_filter != FALSE) {
    x <- x %>% filter(year != year_filter)
  }
  x
}




#Create cube root transformation for X-axis
cube_root <- function(x) x ^ (1/3)
cube <- function(x) x ^ 3
trans_cube <- scales::trans_new(name = "cube root",
                        transform = cube_root,
                        inverse = cube)
#Get props using mutate
mprop <- function(data, prop_var) {
  data %>% mutate(prop = 100*!!as.symbol(prop_var)/sum(!!as.symbol(prop_var)))
}




#Get aesthetics for forest plot
forestploter_aes <- function(forest = forest, fp_df,
                             col_names = TRUE, specific_rows = TRUE,
                             marker = "header") {
  ##Header aesthetics
  if(col_names == TRUE) {
    
    #Underline headers
    forest <- add_underline(plot = forest, part = "header")
    
    # Bold column headers text
    forest <- edit_plot(forest,
                        part = "header",
                        gp = gpar(fontface = "bold"))
  }
  
  #Specific row aesthetics
  if(specific_rows == TRUE) {
    # Bold specific rows
    forest <- edit_plot(forest,
                        part = "body",
                        gp = gpar(fontface = "bold"),
                        row = fp_df[, marker] %>% which)
    
    forest <- add_underline(forest, 
                            row = c(fp_df[, marker] %>% which, 
                                    fp_df[, marker] %>% which %>% {.-1}))
    
    
  }
  
}

#Perform some aesthetic modifications for flextables
flextable_aes <- function(table, table_title) {
  
  #Table font
  table <- flextable::font(table, fontname = "Times New Roman", part = "all")
  
  #Align everything centrally
  table <- flextable::align(table, align = "center", part = "all")
  
  #Table heading
  table <- flextable::set_caption(table, caption = table_title,
                                  fp_p = fp_par(text.align = "left"))
  
  #Align footer to the left
  table <- flextable::align(table, align = "left", part = "footer")
  
  #Bold header
  table <- flextable::bold(table, part = "header")
  
  #Set Table width
  table <- flextable::width(table, width = 1.5, unit = "in")
  
  #Bold
  table <- flextable::bold(table, part = "header")
  
  #Print
  table
}


#Get knot placement evaluation using rcs
rcs_knots <- function(variable, n_knots) {
  rcspline.eval(
  variable,
  nk = n_knots,
  knots.only = TRUE
)
  
}

#Function to insert footnote symbol in the correct order
footnote_symbol <- function(i) {
  footnote_symbols <- c("\U002A", "\U2020", "\U2021", "\U00A7")
  footnote_symbols[i]
}

#Odds to probability
o2p <- function(odds) {
  odds/(1 + odds)
}

#Probablity to odds
p2o <- function(prob) {
  prob/(1 - prob)
}

#Extract individual study estimates from meta function (FOR PROPS)
extract_study_es <- function(meta, par) {
  data.frame(pe = meta$event/meta$n,
             lci = meta$lower,
             uci = meta$upper,
             par = par,
             study = meta$studlab) %>%
    pe_ci_pct
  
}


#Add pe_ci
pe_ci_pct <- function(data, pe = pe, lci = lci, uci = uci, rounding = 1) {
  data %>%   mutate(pe_ci = paste0(
    pe %>% pr1 %>% format(nsmall = rounding), "% (", 
    lci %>% pr1 %>% format(nsmall = rounding), " to ",
    uci %>% pr1 %>% format(nsmall = rounding),
    "%)"
  )
  )
}

#Add pe_ci without pct
pe_ci <- function(data, pe = pe, lci = lci, uci = uci, rounding = 2) {
  data %>%   mutate(pe_ci = paste0(
    pe  %>% round(rounding) %>% format(nsmall = rounding), " (", 
    lci %>% round(rounding) %>% format(nsmall = rounding), " to ",
    uci  %>% round(rounding) %>% format(nsmall = rounding),
    ")"
  ))
  
}

grid_2by2 <- function() {
  
  hrzl_coords = c(0.5, 0.5, 0, 1)
  vert_coords = c(0, 1, 0.5, 0.5)
  line_id = c(1, 1, 2, 2)
  grid.polygon(hrzl_coords, vert_coords, line_id, gp = gpar(lwd = 2))
  
}

grid_3by2 <- function() {
  
  hrzl_coords = c(0.5, 0.5, 0, 1, 0, 1)
  vert_coords = c(0, 1, 0.333333, 0.333333, 0.666666, 0.666666)
  line_id = c(1, 1, 2, 2, 3, 3)
  grid.polygon(hrzl_coords, vert_coords, line_id, gp = gpar(lwd = 2))
  
}

#Function to insert a single horizontal line (to separate figures)
grid_single_horizontal <- function() {
  vert_coords = c(0.5, 0.5)
  hrzl_coords = c(0, 1)
  line_id = c(1, 1)
  grid.polygon(hrzl_coords, vert_coords, line_id, gp = gpar(lwd = 2))
}


#Undo P-value function
undo_p <- function(p) {
  ifelse(p == "<0.001", 0.001, as.numeric(p))
} 
