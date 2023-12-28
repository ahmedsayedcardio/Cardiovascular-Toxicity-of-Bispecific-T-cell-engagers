#Prepare forest df
mort_fp_df <- mort_rr_df %>%
  filter(ae %>% str_detect("dnae|snae|dhae|shae|dcvae|scvae|cardio|vascular", negate = TRUE)) %>%
  filter( ((pval == "<0.001" | as.numeric(pval) < 0.05) & rr > 1) | ae %in% c("crs", "neuro", "infection"))  %>%
  filter(drug == "bite") %>%
  select(drug_full, ae_full,
         rr, lci, uci, table_es, 
         ae, event_mortality, noevent_mortality) %>%
  mutate(across(.cols = c(rr, lci, uci), ~ . %>%  as.numeric))



#Set x limits
xlim <- c(0.2, 5)


#Place Ticks
xticks <- c(0.2, 0.5, 1, 2, 5)


#Set theme
fp_theme <- forest_theme(base_size = 14.7,
                         title_cex = 1.5,
                         footnote_cex = 0.7,
                         refline_lwd = 2,
                         xaxis_lwd = 2,
                         xaxis_cex = 1,
                         ci_pch = 15,
                         ci_Theight = 0.25,
                         ci_col = "black",
                         title_just = "center",
                         core = list(bg_params = list(fill = c("#D3EAEA", "#FFFFFF")),
                                     fg_params = list(hjust = 0.5, x = 0.5)
                                     ),
                         colhead = list(
                           fg_params = list(hjust = 0.5, x = 0.5))
)

#Set title
title <- "Figure 2: Mortality risk ratios of adverse events reported with bispecific T-cell engagers.\n\n"

#Write footnote
footnote <- "\n\n\n\n\U002ALogistic regression models with death as the dependent (outcome) variable and age, sex, and a given adverse event as independent (predictor) variables were built. Average marginal effects were then applied to obtain\nadjusted mortality rates and risk ratios. Only events where mortality outcomes were available for at least 10 cases were eligible for this analysis."


#Arrange according to outcomes
mort_fp_df <- arrange(mort_fp_df, -rr)



#Insert empty column for FP plotting space
mort_fp_df$"space" <- paste(rep(" ", 40), collapse = " ")

#Columns to get
fp_cols <- c(
  which(colnames(mort_fp_df) == "ae_full"),
  which(colnames(mort_fp_df) == "event_mortality"),
  which(colnames(mort_fp_df) == "noevent_mortality"),
  length(mort_fp_df),
  which(colnames(mort_fp_df) == "table_es")
)


#Forest
forest <- forest(data = mort_fp_df %>% dplyr::select(fp_cols) %>%
                   rename("Adverse Event" = "ae_full",
                          "Adjusted mortality\nwith adverse event (%)\U002A" = "event_mortality",
                          "Adjusted mortality\nwithout adverse event (%)\U002A" = "noevent_mortality",
                          " " = "space",
                          "Adjusted\nRisk Ratio [95% CI]\U002A" = "table_es"),
                 #Labels and est/ci
                 est = mort_fp_df$rr,
                 ci_column = 4,
                 lower = mort_fp_df$lci,
                 upper = mort_fp_df$uci,
                 #Labels
                 arrow_lab = c("Lower mortality", "Higher mortality"),
                 #Box Aesthetics,
                 sizes = 5,
                 footnote = footnote,
                 #X-axis
                 ticks_at = xticks,
                 xlog = TRUE,
                 xlim = xlim,
                 #Title,
                 title = title,
                 #Theme
                 theme = fp_theme
)

#Underline headers
forest <- add_underline(plot = forest, part = "header")

# #Underline other rows
# forest <- add_underline(forest, row = c(which(mort_fp_df$outcome_full %>% str_detect("Major|Safety", negate = T))[c(-1, -2, -3, -length(which(ame_fp$var_full %>% str_detect("Cardiovascular disease", negate = T))))], 
#                                         which(mort_fp_df$var_full %>% str_detect("Cardiovascular disease", negate = T)) - 1)
# )

# Bold column headers text
forest <- edit_plot(forest,
                    part = "header",
                    gp = gpar(fontface = "bold"))


# forest <- edit_plot(forest,
#                     part = "body",
#                     gp = gpar(fontsize = 15)) 
# forest <- edit_plot(forest,
#                     part = "body",
#                     gp = gpar(fontsize = 40))
#Trim some of the white space above by changing vertical justification
forest$vp <- viewport(just = c(0.5, 0.5))

#Adjust row height
forest$heights[4:(7 + length(mort_fp_df))] <- unit(11, "mm")
# forest$heights[2] <- unit(25, "mm")

# forest$heights[7 + length(mort_fp_df)] <- unit(2, "mm")



#Produce file
ggsave(forest,
       filename = paste0(
         folder,
         "/Figure 4.pdf"),
       dpi = 600,
       width = 16,
       height = 9,
       bg = "white")



