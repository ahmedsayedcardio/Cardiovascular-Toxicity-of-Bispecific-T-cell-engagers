#Print out a CSV of all CVAE fatality rates for Dr. Addison
mres_df %>%
  filter(var == "bite") %>%
  filter(outcome %in% cvae_vars) %>%
  mutate(prop_death = prop_death %>% as.character %>% str_replace("NaN", "Can not be calculated since event did not occur")) %>%
  select(drug_full, outcome_full, mortality_denom, n_death, prop_death) %>%
  rename_with(~ c("Drug", "Outcome", "Number of events (with mortality data available)",
                  "Number of events resulting in death",
                  "Fatality proportion")) %>%
  write.csv("C:/Ahmed's Stuff/ResearchStuff/BITE_Cardiotoxicity/Manuscript/Fatality rates of CVAE (BiTE).csv")

#Filter to bite and choose AEs of interest
ff_df <- mres_df %>% 
  filter(var == "bite") %>%
  filter(outcome %>% str_detect("dnae|snae|dhae|shae|dcvae|scvae", negate = TRUE)) %>%
  arrange(-prop_death) %>%
  filter(outcome %in% c("hf", "myo", "hypotension", "bleed", "dic", "tachyarr", "neuro", "shock", "crs", "infection"))
  

##Clarify duplicated ones
# for(outcome in unique(ff_df$outcome_full))  {
#   
#   #Count N of times outcome shows up
#   n <- sum(ff_df$outcome_full == outcome)
#   
#   #Add first letter of drug name if it shows up more than once
#   if(n > 1) {
#     ff_df$outcome_full[ff_df$outcome_full == outcome] <- paste0(
#       ff_df$outcome_full[ff_df$outcome_full == outcome], " (", 
#       word(ff_df$drug_full[ff_df$outcome_full == outcome], 1),
#       ")"
#     )
#   }
#   
# }

# ff_df$outcome_full <- paste0(ff_df$outcome_full, " (", ff_df$drug_full, ")")

#Get average fatality rate associated with CVAEs
avg_prop_death <- 100*(x[ bite == 1 & death == 1] %>% nrow/
  x[ bite == 1 & !is.na(death)] %>% nrow)


#Factorize and reorder outcome
ff_df$outcome_full <- fct_reorder(ff_df$outcome_full, -ff_df$prop_death)


#Change tableau color palette
tableau_color_pal("Tableau 20")(20)
my_palette <- c(
  tableau_color_pal("Tableau 20")(20)[seq(1, 19, 2)],
  tableau_color_pal("Tableau 20")(20)[seq(2, 20, 2)]
)

#Set titles
xtitle <- "Adverse Event"
ytitle <- "Fatality rate (%)"
main_title <- paste0("Fatality rates of adverse events occurring with bispecific T-cell engagers.")
subtitle <- paste0("The dashed black line denotes the average fatality rate of adverse events associated with bispecific T-cell engagers.")

#Set breaks for axes
ybreaks <- c(seq(0, 100, 10))

#Set limits
ylimits <- c(0, 65)

#Plot
ggplot(data = ff_df,
       aes(x = outcome_full,
           y = prop_death,
           fill = outcome_full)) +
  geom_col(color = "black", size = 1.1) +
  geom_hline(yintercept = avg_prop_death,
             color = "black",
             lwd = 1.25,
             linetype = "dashed") +
  geom_text(aes(label = paste0(prop_death %>% format(nsmall = 1), "%")),
             fill = "white",
             nudge_y = 1.7,
             size = 8,
             label.size = 0.75) +

  scale_fill_manual(values = my_palette, name = "") +
  #Add scales
  scale_x_discrete(
    name = "Adverse event") +
  scale_y_continuous(
    name = ytitle,
    breaks = ybreaks,
    limits = ylimits,
    expand = c(0, 0)
  ) +
  #Add plot title
  ggtitle(main_title,
          subtitle = subtitle) +
  #Theme
  theme_pubclean() +
  theme(text = element_text(size = 23),
        plot.title=element_text(face = "bold",hjust = 0.0, size = 24),
        plot.subtitle = element_text(face = "bold", size = 11, hjust = 0.0, color = "grey45"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 25, face = "bold"),
        axis.title.y = element_text(size = 25, face = "bold"),
        axis.line = element_line(colour = "black", size = 1.2),
        plot.margin = margin(0.5, 1, 0.5, 1, "cm"),
        legend.position = "bottom",
        legend.text = element_text(size = 12.5, face = "bold"),
        legend.key.width = unit(1, "cm")) 


#Save plot
ggsave(plot = last_plot(),
       filename = paste0(
         folder,
         "/Figure 3.pdf"),
       height = 9, width = 16, dpi = 600)


