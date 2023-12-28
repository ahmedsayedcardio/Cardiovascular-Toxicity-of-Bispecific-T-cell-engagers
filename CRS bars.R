#Create atality figure df (Filter to bli as that only has a sufficient sample size)
crs_df <- mres_df %>% 
  mutate(outcome_full = factor(outcome_full)) %>%
  filter(var == "bite") %>%
  arrange(-prop_crs) %>%
  filter(outcome %in% c("cvae", "dcvae", "myo", "hf", "bleed", "shock", "dic", "neuro", "hypotension", "infection", "bm_suppression"))

#Turn to character
crs_df$outcome_full <- as.character(crs_df$outcome_full)

##Clarify duplicated ones
for(outcome in unique(crs_df$outcome_full))  {

  #Count N of times outcome shows up
  n <- sum(crs_df$outcome_full == outcome)

  #Add first letter of drug name if it shows up more than once
  if(n > 1) {
    crs_df$outcome_full[crs_df$outcome_full == outcome] <- paste0(
      crs_df$outcome_full[crs_df$outcome_full == outcome], " (",
      word(crs_df$drug_full[crs_df$outcome_full == outcome], 1),
      ")"
    )
  }

}

#Get average fatality rate associated with CVAEs
avg_prop_crs <- 100*(x[ bite == 1 & crs == 1] %>% nrow/
                         x[ bite == 1] %>% nrow)


#Factorize and reorder outcome
crs_df$outcome_full <- fct_reorder(crs_df$outcome_full, -crs_df$prop_crs)



#Set titles
xtitle <- "Adverse Event"
ytitle <- "CRS overlap rate (%)"
main_title <- paste0("Rates of overlap between cytokine release syndrome and other adverse events reported with bispecific T-cell engagers.")
subtitle <- paste0("The black line denotes the average proportion of adverse events involving cytokine release syndrome.")

#Set breaks for axes
ybreaks <- c(seq(0, 100, 10))

#Set limits
ylimits <- c(0, 45)

#Plot
ggplot(data = crs_df,
       aes(x = outcome_full,
           y = prop_crs,
           fill = outcome_full)) +
  geom_col(color = "black", size = 1.1) +
  geom_text(aes(label = paste0(prop_crs %>% format(nsmall = 1))),
             fill = "white",
             vjust = -0.2,
             size = 8,
             label.size = 0.75,
             position = position_dodge(width = 0.9)) +
  geom_hline(yintercept = avg_prop_crs,
             color = "black",
             lwd = 1.25,
             linetype = "dashed") +
  scale_fill_tableau(name = "") +
  #Add scales
  scale_x_discrete(
    name = "Adverse event") +
  scale_y_continuous(
    name = ytitle,
    limits = ylimits,
    breaks = ybreaks,
    expand = c(0, 0)
  ) +
  #Add plot title
  ggtitle(main_title,
          subtitle = subtitle) +
  #Theme
  theme_pubclean() +
  theme(text = element_text(size = 23),
        plot.title=element_text(face = "bold",hjust = 0.0, size = 18),
        plot.subtitle = element_text(face = "bold", size = 11, hjust = 0.0, color = "grey45"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 25, face = "bold"),
        axis.title.y = element_text(size = 25, face = "bold"),
        axis.line = element_line(colour = "black", size = 1.2),
        plot.margin = margin(0.5, 1, 0.5, 1, "cm"),
        legend.position = "bottom",
        legend.text = element_text(size = 12, face = "bold"),
        legend.key.width = unit(1, "cm")) 


#Save plot
ggsave(plot = last_plot(),
       filename = paste0(
         folder,
         "/Figure 5.pdf"),
       height = 9, width = 16, dpi = 600)


