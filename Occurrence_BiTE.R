#Create fatality figure df (Filter to bli as that only has a sufficient sample size)
o_df <- mres_df %>% 
  filter(var == "bite" & outcome %in% cvae_vars) %>%
  filter(outcome %in% c("bleed", "hypotension", "te", "hf", "shock", "tachyarr", "myo", "pericarditis", "sd")) %>%
  arrange(-n_ae) %>%
  slice(1:20) %>%
  select(outcome_full, n_ae)


#Rename for space
o_df$outcome_full <- o_df$outcome_full %>% str_replace("Thromboembolic disease", "Thromboembolic\ndisease")
o_df$outcome_full <- o_df$outcome_full %>% str_replace("Pericardial disease", "Pericardial\ndisease")

#Factorize and reorder outcome
o_df$outcome_full <- fct_reorder(o_df$outcome_full, -o_df$n_ae)


#Change tableau color palette
tableau_color_pal("Tableau 20")(20)
my_palette <- c(
  tableau_color_pal("Tableau 20")(20)[seq(1, 19, 2)],
  tableau_color_pal("Tableau 20")(20)[seq(2, 20, 2)]
)

#Set titles
xtitle <- "Adverse Event"
ytitle <- "Reported Events (N)"
main_title <- paste0("Frequency of cardiovascular adverse events occurring with bispecific T-cell engagers.")
subtitle <- paste0("Each reported case may have involved several reported adverse events; therefore, these numbers are not mutually exclusive.")

#Set breaks for axes
ybreaks <- c(seq(0, 1000, 20))

#Set limits
ylimits <- c(0, 230)

#Plot
ggplot(data = o_df,
       aes(x = outcome_full,
           y = n_ae,
           fill = outcome_full)) +
  geom_col(color = "black", size = 1.1) +
  geom_text(aes(label = paste0(n_ae, "")),
             fill = "white",
             nudge_y = 4.0,
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
        plot.title=element_text(face = "bold",hjust = 0.0, size = 22),
        plot.subtitle = element_text(face = "bold", size = 11, hjust = 0.0, color = "grey45"),
        axis.text.x = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 25, face = "bold"),
        axis.title.y = element_text(size = 25, face = "bold"),
        axis.line = element_line(colour = "black", size = 1.2),
        plot.margin = margin(0.5, 1, 0.5, 1, "cm"),
        legend.position = "none",
        legend.text = element_text(size = 7.8, face = "bold"),
        legend.key.width = unit(1, "cm")) 


#Save plot
ggsave(plot = last_plot(),
       filename = paste0(
         folder,
         "/Figure 1.pdf"),
       height = 9, width = 16, dpi = 600)


