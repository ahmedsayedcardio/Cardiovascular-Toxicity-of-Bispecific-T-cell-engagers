#Create dataframe for time-to-event data
tdf <- mres_df %>%
  filter(var == "bite") %>%
  filter(outcome %in% c("cvae", "ncvae", "hf", "dic", "bleed", "shock", "hypotension"))


#Remove cvae_vars to not intervene with naming
rm(list = cvae_vars)

#Foreach loop to get T2E data for each outcome
tdf <- foreach(ae_short = tdf$outcome,
               adverse_event = tdf$outcome_full,
               drug = tdf$var,
               .combine = "rbind") %do% {
                 
                 #Get time for desired commbination of drug and adverse event
                 ae_times <- x[get(ae_short) == 1 & get(drug) == 1 & !is.na(aetime), aetime]
                 
                 #Combine into a dataframe
                 df <- data.frame(etime = ae_times)
                 
                 #Arrange by time
                 df <- arrange(df, etime)
                 
                 
                 
                 #Create drug column
                 df$drug <- drug
                 df$event <- adverse_event
                 
                 #Return df
                 df
               }


#Set titles
xtitle <- "Time to onset (Days)"
ytitle <- "Proportion (%)"

subtitle <- paste0("The lines represent the cumulative proportion of adverse events occurring by a given time point.")

#Set breaks for axes
xbreaks <- c(1, 5, 10, 30, 50, c(seq(0, 1000, 100)))

#Set limits
ylimits <- c(0, 1)
ybreaks <- seq(0, 1, 0.25)

comparisons <- c("CVAE vs NCVAE", "DIC vs other CVAE")

foreach(comparison = comparisons) %do% {

  if(comparison == "CVAE vs NCVAE") {
    tdata <- tdf %>% filter(event %in% c("CVAE", "Non-CVAE"))
    main_title <- paste0("Time to onset of adverse events (cardiovascular vs. non-cardiovascular) associated with bispecific T-cell engagers.")
    cbreaks <- c("Non-CVAE", "CVAE") 
    
    
  }  else if(comparison == "DIC vs other CVAE") {
    tdata <- tdf %>% filter(!event %in% c("CVAE", "NCVAE"))
    main_title <- paste0("Time to onset of selected cardiovascular adverse events associated with bispecific T-cell engagers.")
    cbreaks <- c(tdf$event %>% unique %>% {.[.!= "DIC"]},
                 "DIC")
    
    
  }


#Plot
ggplot(tdata, aes(etime, col = event)) + 
  stat_ecdf(geom = "step", lwd = 1.25) +
  scale_color_tableau(name = "",
                      breaks = cbreaks) +
  #Add scales
  scale_y_continuous(
    name = ytitle,
    breaks = ybreaks,
    expand = c(0.05, 0.02),
    labels = ~ . * 100) +
  scale_x_continuous(
    name = xtitle,
    breaks = xbreaks,
    expand = c(0.03, 0),
    trans = trans_cube
  ) +
  #Add plot title
  ggtitle(main_title,
          subtitle = subtitle) +
  #Theme
  theme_pubclean() +
  theme(text = element_text(size = 23),
        plot.title=element_text(face = "bold",hjust = 0.0, size = 18),
        plot.subtitle = element_text(face = "bold", size = 14, hjust = 0.0, color = "grey45"),
        axis.text = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 25, face = "bold"),
        axis.title.y = element_text(size = 25, face = "bold"),
        axis.line = element_line(colour = "black", size = 1.2),
        plot.margin = margin(0.5, 1, 0.5, 1, "cm"),
        legend.position = "bottom",
        legend.text = element_text(size = 13, face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.key.width = unit(2, "cm")) +
  guides(color = guide_legend(reverse = TRUE,
                              nrow = 1))


#Save plot
ggsave(plot = last_plot(),
       filename = paste0(
         folder,
         "/Time to Event ", comparison, ".pdf"),
       height = 9, width = 16, dpi = 600)

}
