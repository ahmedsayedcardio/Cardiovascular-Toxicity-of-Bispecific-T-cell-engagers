#Get baseline chars
bite_ss <- x[bite == 1] %>% nrow %>% comma
bli_ss <- x[bli == 1] %>% nrow %>% comma
tec_ss <- x[tec == 1] %>% nrow %>% comma
total_ss <- x %>% nrow %>% comma


drug_props <- x[bite == 1, .(.N), by = .(bli, tec, glo, epc, mos)] %>%
  gprop

ll_mm_props <- x[bite == 1, .(.N), by = .(ll, mm)] %>%
  gprop


dcvae_comorbid_pct <- x[bite == 1, .N, by = .(cvae_comorbid, dcvae)] %>% 
  filter(!is.na(dcvae)) %>% group_by(cvae_comorbid) %>% mutate(prop = N/sum(N)) %>%
  gprop %>% filter(cvae_comorbid == 1 & dcvae == 1) %>% spull(prop)

dcvae_no_comorbid_pct <- x[bite == 1, .N, by = .(cvae_comorbid, dcvae)] %>% 
  filter(!is.na(dcvae)) %>% group_by(cvae_comorbid) %>% mutate(prop = N/sum(N)) %>%
  gprop %>% filter(cvae_comorbid == 0 & dcvae == 1) %>% spull(prop)


occp_props <- x[bite == 1, .(.N), by = .(occp_cod)] %>%
  gprop %>% arrange(-prop)

sex_props <- x[bite == 1, .(.N), by = .(sex)] %>%
  gprop

country_props <- x[bite == 1, .(.N), by = .(reporter_country)] %>%
  gprop %>% arrange(-prop)

age_iqr <- x[bite == 1, age_yr] %>% quantile(na.rm = T) %>% r1

#Save stuff needed for rendering (everything except x)
save(list = ls() %>% {.[. != "x"]}, file = "Bite Rendering.RData")
