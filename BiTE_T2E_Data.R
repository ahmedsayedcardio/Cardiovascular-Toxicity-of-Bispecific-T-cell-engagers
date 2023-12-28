#use gamma regression to compare CVAE to NCVAE and TE to other CVAE in terms of timing
cvae_time_p <- wilcox.test(x[bite == 1 & cvae == 0, aetime],
                           x[bite == 1 & cvae == 1, aetime]
                           )$p.value %>% p

dic_time_p <- wilcox.test(x[bite == 1 & cvae == 1 & dic == 0, aetime],
                          x[bite == 1 & cvae == 1 & dic == 1, aetime]
                          )$p.value %>% p

hypotension_time_p <- wilcox.test(x[bite == 1 & cvae == 1 & hypotension == 0, aetime],
            x[bite == 1 & cvae == 1 & hypotension == 1, aetime]
            )$p.value %>% p

cvae_mtime <- x[bite == 1 & cvae == 1, aetime] %>% median(na.rm = T)
ncvae_mtime <- x[bite == 1 & cvae == 0, aetime] %>% median(na.rm = T)
dic_mtime <- x[bite == 1 & cvae == 1 & dic == 1, aetime] %>% median(na.rm = T)
ndic_mtime <- x[bite == 1 & cvae == 1 & dic == 0, aetime] %>% median(na.rm = T)
hypotension_mtime <- x[bite == 1 & cvae == 1 & hypotension == 1, aetime] %>% median(na.rm = T)
nhypotension_mtime <- x[bite == 1 & cvae == 1 & hypotension == 0, aetime] %>% median(na.rm = T)
