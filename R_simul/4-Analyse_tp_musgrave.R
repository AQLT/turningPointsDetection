source("R_simul/4-utils.R",encoding = "UTF-8")
library(ggplot2)

all_tp <- merge(readRDS("results_simul/compile_tp/troughs_lp.RDS"),
                readRDS("results_simul/compile_tp/peaks_lp.RDS"),
                by=c("series","kernel", "method"))
all_tp <- merge(readRDS("results_simul/compile_tp/troughs_lp.RDS"),
                readRDS("results_simul/compile_tp/peaks_lp.RDS"),
                by=c("series","kernel", "method")) %>%
  select_var() %>% 
  mutate(method = factor(method,levels = c("lc","ql","cq","daf"),ordered = TRUE))
nb_series <- all_tp %>%
  group_by(kernel, method) %>%
  summarise_if(is.numeric, \(x) sum(!is.na(x))) %>%
  data.frame
unique(all_tp$series)

all_tp_norev <- merge(readRDS("results_simul/compile_tp_norev/troughs_lp.RDS"),
                      readRDS("results_simul/compile_tp_norev/peaks_lp.RDS"),
                      by=c("series","kernel", "method")) %>%
  select_var() %>% 
  mutate(method = factor(method,levels = c("lc","ql","cq","daf"),ordered = TRUE))
nb_series_norev <- all_tp_norev %>%
  group_by(kernel, method) %>%
  summarise_if(is.numeric, \(x) sum(!is.na(x))) %>%
  data.frame

nb_series_norev_diff <- nb_series
nb_series_norev_diff[,-(1:2)] <- nb_series[,-(1:2)] - nb_series_norev[,-(1:2)]
nb_series_norev %>% 
  filter(kernel =="henderson", method == "lc") %>% `[`(-c(1,2,22)) %>% 
  sum()
all_tp_diff <- all_tp
all_tp_diff[,-c(1,2,3,24)] <- all_tp_diff[,-c(1,2,3,24)] - all_tp_norev[,-c(1,2,3,24)]
all_tp_diff[is.na(all_tp_diff)] <- 0
((all_tp_diff %>% filter(kernel == "henderson", method == "lc") %>% 
  `[`(-c(1,2,3,23,24)) %>% abs())>0) %>% 
  sum()
