source("R_simul/4-utils.R",encoding = "UTF-8")
library(ggplot2)

all_tp <- merge(readRDS("results_simul/compile_tp_norev/troughs_lp.RDS"),
                readRDS("results_simul/compile_tp_norev/peaks_lp.RDS"),
                by=c("series","kernel", "method"))
all_tp <- merge(readRDS("results_simul/compile_tp_norev/troughs_lp.RDS"),
      readRDS("results_simul/compile_tp_norev/peaks_lp.RDS"),
      by=c("series","kernel", "method")) %>%
  select_var() %>% 
  mutate(method = factor(method,levels = c("lc","ql","cq","daf"),ordered = TRUE))
nb_series <- all_tp %>%
  group_by(kernel, method) %>%
  summarise_if(is.numeric, \(x) sum(!is.na(x))) %>%
  data.frame
unique(all_tp$series)

all_rev_fe <- readRDS("results_simul/compile_revisions/lp_fe_rev.RDS") |> 
  select_series() |> 
  select_mae()
all_rev_ce <- readRDS("results_simul/compile_revisions/lp_ce_rev.RDS") |> 
  select_series() |> 
  select_mae()


mean2 <- function(x,...){
  round(mean(x,...), 2)
}
by_kernel_rev_fe <- all_rev_fe %>% 
  group_by(stats, Group, kernel, length) %>% 
  summarise_at(vars(rev.q0:rev.q10), mean2, na.rm = TRUE) %>% 
  data.frame%>% 
  arrange(length, Group, stats, kernel) %>% as.data.frame()

by_kernel_rev_ce <- all_rev_ce %>% 
  group_by(stats, Group, kernel, length) %>% 
  summarise_at(vars(rev.q0:rev.q10), mean2, na.rm = TRUE) %>% 
  data.frame%>% 
  arrange(length, Group, stats, kernel) %>% as.data.frame()

by_kernel <- all_tp %>% 
  tidyr::pivot_longer(
    cols = starts_with("x"), 
    names_to = "name",
    values_to = "value"
  ) %>% 
  group_by(kernel, length, method) %>% 
  summarise(
    moy = round(mean(value,na.rm = TRUE),2),
    mead = median(value, na.rm = TRUE),
    min = round(mean(value, na.rm = TRUE),2),
    max = max(value, na.rm = TRUE),
    nb_obs = nb_obs(value)
  ) %>% 
  arrange(length, kernel) %>% as.data.frame()

by_kernel %>% filter(length == 13)
by_kernel_rev_fe %>% filter(length == 13,
                            stats == "MAE")

by_kernel %>% filter(length == 13)
by_kernel_rev_fe %>% filter(length == 13,
                            stats == "MAE")
by_kernel_rev_ce %>% filter(length == 13,
                            stats == "MAE")
by_method <- all_tp %>% 
  tidyr::pivot_longer(
    cols = starts_with("x"), 
    names_to = "name",
    values_to = "value"
  ) %>% 
  group_by(method, length) %>% 
  summarise(
    moy = mean(value,na.rm = TRUE),
    mead = median(value, na.rm = TRUE),
    min = mean(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    nb_obs = nb_obs(value)
  )  %>% 
  mutate(method = factor(method,levels = c("lc","ql","cq","daf"),ordered = TRUE)) %>% 
  arrange(length, method) %>% as.data.frame()
all_tp %>% 
  tidyr::pivot_longer(
    cols = starts_with("x"), 
    names_to = "name",
    values_to = "value"
  ) %>% 
  group_by(kernel, method, length) %>% 
  summarise(
    moy = mean(value,na.rm = TRUE),
    mead = median(value, na.rm = TRUE),
    min = mean(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    nb_obs = nb_obs(value)
  ) %>% 
  mutate(method = factor(method,levels = c("lc","ql","cq","daf"),ordered = TRUE)) %>% 
  arrange(length, method, kernel) %>% as.data.frame()


all_tp <- merge(readRDS("results_simul/compile_tp/troughs_rkhs.RDS"),
                readRDS("results_simul/compile_tp/peaks_rkhs.RDS"),
                by=c("series","kernel", "method"))
all_tp <- select_var(all_tp)

all_tp %>% 
  tidyr::pivot_longer(
    cols = starts_with("x"), 
    names_to = "name",
    values_to = "value"
  ) %>% 
  group_by(method, length) %>% 
  summarise(
    moy = mean(value,na.rm = TRUE),
    mead = median(value, na.rm = TRUE),
    min = mean(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    nb_obs = nb_obs(value)
  )  %>% 
  arrange(length, method) %>% as.data.frame()


pivot_data <- all_tp %>% 
  tidyr::pivot_longer(
    cols = starts_with("x"), 
    names_to = "name",
    values_to = "value"
  )
all_rev_fe$method = factor(all_rev_fe$method,levels = c("lc","ql","cq","daf"), ordered = TRUE)
all_rev_fe$Group = factor(all_rev_fe$Group,levels = c("total","normal","turning-point"), ordered = TRUE)

all_rev_ce$method = factor(all_rev_ce$method,levels = c("lc","ql","cq","daf"), ordered = TRUE)
all_rev_ce$Group = factor(all_rev_ce$Group,levels = c("total","normal","turning-point"), ordered = TRUE)

ggplot(pivot_data[pivot_data$kernel=="henderson",],aes(x=method, y = value))+ 
  geom_violin(draw_quantiles = c(0.25,0.5,0.75))

ggplot(pivot_data[pivot_data$kernel=="henderson",],aes(x=method, y = value))+ 
  geom_violin(draw_quantiles = c(0.25,0.5,0.75)) +
  facet_wrap(vars(variability))

ggplot(all_rev_fe[(all_rev_fe$kernel=="henderson")&(all_rev_fe$stats=="RMSE"),],aes(x=method, y = rev.q0))+ 
  geom_violin(draw_quantiles = c(0.25,0.5,0.75)) +
  facet_wrap(vars(variability, Group), ncol = 3,
             scales = "free_y") + AQLTools:::theme_aqltools()

ggplot(all_rev_fe[(all_rev_fe$kernel=="henderson")&(all_rev_fe$stats=="RMSE"),],aes(x=method, y = rev.q1))+ 
  geom_violin(draw_quantiles = c(0.25,0.5,0.75)) +
  facet_wrap(vars(variability, Group), ncol = 3,
             scales = "free_y") + AQLTools:::theme_aqltools()

ggplot(all_rev_fe[(all_rev_ce$kernel=="henderson")&(all_rev_fe$stats=="RMSE"),],aes(x=method, y = rev.q0))+ 
  geom_violin(draw_quantiles = c(0.25,0.5,0.75)) +
  facet_wrap(vars(variability, Group), ncol = 3,
             scales = "free_y") + AQLTools:::theme_aqltools()

ggplot(all_rev_fe[(all_rev_ce$kernel=="henderson")&(all_rev_fe$stats=="RMSE"),],aes(x=method, y = rev.q1))+ 
  geom_violin(draw_quantiles = c(0.25,0.5,0.75)) +
  facet_wrap(vars(variability, Group), ncol = 3,
             scales = "free_y") + AQLTools:::theme_aqltools()
  


ggplot(pivot_data[pivot_data$method=="ql",],aes(x=kernel, y = value))+ 
  geom_violin(draw_quantiles = c(0.25,0.5,0.75))

ggplot(pivot_data[pivot_data$method=="ql",],aes(x=kernel, y = value))+ 
  geom_violin(draw_quantiles = c(0.25,0.5,0.75)) +
  facet_wrap(vars(variability),nrow = 2)


global_data <-  pivot_data%>% 
  group_by(kernel, method, length, variability) %>% 
  summarise(
    moy = mean(value,na.rm = TRUE),
    med = median(value, na.rm = TRUE),
    min = mean(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    nb_obs = nb_obs(value)
  )  %>% 
  na.omit %>% 
  arrange(kernel, method, variability) %>% 
  data.frame

mean_100 <- function(x){
  (mean(x,na.rm = TRUE))
}
data_rev_fe <- all_rev_fe %>% 
  group_by(stats, Group, weight, degree, length) %>% 
  summarise_at(vars(rev.q0:rev.q10), mean_100)%>% 
  left_join(all_pond) %>% data.frame

data_rev_ce <- all_rev_ce %>% 
  group_by(stats, weight, degree, length) %>% 
  summarise_at(vars(rev.q0:rev.q10), mean_100)%>% 
  left_join(all_pond) %>% data.frame