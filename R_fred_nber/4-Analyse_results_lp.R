source("R_fred/4-utils.R",encoding = "UTF-8")

# all_t <- select_var(readRDS("results_nber/compile_tp/troughs_lp.RDS"))
# 
# nb_series <- all_t %>%
#   group_by(kernel, method) %>%
#   summarise_if(is.numeric, \(x) sum(!is.na(x))) %>%
#   data.frame
# 
# series_used <- function(x){
#   years <- grep("^X",colnames(x))
#   for(i in years){
#     x[!is.na(x[,i]),i] <- x[!is.na(x[,i]),"series"]
#   }
#   x %>% 
#     group_by(kernel, method) %>% 
#     summarise_at(years, \(x) paste(sort(unique(na.omit(x))), collapse = " - ")) %>% 
#     data.frame
# }
# series_name <- series_used(all_t)




all_tp <- merge(readRDS("results_nber/compile_tp/troughs_lp.RDS"),
                readRDS("results_nber/compile_tp/peaks_lp.RDS"),
                by=c("series","kernel", "method"))
all_tp <- select_var(all_tp)
nb_series <- all_tp %>%
  group_by(kernel, method) %>%
  summarise_if(is.numeric, \(x) sum(!is.na(x))) %>%
  data.frame

all_rev_fe <- select_series(readRDS("results_nber/compile_revisions/lp_fe_rev.RDS"))
all_rev_ce <- select_series(readRDS("results_nber/compile_revisions/lp_ce_rev.RDS"))


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


all_tp <- merge(readRDS("results_nber/compile_tp/troughs_rkhs.RDS"),
                readRDS("results_nber/compile_tp/peaks_rkhs.RDS"),
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
