source("R_simul/4-utils.R",encoding = "UTF-8")
library(ggplot2)
library(scales)

all_tp <- merge(readRDS("results_simul/compile_tp_norev/troughs_lp.RDS"),
                readRDS("results_simul/compile_tp_norev/peaks_lp.RDS"),
                by=c("series","kernel", "method")) %>%
  select_var()
all_rev_fe <- readRDS("results_simul/compile_revisions/lp_fe_rev.RDS") |> 
  select_series() |> 
  select_mae()
all_rev_ce <- readRDS("results_simul/compile_revisions/lp_ce_rev.RDS") |> 
  select_series() |> 
  select_mae()

all_tp_rkhs <- 
  merge(readRDS("results_simul/compile_tp/troughs_rkhs.RDS"),
        readRDS("results_simul/compile_tp/peaks_rkhs.RDS"),
        by=c("series","kernel", "method")) %>%
  select_var()
all_rev_rkhs_fe <- readRDS("results_simul/compile_revisions/rkhs_fe_rev.RDS") |> 
  select_series() |> 
  select_mae()
all_rev_rkhs_ce <- readRDS("results_simul/compile_revisions/rkhs_ce_rev.RDS") |> 
  select_series() |> 
  select_mae()
all_tp_rkhs <- rbind(all_tp %>% mutate(article = "lpp"), 
                     all_tp_rkhs %>% mutate(article = "rkhs"))
all_rev_rkhs_fe <- rbind(all_rev_fe %>% mutate(article = "lpp"), 
                         all_rev_rkhs_fe %>% mutate(article = "rkhs"))
all_rev_rkhs_ce <- rbind(all_rev_ce %>% mutate(article = "lpp"), 
                         all_rev_rkhs_ce %>% mutate(article = "rkhs"))

all_tp_arima <- 
  merge(readRDS("results_simul/compile_tp/troughs_arima.RDS"),
        readRDS("results_simul/compile_tp/peaks_arima.RDS"),
        by=c("series","kernel", "method")) %>%
  select_var()
all_rev_arima_fe <- readRDS("results_simul/compile_revisions/arima_fe_rev.RDS") |> 
  select_series() |> 
  select_mae()
all_rev_arima_ce <- readRDS("results_simul/compile_revisions/arima_ce_rev.RDS") |> 
  select_series() |> 
  select_mae()

all_tp <- rbind(all_tp_rkhs, 
                all_tp_arima %>% mutate(article = "arima")) %>% 
  mutate(method = factor(method,levels = c("lc","ql","cq","daf", "frf", "gain", "phase", "auto_arima"),
                         ordered = TRUE),
         variability = factor(variability,
                              levels = c("lowvariability","mediumvariability","highvariability"),
                              ordered = TRUE),
         kernel = tolower(kernel))
all_rev_fe <- rbind(all_rev_rkhs_fe, 
                    all_rev_arima_fe %>% mutate(article = "rkhs")) %>% 
  mutate(method = factor(method,levels = c("lc","ql","cq","daf", "frf", "gain", "phase", "auto_arima"),
                         ordered = TRUE),
         variability = factor(variability,
                              levels = c("lowvariability","mediumvariability","highvariability"),
                              ordered = TRUE),
         kernel = tolower(kernel))
all_rev_ce <- rbind(all_rev_rkhs_ce, 
                    all_rev_arima_ce %>% mutate(article = "rkhs")) %>% 
  mutate(method = factor(method,levels = c("lc","ql","cq","daf", "frf", "gain", "phase", "auto_arima"),
                         ordered = TRUE),
         variability = factor(variability,
                              levels = c("lowvariability","mediumvariability","highvariability"),
                              ordered = TRUE),
         kernel = tolower(kernel))

normalise_rev <- function(x, ref = "lc", suff = "^(rev|X)"){
  # x <- all_rev_ce
    ref = x[(x$method == "lc") & (x$kernel == "henderson"),grep(suff,colnames(x)) ]
  for(k in unique(x$kernel)){
    for(m in unique(x$method)){
      if(nrow(x[x$method == m & x$kernel == k,grep(suff,colnames(x))]) > 0){
        
        x[x$method == m & x$kernel == k,grep(suff,colnames(x))] <- 
          x[x$method == m& x$kernel == k,grep(suff,colnames(x))] / ref
      }
    }
  }
  x
}
summarise_ref <- function(x, normalise = FALSE){
  if(normalise){
    x = x %>% normalise_rev()
    digits = 1
  } else{
    digits = 2
  }
  x %>% 
    group_by(variability, kernel, method) %>%
    summarise(across(
      .cols = where(is.numeric), 
      .fns = list(Mean = \(x) round(mean(x),digits)), 
      .names = "{col}"
    )) %>% 
    select(!c(rev.q6:rev.q10, length)) %>% 
    data.frame() %>%  filter(kernel == "henderson")  %>% 
    select(!c(kernel))
}
rev_tot = rbind(all_rev_fe %>% summarise_ref(),
                all_rev_ce %>% summarise_ref())
rev_rel = rbind(all_rev_fe %>% summarise_ref(normalise = TRUE),
                all_rev_ce %>% summarise_ref(normalise = TRUE))
rev_tot %>% filter(variability == "mediumvariability")
rev_rel %>% filter(variability == "mediumvariability")
rev_tot %>% filter(method %in% c("lc", "gain"))

rev_table <- rev_tot %>% filter(variability == "mediumvariability")%>%
  select(!c(variability)) %>% 
  mutate(method = recode(method, lc = "LC", ql = "QL",
                         cq = "CQ", daf = "DAF",
                         frf = "$b_{q,\\Gamma}$",
                         gain = "$b_{q,G}$",
                         phase = "$b_{q,\\varphi}$",
                         auto_arima = "ARIMA")) %>% 
  rename_at(vars(starts_with("rev")), function(x){
    sprintf("$q=%s$", gsub("rev.q","",x))
  }) %>% 
  rename(`Méthode` = method)

library(kableExtra)
title = "toto"
saveRDS(rev_table, file = "JMS_2022/data/simulations_revisions.RDS")
rev_table  %>%
  kable(format.args = list(digits = 2,
                           decimal.mark = ","),
        align = "c", booktabs = T, row.names = FALSE, 
        escape = FALSE, caption = title) %>%  
  kable_styling(latex_options=c(#"striped",  
    "hold_position")) %>% 
  pack_rows(index = c("$MAE_{fe}(q) = \\mathbb E\\left[
\\left|\\frac{(y_{t|t+q} -  y_{t|last})/ y_{t|last}\\right|
\\right]$"=8,
                      "$MAE_{ce}(q)=\\mathbb E\\left[
\\left|(y_{t|t+q} - y_{t|t+q+1})/y_{t|t+q+1}\\right|
\\right]$"=8), escape = FALSE)



### Déphasage Henderson 
all_tp %>%
  filter(kernel %in% c("henderson")) %>% 
  unique_series() %>%
  group_by(kernel, method, variability) %>%
  summarise_if(is.numeric, \(x) sum(!is.na(x))) %>%
  select (-length) %>% 
  mutate(sum = rowSums(across(where(is.numeric))))  %>%
  select(!starts_with("X")) %>% 
  filter(method == "lc") %>% 
  data.frame %>% 
  tail(3)


# Graphique sur le dephasage
pivot_data <- all_tp %>% 
  tidyr::pivot_longer(
    cols = starts_with("x"), 
    names_to = "name",
    values_to = "value"
  )

data_tp <- all_tp %>% 
  tidyr::pivot_longer(
    cols = starts_with("x"), 
    names_to = "name",
    values_to = "value"
  )%>% filter(kernel == "henderson") %>% 
  unique_series_pivot() %>% 
  mutate(method = recode(method, lc = "LC", ql = "QL",
                         cq = "CQ", daf = "DAF",
                         frf = "b['q, '] [Gamma]",
                         gain = "b['q, '] [G]",
                         phase = "b['q, '] [phi]"),
         variability = recode(variability,
                              lowvariability = "Faible variabilité",
                              mediumvariability = "Variabilité moyenne",
                              highvariability = "Forte variabilité")) %>% 
  na.omit()
data_tp_rel <- all_tp %>% 
  normalise_rev() %>% 
  tidyr::pivot_longer(
    cols = starts_with("x"), 
    names_to = "name",
    values_to = "value"
  )%>% filter(kernel == "henderson") %>% 
  unique_series_pivot() %>% 
  mutate(method = recode(method, lc = "LC", ql = "QL",
                         cq = "CQ", daf = "DAF",
                         frf = "b['q, '] [gamma]",
                         gain = "b['q, '] [G]",
                         phase = "b['q, '] [phi]",
                         auto_arima = "ARIMA"),
         variability = recode(variability,
                              lowvariability = "Faible variabilité",
                              mediumvariability = "Variabilité moyenne",
                              highvariability = "Forte variabilité")) %>% 
  na.omit()
p = ggplot(data_tp ,aes(x=method, y = value))+ 
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + AQLTools:::theme_aqltools() +
  labs(y="Déphasage", x = NULL) +# geom_jitter(width = 0.2, alpha = 0.1) +
  scale_x_discrete(labels = label_parse())
p 
ggMultisave("JMS_2022/img/simulations/phase_shift_simul", 
            plot =p,
            width = 8,height = 5)

ggplot(data_tp_rel ,aes(x=method, y = value))+ 
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + AQLTools:::theme_aqltools() +
  labs(y="Déphasage", x = NULL) +# geom_jitter(width = 0.2, alpha = 0.1) +
  scale_x_discrete(labels = label_parse())

## Révisions
all_tp %>%
  filter(method == "lc",
         !kernel %in% c("gaussian",
                        "parabolic", 
                        "uniform")) %>% 
  unique_series() %>%
  group_by(kernel, method, variability) %>%
  summarise_if(is.numeric, \(x) sum(!is.na(x))) %>%
  select (-length) %>% 
  mutate(sum = rowSums(across(where(is.numeric))))  %>%
  select(!starts_with("X")) %>% 
  filter(method == "lc") %>% 
  data.frame %>% 
  tail(3)
all_tp %>%
  filter(kernel %in% c("henderson")) %>% 
  unique_series() %>%
  group_by(kernel, method, variability) %>%
  summarise_if(is.numeric, \(x) sum(!is.na(x))) %>%
  select (-length) %>% 
  mutate(sum = rowSums(across(where(is.numeric))))  %>%
  select(!starts_with("X")) %>% 
  filter(method == "lc") %>% 
  data.frame %>% 
  tail(3)
all_tp %>%
  filter(method == "lc") %>% 
  # unique_series() %>%
  group_by(kernel, method, variability) %>%
  summarise_if(is.numeric, \(x) sum(!is.na(x))) %>%
  select (-length) %>% 
  mutate(sum = rowSums(across(where(is.numeric))))  %>%
  select(!starts_with("X")) %>%
  filter(method == "lc") %>% 
  group_by(variability) %>% 
  mutate(toto = paste(kernel[sum == max(sum)], collapse = " - ")) %>% 
  data.frame
unique(all_tp$series)
ggplot(pivot_data |> 
         filter(method == "lc"),aes(x=kernel, y = value))+ 
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + AQLTools:::theme_aqltools() +
  labs(title="Dephasage")

all_tp %>% 
  filter(method == "lc")
