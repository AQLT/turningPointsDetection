# library(RJDemetra)
source("R_fred_nber/4-utils.R",encoding = "UTF-8")

library(patchwork)
library(rjdfilters)
Sys.setlocale("LC_TIME", "en_US.UTF-8") 

s <- "data/byseries_nber/RETAILx.RDS"
tp_keep = "2020.25"
tp_date = as.numeric(tp_keep)
nb_est = 10
nb_dates_before <- 6
data <- readRDS(s)
data <- data[names(data) >= tp_keep][1:nb_est]
data_info <- readRDS(sub("byseries_nber", "byseriesinfo_nber", s))
data_info <- data_info[names(data_info) >= tp_keep][1:nb_est]
names(data) <- as.character(zoo::as.yearmon(as.numeric(names(data))))

arima_prevs = do.call(ts.union, lapply(data, function(y){
  prevs = forecast::auto.arima(y, max.Q = 0, max.D = 0, max.P = 0) |> 
    forecast::forecast(6)
  ts(c(tail(y,1), prevs$mean), start = tail(time(y),1),
     frequency = frequency(y))
}))

prev_inter = lapply(1:ncol(arima_prevs),function(i){
  x = na.omit(arima_prevs[,i])
  data_all = ts(c(window(data[[i]], end = time(x)[1] -1/frequency(data[[i]])), 
                  x),
                start = start(data[[i]]),
                frequency = frequency(data[[i]]))
  henderson(data_all,length = 13,musgrave = FALSE)
})
tp_arima = lapply(prev_inter,function(x){
  AQLThesis::turning_points(x)
})

names(prev_inter) <- names(data)
names(tp_arima) <- sapply(prev_inter, \(x) tail(time(na.omit(x)),1))

library(AQLThesis)
compute_time_lag(data = tp_arima,
                 peaks = nber_tp_m[,"Peak"],
                 troughs = nber_tp_m[,"Trough"],
                 type = "no_revisions")
#Déphasage de 3 mois

prev_inter <- window(do.call(ts.union, prev_inter), 
                     start = tp_date - nb_dates_before/frequency(data[[1]]))

rkhs_f <- readRDS("filters/rkhs_rw_p3.RDS")$`h=6`
rkhs_f

prevs_imp_h_lp <- 
  lapply(rkhs_f, function(coefs){
    do.call(ts.union, lapply(data, function(y){
      prevs = implicit_forecast(y=y, coefs = coefs)
      prevs_a = ts(c(tail(y,1), prevs), start = tail(time(y),1),
                   frequency = frequency(y))
      prevs_a
    }))
  })
names(prevs_imp_h_lp) <- c("frf", "gain", "phase")
list_kernel <- c("Henderson")
list_method <- c("LC","QL", "CQ", "DAF")
prevs_imp_lp <- do.call(c,  
                        lapply(list_kernel, function(kernel){
                          lapply(list_method, function(method){
                            do.call(ts.union, lapply(seq_along(data), function(i){
                              y <- data[[i]]
                              l = 13
                              icr = data_info[[i]][sprintf("icr-%s", l)]
                              lp_coef = lp_filter(horizon = (l-1)/2,
                                                  kernel = kernel,
                                                  endpoints = method,
                                                  ic = icr)$filters.coef
                              prevs = implicit_forecast(y=y, coefs = lp_coef)
                              prevs_a = ts(c(tail(y,1), prevs), start = tail(time(y),1),
                                           frequency = frequency(y))
                              prevs_a
                            }))
                          })
                        }))
names(prevs_imp_lp) <- sprintf("%s_%s", rep(tolower(list_kernel), each = length(list_method)), 
                               rep(tolower(list_method),length(list_kernel)))



data <- do.call(ts.union, data)
data <- window(data, start = tp_date - nb_dates_before/frequency(data))

all_prevs = c(prevs_imp_lp, prevs_imp_h_lp)
all_prevs = lapply(all_prevs, function(x){
  colnames(x) <- colnames(data)
  x
})


all_titles = list(lc_h = list(title = "Linear-Constant~(LC)~(Musgrave)", subtitle = "Déphasage de 3 mois"), 
                  ql_h = list(title = "Quadratic-Linear~(QL)", subtitle = "Déphasage de 2 mois"), 
                  cq_h = list(title = "Cubic-quadratic~(CQ)", subtitle = "Déphasage de 2 mois"), 
                  daf_h = list(title = "DAF", subtitle = "Déphasage de 2 mois"), 
                  rkhs_frf = list(title = "b['q, '] [Gamma]", 
                                  subtitle = "Déphasage de 3 mois"), 
                  rkhs_gain = list(title = "b['q, '] [G]", 
                                   subtitle = "Déphasage de 2 mois"),
                  rkhs_phase = list(
                    title = "b['q, '] [phi]", subtitle = "Déphasage de 6 mois"))


data_plots <- all_prevs[c("henderson_lc", "henderson_ql", "henderson_cq", "henderson_daf", "frf", "gain", "phase")]

all_ranges = range(c(sapply(c(data_plots), range,na.rm = TRUE), range(data, na.rm = TRUE)))


plots <- lapply(seq_along(data_plots), 
                \(x,...)plot_prevs(
                  data, 
                  data_prevs = data_plots[[x]], 
                  titre = parse(text = all_titles[[x]][[1]]),
                  # sous_titre = all_titles[[x]][[2]],
                  ...))

colnames(arima_prevs) <- colnames(data)

rkhs_arima <- c(plots[5:7],
                list(plot_prevs(
                  data, 
                  data_prevs = arima_prevs, 
                  titre = "ARIMA")))
ggMultisave("C:/Users/zw20hj/github/slides/2022 - 06 - SAPW/img/retailx_lp_implicit_forecast", 
            plot =wrap_plots(plots[1:4],ncol = 2),
            width = 7,height = 5,out = "pdf")
ggMultisave("C:/Users/zw20hj/github/slides/2022 - 06 - SAPW/img/retailx_rkhs_arima_implicit_forecast",
            plot = wrap_plots(rkhs_arima,ncol = 2),
            width = 7,height = 5,out = "pdf")


all_tp <- merge(readRDS("results_nber/compile_tp_norev/troughs_lp.RDS"),
                readRDS("results_nber/compile_tp_norev/peaks_lp.RDS"),
                by=c("series","kernel", "method")) %>%
  select_var()

all_tp_rkhs <- 
  merge(readRDS("results_nber/compile_tp_norev/troughs_rkhs.RDS"),
        readRDS("results_nber/compile_tp_norev/peaks_rkhs.RDS"),
        by=c("series","kernel", "method")) %>%
  select_var()

all_tp_rkhs <- rbind(all_tp %>% mutate(article = "lpp"), 
                     all_tp_rkhs %>% mutate(article = "rkhs"))
all_tp_arima <- 
  merge(readRDS("results_nber/compile_tp_norev/troughs_arima.RDS"),
        readRDS("results_nber/compile_tp_norev/peaks_arima.RDS"),
        by=c("series","kernel", "method")) %>%
  select_var() %>% mutate(article = "arima")


all_tp <- all_tp_rkhs %>%
  rbind(all_tp_arima) %>% 
  mutate(method = factor(method,levels = c("lc","ql","cq","daf", "frf", "gain", "phase","auto_arima"),
                         ordered = TRUE),
         kernel = tolower(kernel))


series <- "RETAILx"
tp_keep = "2020.25"
column_to_remove <- grep(tp_keep, grep("^X", colnames(all_tp),value = TRUE), value = TRUE, invert = TRUE)
all_tp = 
  all_tp %>% filter(series %in% !!series) %>% 
  mutate(method2 = recode(method, lc = "Linear-Constant~(LC)~(Musgrave)",
                          ql = "Quadratic-Linear~(QL)",
                          cq = "Cubic-quadratic~(CQ)", daf = "DAF",
                          frf = "b['q, '] [Gamma]",
                          gain = "b['q, '] [G]",
                          phase = "b['q, '] [phi]",
                          auto_arima = "ARIMA")) %>% 
  mutate(title = sprintf("%s%s",
                         ifelse(kernel == "henderson", as.character(method2), ""),
                         ifelse((kernel != "henderson") & article == "lpp",
                                sprintf("Noyau~%s", kernel), ""
                         )),
         subtitle = sprintf("Déphasage de %i mois",
                            round(X2020.25))) %>%
  select(!c(!!column_to_remove, article))


all_mod <- list(lc_h = list(kernel = "henderson",
                            method = "lc"),
                ql_h = list(kernel = "henderson",
                            method = "ql"),
                cq_h = list(kernel = "henderson",
                            method = "cq"),
                daf_h = list(kernel = "henderson",
                             method = "daf"),
                rkhs_frf = list(kernel = "henderson",
                                method = "frf"),
                rkhs_gain = list(kernel = "henderson",
                                 method = "gain"),
                rkhs_phase = list(kernel = "henderson",
                                  method = "phase"),
                arima = list(kernel = "henderson",
                             method = "auto_arima")
)
all_titles = lapply(all_mod, function(x){
  title = all_tp %>% filter(kernel == x$kernel,
                            method == x$method) %>% 
    select(c(title, subtitle))
  # gsub(" ","~", title)
  title
})

all_mod_est <- lapply(all_mod, \(x) do.call(extract_est_data, x))
all_range <- range(sapply(all_mod_est, range,na.rm = TRUE))

plots <- lapply(names(all_mod_est), 
                \(x,...)plot_est(all_mod_est[[x]], 
                                 titre = parse(text = all_titles[[x]][1]),
                                 # sous_titre = all_titles[[x]][2],
                                 ...),
                limits_y = all_range)

wrap_plots(plots[1:4])
wrap_plots(plots[-(1:4)])

ggMultisave("C:/Users/zw20hj/github/slides/2022 - 06 - SAPW/img/retailx_lp", 
            plot =wrap_plots(plots[1:4],ncol = 2),
            width = 7,height = 5,out = "pdf")
ggMultisave("C:/Users/zw20hj/github/slides/2022 - 06 - SAPW/img/retailx_rkhs_arima",
            plot = wrap_plots(plots[5:8],ncol = 2),
            width = 7,height = 5,out = "pdf")


