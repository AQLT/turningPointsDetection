# library(RJDemetra)
source("R_fred_nber/4-utils.R",encoding = "UTF-8")

library(patchwork)
library(rjdfilters)


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

w_to_keep = c(#"weight231", 
  "weight232", 
  #"weight233", 
  "weight234", 
  "weight235", 
  "weight236"
)
fst2 <- readRDS(sprintf("filters/fst_pdegree%i.RDS",2))$coefs[c(232,234,235,236)]
fst2 <- lapply(fst2, \(x) x$`h=6`)
fst3 <- readRDS(sprintf("filters/fst_pdegree%i.RDS",3))$coefs[c(232,234,235,236)]
fst3 <- lapply(fst3, \(x) x$`h=6`)

prevs_imp_h_lp <- 
  lapply(c(rkhs_f, fst2, fst3), function(coefs){
    do.call(ts.union, lapply(data, function(y){
      prevs = implicit_forecast(y=y, coefs = coefs)
      prevs_a = ts(c(tail(y,1), prevs), start = tail(time(y),1),
                   frequency = frequency(y))
      prevs_a
    }))
  })
names(prevs_imp_h_lp) <- c("frf", "gain", "phase", 
                           sprintf("%s_d%i", rep(w_to_keep,2),
                                   rep(2:3, each = length(w_to_keep))))
list_kernel <- c("Henderson", "Uniform", "Biweight", "Triweight", "Tricube",
                 "Gaussian", "Triangular", "Parabolic")
list_method <- c("LC","QL", "CQ", "DAF")
kernel = "Henderson"
method = "LC"
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

# #verif
# est_coef = lapply(all_prevs,function(x){
#   # x = all_prevs[[1]]
#   # i <- 1
#   res = do.call(ts.union, lapply(1:ncol(x),function(i){
#     x_ = na.omit(x[,i])[-1]
#     window(na.omit(henderson(ts(c(data[[i]], x_), start = start(data[[1]]), frequency = 12), musgrave = FALSE,length = 13)),
#            start = tp_date - nb_dates_before/12)
#   }))
#   colnames(res) <- colnames(x)
#   res
# })
# round(all_mod_est$lc_h - est_coef$henderson_lc,2)
# round(all_mod_est$cq_h - est_coef$henderson_cq,1)
# round(all_mod_est$daf_h - est_coef$henderson_daf,1)


data <- do.call(ts.union, data)
data <- window(data, start = tp_date - nb_dates_before/frequency(data))

all_prevs = c(prevs_imp_lp, prevs_imp_h_lp)
all_prevs = lapply(all_prevs, function(x){
  colnames(x) <- colnames(data)
  x
})


all_titles = list(lc_h = list(title = "Linear-Constant~(LC)", subtitle = "Déphasage de 3 mois"), 
                  ql_h = list(title = "Quadratic-Linear~(QL)", subtitle = "Déphasage de 2 mois"), 
                  cq_h = list(title = "Cubic-quadratic~(CQ)", subtitle = "Déphasage de 2 mois"), 
                  daf_h = list(title = "DAF", subtitle = "Déphasage de 2 mois"), 
                  rkhs_frf = list(title = "b['q, '] [Gamma]", 
                                  subtitle = "Déphasage de 3 mois"), 
                  rkhs_gain = list(title = "b['q, '] [G]", 
                                   subtitle = "Déphasage de 2 mois"),
                  rkhs_phase = list(
                    title = "b['q, '] [phi]", subtitle = "Déphasage de 6 mois"))
all_titles_kernel = 
  list(henderson = list("Noyau~Henderson", subtitle = "Déphasage de 3 mois"), 
       biweight = list("Noyau~Biweight", subtitle = "Déphasage de 3 mois"), 
       gaussian = list("Noyau~Gaussian", subtitle = "Déphasage de 3 mois"), 
       parabolic = list("Noyau~Parabolic", subtitle = "Déphasage de 3 mois"), 
       triangular = list("Noyau~Triangular", subtitle = "Déphasage de 3 mois"), 
       tricube = list("Noyau~Tricube", subtitle = "Déphasage de 3 mois"), 
       triweight = list("Noyau~Triweight", subtitle = "Déphasage de 3 mois"), 
       uniform = list("Noyau~Uniform", subtitle = "Déphasage de 10 mois"))
all_titles_fst = 
  list(weight232_d2 = list(title = "FST~degré~2~alpha == 0.05~beta == 0.05~gamma == 0.90", 
                           subtitle = "Déphasage de 3 mois"), 
       weight234_d2 = list(title = "FST~degré~2~alpha == 0.05~beta == 0.00~gamma == 0.95", 
                           subtitle = "Déphasage de 2 mois"), 
       weight235_d2 = list(title = "FST~degré~2~alpha == 0.00~beta == 0.05~gamma == 0.95", 
                           subtitle = "Déphasage de 3 mois"), 
       weight236_d2 = list(title = "FST~degré~2~alpha == 0.00~beta == 0.00~gamma == 1.00", 
                           subtitle = "Déphasage de 2 mois"), 
       weight232_d3 = list(title = "FST~degré~3~alpha == 0.05~beta == 0.05~gamma == 0.90", 
                           subtitle = "Déphasage de 2 mois"), 
       weight234_d3 = list(title = "FST~degré~3~alpha == 0.05~beta == 0.00~gamma == 0.95", 
                           subtitle = "Déphasage de 2 mois"), 
       weight235_d3 = list(title = "FST~degré~3~alpha == 0.00~beta == 0.05~gamma == 0.95", 
                           subtitle = "Déphasage de 2 mois"), 
       weight236_d3 = list(title = "FST~degré~3~alpha == 0.00~beta == 0.00~gamma == 1.00", 
                           subtitle = "Déphasage de 2 mois"))


data_plots <- all_prevs[c("henderson_lc", "henderson_ql", "henderson_cq", "henderson_daf", "frf", "gain", "phase")]
data_kernels <- all_prevs[c("henderson_lc", "biweight_lc", "gaussian_lc", "parabolic_lc", 
                            "triangular_lc", "tricube_lc", "triweight_lc", "uniform_lc")]
data_fst <- all_prevs[c("weight232_d2", 
                        "weight234_d2", "weight235_d2", "weight236_d2", "weight232_d3", 
                        "weight234_d3", "weight235_d3", "weight236_d3")]

all_ranges = range(c(sapply(c(data_plots, data_fst), range,na.rm = TRUE), range(data, na.rm = TRUE)))
# all_ranges = range(c(sapply(c(data_plots), range,na.rm = TRUE), range(data, na.rm = TRUE)))



plots <- lapply(seq_along(data_plots), 
                \(x,...)plot_prevs(
                  data, 
                  data_prevs = data_plots[[x]], 
                  titre = parse(text = all_titles[[x]][[1]]),
                  sous_titre = all_titles[[x]][[2]],
                  ...))
plots_kernel <-  lapply(seq_along(data_kernels), 
                        \(x,...)plot_prevs(
                          data, 
                          data_prevs = data_kernels[[x]], 
                          titre = parse(text = all_titles_kernel[[x]][[1]]),
                          sous_titre = all_titles_kernel[[x]][[2]],
                          ...))
plots_fst <- lapply(seq_along(data_fst), 
                    \(x,...)plot_prevs(data, 
                                       data_prevs = data_fst[[x]], 
                                       titre = parse(text = all_titles_fst[[x]][[1]]),
                                       sous_titre = all_titles_fst[[x]][[2]],
                                       ...))

plots_fst <- plots_fst[rep(c(1,5),4) +rep(0:3, each = 2)]


wrap_plots(plots[1:4],ncol = 2)
wrap_plots(plots[5:7],ncol = 2)

wrap_plots(plots_fst[1:4])
wrap_plots(plots_fst[-(1:4)])

wrap_plots(plots_kernel)
wrap_plots(plots_kernel[c(1,8)],ncol = 1)


colnames(arima_prevs) <- colnames(data)

rkhs_arima <- c(plots[5:7],
                list(plot_prevs(
                  data, 
                  data_prevs = arima_prevs, 
                  titre = "Modèle ARIMA",
                  sous_titre = "Déphasage 3 mois")))
rkhs_arima[[4]]
ggMultisave("JMS_2022/img/nber/retailx_lp_implicit_forecast", 
            plot =wrap_plots(plots[1:4],ncol = 2),
            width = 7,height = 5)
ggMultisave("JMS_2022/img/nber/retailx_rkhs_arima_implicit_forecast",
            plot = wrap_plots(rkhs_arima,ncol = 2),
            width = 7,height = 5)
ggMultisave("JMS_2022/img/nber/retailx_fstp1_implicit_forecast",
            plot = wrap_plots(plots_fst[1:4],ncol = 2),
            width = 7,height = 5)
ggMultisave("JMS_2022/img/nber/retailx_fstp2_implicit_forecast",
            plot = wrap_plots(plots_fst[-(1:4)],ncol = 2),
            width = 7,height = 5)


c(list(a = "t", b = 1), 3)
plot_prevs(
  data, 
  data_prevs = arima_prevs, 
  titre = "Modèle ARIMA",
  sous_titre = "Déphasage 3 mois")
plot_est(
  prev_inter , 
  titre = "Modèle ARIMA",
  sous_titre = "Déphasage 3 mois",
  limits_y = c(12.9092094671746, 13.2946713382075))
ggMultisave("JMS_2022/img/nber/retailx_arima_implicit_forecast",
            plot = plot_prevs(
              data, 
              data_prevs = arima_prevs, 
              titre = "Modèle ARIMA",
              sous_titre = "Déphasage 3 mois"),
            width = 7,height = 2.5)
ggMultisave("JMS_2022/img/nber/retailx_arima",
            plot = plot_est(
              prev_inter , 
              titre = "Modèle ARIMA",
              sous_titre = "Déphasage 3 mois",limits_y = c(12.9092094671746, 13.2946713382075)),
            width = 7,height = 2.5)
