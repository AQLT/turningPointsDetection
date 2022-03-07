source("R_fred_nber/4-utils.R",encoding = "UTF-8")
library(patchwork)

all_tp <- merge(readRDS("results_nber/compile_tp_norev/troughs_lp.RDS"),
                readRDS("results_nber/compile_tp_norev/peaks_lp.RDS"),
                by=c("series","kernel", "method")) %>%
  select_var()
all_rev_fe <- readRDS("results_nber/compile_revisions/lp_fe_rev.RDS") |> 
  select_series() |> 
  select_mae()
all_rev_ce <- readRDS("results_nber/compile_revisions/lp_ce_rev.RDS") |> 
  select_series() |> 
  select_mae()

all_tp_rkhs <- 
  merge(readRDS("results_nber/compile_tp/troughs_rkhs.RDS"),
        readRDS("results_nber/compile_tp/peaks_rkhs.RDS"),
        by=c("series","kernel", "method")) %>%
  select_var()
all_rev_rkhs_fe <- readRDS("results_nber/compile_revisions/rkhs_fe_rev.RDS") |> 
  select_series() |> 
  select_mae()
all_rev_rkhs_ce <- readRDS("results_nber/compile_revisions/rkhs_ce_rev.RDS") |> 
  select_series() |> 
  select_mae()
all_tp_rkhs <- rbind(all_tp %>% mutate(article = "lpp"), 
                     all_tp_rkhs %>% mutate(article = "rkhs"))
all_rev_rkhs_fe <- rbind(all_rev_fe %>% mutate(article = "lpp"), 
                         all_rev_rkhs_fe %>% mutate(article = "rkhs"))
all_rev_rkhs_ce <- rbind(all_rev_ce %>% mutate(article = "lpp"), 
                         all_rev_rkhs_ce %>% mutate(article = "rkhs"))

all_tp <- all_tp_rkhs %>% 
  mutate(method = factor(method,levels = c("lc","ql","cq","daf", "frf", "gain", "phase"),
                         ordered = TRUE),
         kernel = tolower(kernel))
all_rev_fe <- all_rev_rkhs_fe %>% 
  mutate(method = factor(method,levels = c("lc","ql","cq","daf", "frf", "gain", "phase"),
                         ordered = TRUE),
         kernel = tolower(kernel))
all_rev_ce <- all_rev_rkhs_ce %>% 
  mutate(method = factor(method,levels = c("lc","ql","cq","daf", "frf", "gain", "phase"),
                         ordered = TRUE),
         kernel = tolower(kernel))


series <- "RETAILx"
tp_keep = "2020.25"
column_to_remove <- grep(tp_keep, grep("^X", colnames(all_tp),value = TRUE), value = TRUE, invert = TRUE)
all_tp = all_tp %>% filter(series %in% !!series) %>% 
  mutate(method2 = recode(method, lc = "Linear-Constant~(LC)",
                          ql = "Quadratic-Linear~(QL)",
                          cq = "Cubic-quadratic~(CQ)", daf = "DAF",
                          frf = "b['q, '] [gamma]",
                          gain = "b['q, '] [G]",
                          phase = "b['q, '] [phi]")) %>% 
  mutate(title = sprintf("%s%s",
                         ifelse(kernel == "henderson", as.character(method2), ""),
                         ifelse((kernel == "henderson") & article == "lpp", "",
                                sprintf("Noyau~%s", kernel))),
         subtitle = sprintf("Déphasage de %i mois",
                            round(X2020.25))) %>%
select(!c(!!column_to_remove, article))


### Import fst
w_to_keep = c(#"weight231", 
  "weight232", 
  #"weight233", 
  "weight234", 
  "weight235", 
  "weight236"
  )


all_pond <- do.call(rbind,
                    lapply(0:3, function(degree){
                      data = readRDS(sprintf("filters/fst_pdegree%i.RDS", degree))$weights
                      data$degree = degree
                      data$weight = sprintf("weight%i", seq_len(nrow(data)))
                      data
                    }))

all_tp_ref <- merge(readRDS("results_nber/compile_tp_norev/troughs_lp.RDS"),
                    readRDS("results_nber/compile_tp_norev/peaks_lp.RDS"),
                    by=c("series","kernel", "method")) |>
  select_var() |> format_ref_fst()

all_rev_fe_ref <- readRDS("results_nber/compile_revisions/lp_fe_rev.RDS") |> 
  select_series() |>  select_mae()|> format_ref_fst()
all_rev_ce_ref <- readRDS("results_nber/compile_revisions/lp_ce_rev.RDS") |> 
  select_series() |> select_mae()|> format_ref_fst()

all_tp_fst <- merge(readRDS("results_nber/compile_tp/troughs_fst.RDS"),
                readRDS("results_nber/compile_tp/peaks_fst.RDS"),
                by=c("series","degree", "weight")) |> 
  select_var() |> 
  format_fst()|>
  filter(weight%in% w_to_keep)
all_tp_fst_norm <- all_tp_fst |> normalise_fst() %>% filter(series %in% !!series)

column_to_remove <- grep(tp_keep, grep("^X", colnames(all_tp_fst),value = TRUE), value = TRUE, invert = TRUE)
all_tp_fst <- all_tp_fst %>% filter(series %in% !!series) %>% 
  select(!c(!!column_to_remove, length)) %>%
  left_join(all_pond) %>% 
  mutate(title = sprintf("FST~degré~%s~alpha == %.2f~beta == %.2f~gamma == %.2f",
                         degree,
                         fidelity.weight,
                         smoothness.weight,
                         timeliness.weight),
         subtitle = sprintf("Déphasage de %i mois",
                            round(X2020.25)))

all_rev_fe_fst <- select_series(readRDS("results_nber/compile_revisions/fst_fe_rev.RDS")) |> 
  format_fst() |> 
  select_mae()|>
  filter(weight%in% w_to_keep) %>% filter(series %in% !!series)
all_rev_fe_fst <- all_rev_fe_fst |> normalise_fst() 

all_rev_ce_fst <- select_series(readRDS("results_nber/compile_revisions/fst_ce_rev.RDS")) |> 
  format_fst() |> 
  select_mae() |>
  filter(weight%in% w_to_keep) %>% filter(series %in% !!series)
all_rev_ce_fst <- all_rev_ce_fst |> normalise_fst()

# Fin import_fst


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
                  method = "phase")
     )
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
                                  method = "phase")
)
all_titles = lapply(all_mod, function(x){
  title = all_tp %>% filter(kernel == x$kernel,
                            method == x$method) %>% 
    select(c(title, subtitle))
  # gsub(" ","~", title)
  title
})
all_mod_kernel <- list(
  henderson = list(kernel = "henderson",method = "lc"), 
  biweight = list(kernel = "biweight", method = "lc"), 
  gaussian = list(kernel = "gaussian", method = "lc"), 
  parabolic = list(kernel = "parabolic", method = "lc"), 
  triangular = list(kernel = "triangular", method = "lc"), 
  tricube = list(kernel = "tricube", method = "lc"),
  triweight = list(kernel = "triweight", method = "lc"),
  uniform = list(kernel = "uniform", method = "lc")
  )
all_titles_kernel <- lapply(all_mod_kernel, function(x){
  title = all_tp %>% filter(kernel == x$kernel,
                            method == x$method) %>% 
    select(c(subtitle))
  c(sprintf("Noyau~%s", Hmisc::capitalize(x$kernel)),title)
})

all_mod_est <- lapply(all_mod, \(x) do.call(extract_est_data, x))
all_range <- range(sapply(all_mod_est, range,na.rm = TRUE))

all_mod_est_kernel <- lapply(all_mod_kernel, \(x) do.call(extract_est_data, x))
all_range_kernel <- range(sapply(all_mod_est_kernel, range,na.rm = TRUE))

all_mod_fst_ <- expand.grid(weight = w_to_keep,
                           degree = 2:3)
all_mod_fst = apply(all_mod_fst_,1, as.list)
names(all_mod_fst) <- 
  sprintf("%s_d%s", 
          all_mod_fst_[,"weight"], 
          all_mod_fst_[,"degree"])
all_titles_fst = lapply(all_mod_fst, function(x){
  title = all_tp_fst %>% filter(weight == x$weight,
                            degree == x$degree) %>% 
    select(c(title, subtitle))
  # gsub(" ","~", title)
  title
})
all_mod_est_fst <- lapply(all_mod_fst, \(x) do.call(extract_est_data_fst, x))
all_mod_fst_range <- range(sapply(all_mod_est_fst, range,na.rm = TRUE))

all_range <- range(c(all_range, all_mod_fst_range, all_range_kernel))

plots <- lapply(names(all_mod_est), 
                \(x,...)plot_est(all_mod_est[[x]], 
                                 titre = parse(text = all_titles[[x]][1]),
                                 sous_titre = all_titles[[x]][2],
                                 ...),
                limits_y = all_range)
plots_kernel <- lapply(names(all_mod_est_kernel), 
                    \(x,...)plot_est(all_mod_est_kernel[[x]], 
                                     titre = parse(text = all_titles_kernel[[x]][1]),
                                     sous_titre = all_titles_kernel[[x]][2],
                                     ...),
                    limits_y = all_range)

plots_fst <- lapply(names(all_mod_est_fst), 
                \(x,...)plot_est(all_mod_est_fst[[x]], 
                                 titre = parse(text = all_titles_fst[[x]][1]),
                                 sous_titre = all_titles_fst[[x]][2],
                                  ...),
                limits_y = all_range)
plots_fst <- plots_fst[rep(c(1,5),4) +rep(0:3, each = 2)]

wrap_plots(plots_fst[1:4])
wrap_plots(plots_fst[-(1:4)])

wrap_plots(plots_kernel[1:4])
wrap_plots(plots_kernel[c(1,8)],ncol = 1)

ggMultisave("JMS_2022/img/nber/retailx_lp", 
            plot =wrap_plots(plots[1:4],ncol = 2),
       width = 7,height = 5)
ggMultisave("JMS_2022/img/nber/retailx_rkh",
             plot = wrap_plots(plots[5:7],ncol = 2),
       width = 7,height = 5)
ggMultisave("JMS_2022/img/nber/retailx_fstp1",
            plot = wrap_plots(plots_fst[1:4],ncol = 2),
            width = 7,height = 5)
ggMultisave("JMS_2022/img/nber/retailx_fstp2",
            plot = wrap_plots(plots_fst[-(1:4)],ncol = 2),
            width = 7,height = 5)


