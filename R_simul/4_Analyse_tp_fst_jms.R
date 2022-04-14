library(plot3D)
library(magrittr)  

source("R_simul/4-utils.R",encoding = "UTF-8")

format_fst <- function(x){
  weights_ordered = sprintf("weight%i", sort(unique(as.numeric(gsub("weight", "", x$weight)))))
  x <- x |> 
    mutate(weight = factor(weight, 
                           levels = weights_ordered,
                           ordered = TRUE))
  if("variability" %in% colnames(x)){
    x <- x |> 
      mutate(variability = factor(variability,
                                          levels = c("lowvariability","mediumvariability","highvariability"),
                                          ordered = TRUE))
  }
  x
}
format_ref_fst <- function(x){
  x |> 
    filter(method == "lc", kernel == "henderson")  %>% 
    set_rownames(.$series) |> 
    select(!c(method, kernel, variability, length, series))
  
}
normalise_fst <- function(x, suff = "^(rev|X)"){
  name_x <- deparse(substitute(x))
  ref = get(paste0(name_x,"_ref"))
  for(w in unique(x$weight)){
    for(d in 0:3){
      indices = x$weight==w & x$degree ==d
      x[indices,grep(suff,colnames(x))] <- 
        x[indices,grep(suff,colnames(x))] / ref[x[indices,"series"],]
    }
  }
  x
}


# tmp = all_pond %>% filter(!((round(timeliness.weight,4) %in% round(seq(0,1,length.out = 21),2)) &
#                               ( round(smoothness.weight,4) %in% round(seq(0,1,length.out = 21),2)) &
#                               ( round(fidelity.weight,4) %in% round(seq(0,1,length.out = 21),2)))) %>% select(weight) %>% unique()
# dput(tmp[,1])
w_to_remove = c("weight22", "weight23", "weight24", "weight25", "weight26", 
                "weight237", "weight238", "weight239", "weight240"
)

all_tp_ref <- merge(readRDS("results_simul/compile_tp_norev/troughs_lp.RDS"),
                readRDS("results_simul/compile_tp_norev/peaks_lp.RDS"),
                by=c("series","kernel", "method")) |>
  select_var() |> format_ref_fst()

all_rev_fe_ref <- readRDS("results_simul/compile_revisions/lp_fe_rev.RDS") |> 
  select_series() |>  select_mae()|> format_ref_fst()
all_rev_ce_ref <- readRDS("results_simul/compile_revisions/lp_ce_rev.RDS") |> 
  select_series() |> select_mae()|> format_ref_fst()

all_tp <- merge(readRDS("results_simul/compile_tp_norev/troughs_fst.RDS"),
                readRDS("results_simul/compile_tp_norev/peaks_fst.RDS"),
                by=c("series","degree", "weight")) |> 
  select_var() |> 
  format_fst()|>
  filter(!weight%in% w_to_remove)
all_tp <- all_tp |> normalise_fst()

all_rev_fe <- select_series(readRDS("results_simul/compile_revisions/fst_fe_rev.RDS")) |> 
  format_fst() |> 
  select_mae()|>
  filter(!weight%in% w_to_remove)
all_rev_fe <- all_rev_fe |> normalise_fst()

all_rev_ce <- select_series(readRDS("results_simul/compile_revisions/fst_ce_rev.RDS")) |> 
  format_fst() |> 
  select_mae() |>
  filter(!weight%in% w_to_remove)
all_rev_ce <- all_rev_ce |> normalise_fst()

all_pond <- do.call(rbind,
                    lapply(0:3, function(degree){
                      data = readRDS(sprintf("filters/fst_pdegree%i.RDS", degree))$weights
                      data$degree = degree
                      data$weight = sprintf("weight%i", seq_len(nrow(data)))
                      data
                    }))

variability = "mediumvariability"
width = 7;height = 5
list_dev = c("pdf","svg", "jpeg")
for(variability in c("mediumvariability", "lowvariability", "highvariability")){
  print(variability)
  global_data <- all_tp |> 
    filter(variability == !!variability) |> 
    tidyr::pivot_longer(
      cols = starts_with("x"), 
      names_to = "name",
      values_to = "value"
    ) |> 
    group_by(variability, weight, degree, length) |> 
    summarise(
      moy = mean(value,na.rm = TRUE),
      med = median(value, na.rm = TRUE),
      min = min(value, na.rm = TRUE),
      q25 = quantile(value,na.rm = TRUE,probs = 0.25),
      q75 = quantile(value,na.rm = TRUE,probs = 0.75),
      max = max(value, na.rm = TRUE),
      nb_obs = nb_obs(value)
    )  |> 
    na.omit() |> 
    format_fst() |> 
    arrange(length, weight, degree) |> 
    left_join(all_pond) |> data.frame()
  
  data_rev_fe <- all_rev_fe |> 
    filter(variability == !!variability) |> 
    group_by(weight, degree, length) |> 
    summarise_at(vars(rev.q0:rev.q10), median)|> 
    left_join(all_pond) |> 
    format_fst() |> 
    arrange(length, weight, degree) |> data.frame()
  
  data_rev_ce <- all_rev_ce |> 
    filter(variability == !!variability) |> 
    group_by(weight, degree, length) |> 
    summarise_at(vars(rev.q0:rev.q10), median)|> 
    left_join(all_pond) |> data.frame()
  for (stat in c("min", "med", "moy", "max", "q25", "q75")){
    for(dev in list_dev){
      if(dev == "jpeg"){
        get(dev)(file = sprintf("JMS_2022/img/simulations/fst_%s_tp_%s.%s",
                                variability,
                                stat, "jpg"),width = width, height = height,
                 units = "in", res = 300)
      }else{
        get(dev)(file = sprintf("JMS_2022/img/simulations/fst_%s_tp_%s.%s",
                                variability,
                                stat, dev),width = width, height = height)
      }
      par(mfrow = c(2,2), 
          mar = 0 + c(1, 0, 1, 0),
          mai = c(0.2, 0.2, 0.2, 0.2))
      for(i in 0:3){
        plot_fst_tp(global_data, degree = i, title = sprintf("degré %s", i),
                    color = stat)
      }
      dev.off()
    }
    
  }
  
  ## FE
  for(q in 0:2){
    for(dev in list_dev){
      if(dev == "jpeg"){
        get(dev)(file = sprintf("JMS_2022/img/simulations/fst_%s_fe_q%s.%s",
                                variability, q, "jpg"),width = width, height = height,
                 units = "in", res = 300)
      }else{
        get(dev)(file = sprintf("JMS_2022/img/simulations/fst_%s_fe_q%s.%s",
                                variability, q, dev),width = width, height = height)
      }
      par(mfrow = c(2,2), 
          mar = 0 + c(1, 0, 1, 0),
          mai = c(0.2, 0.2, 0.2, 0.2))
      for(i in 0:3){
        plot_fst_rev(data_rev_fe, degree = i, title = sprintf("degré %s", i),
                     color = sprintf("rev.q%i",q),limits = TRUE, by = 1)
      }
      dev.off()
    }
  }
  
  
  ## CE
  for(q in 0:2){
    for(dev in list_dev){
      if(dev == "jpeg"){
        get(dev)(file = sprintf("JMS_2022/img/simulations/fst_%s_ceq%s.%s",
                                variability, q, "jpg"),width = width, height = height,
                 units = "in", res = 300)
      }else{
        get(dev)(file = sprintf("JMS_2022/img/simulations/fst_%s_ceq%s.%s",
                                variability, q, dev),width = width, height = height)
      }
      par(mfrow = c(2,2), 
          mar = 0 + c(1, 0, 1, 0),
          mai = c(0.2, 0.2, 0.2, 0.2))
      for(i in 0:3){
        plot_fst_rev(data_rev_ce, degree = i, title = sprintf("degré %s", i),
                     color = sprintf("rev.q%i",q),limits = TRUE, by = 1)
      }
      dev.off()
    }
  }
  
}