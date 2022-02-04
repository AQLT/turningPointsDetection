source("R_simul/4-utils.R",encoding = "UTF-8")

all_pond <- do.call(rbind,
                    lapply(0:3, function(degree){
                      data = readRDS(sprintf("filters/fst_pdegree%i.RDS", degree))$weights
                      data$degree = degree
                      data$weight = sprintf("weight%i", seq_len(nrow(data)))
                      data
                    }))

mise_en_forme <- function(x, type){
  x %>%  readRDS() %>% 
    select_var() %>% 
    tidyr::pivot_longer(
      cols = starts_with("x"), 
      names_to = "date",
      values_to = "dephasage"
    ) %>% 
    mutate(date = as.numeric(substr(date,2,40)),
           turning_point_type = type)
}
tp_fst = rbind("results_simul/compile_tp/troughs_fst.RDS" %>% 
        mise_en_forme(type = "trough"),
  "results_simul/compile_tp/peaks_fst.RDS" %>% 
  mise_en_forme(type = "peak")) %>% 
  left_join(all_pond)

all_rev_fe2 <- select_series(readRDS("results_simul/compile_revisions/fst_fe_rev.RDS"))%>% 
  left_join(all_pond)
all_rev_ce2 <- select_series(readRDS("results_simul/compile_revisions/fst_ce_rev.RDS"))%>% 
  left_join(all_pond)
write.table(tp_fst[1:10000,],file = "tp_fst.csv",sep=";",row.names = FALSE,na = "")
write.table(all_rev_fe2[1:10000,],file = "rev_firstest_fst.csv",sep=";",row.names = FALSE,na = "")



tp_lp_rkhs <- rbind(rbind("results_simul/compile_tp/troughs_lp.RDS" %>% 
        mise_en_forme(type = "trough"),
      "results_simul/compile_tp/peaks_lp.RDS" %>% 
        mise_en_forme(type = "peak")) %>% 
  mutate(endpoints = method,
         method = "localpolynomial"),
  rbind("results_simul/compile_tp/troughs_rkhs.RDS" %>% 
          mise_en_forme(type = "trough"),
        "results_simul/compile_tp/peaks_rkhs.RDS" %>% 
          mise_en_forme(type = "peak")) %>% 
    mutate(endpoints = method,
           method = "rkhs"))
all_rev_fe <- select_series(readRDS("results_simul/compile_revisions/lp_fe_rev.RDS")) %>% 
  rbind(select_series(readRDS("results_simul/compile_revisions/rkhs_fe_rev.RDS")))
write.table(tp_lp_rkhs,file = "tp_lp_rkhs.csv",sep=";",row.names = FALSE,na = "")
write.table(all_rev_fe,file = "rev_firstest_lp_rkhs.csv",sep=";",row.names = FALSE,na = "")

all_tp_rkhs <- rbind(all_tp_rkhs,all_tp)
all_rev_rkhs_fe <- select_var(readRDS("results_simul/compile_revisions/rkhs_fe_rev.RDS"))
all_rev_rkhs_ce <- select_var(readRDS("results_simul/compile_revisions/rkhs_ce_rev.RDS"))
all_rev_rkhs_fe <- rbind(all_rev_rkhs_fe,all_rev_fe)
all_rev_rkhs_ce <- rbind(all_rev_rkhs_ce,all_rev_ce)
all_tp <- all_tp_rkhs %>% 
  tidyr::pivot_longer(
    cols = starts_with("x"), 
    names_to = "name",
    values_to = "value"
  )

