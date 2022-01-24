source("R_simul/4-utils.R",encoding = "UTF-8")
library(ggplot2)

all_tp <- merge(readRDS("results_simul/compile_tp/troughs_lp.RDS"),
                readRDS("results_simul/compile_tp/peaks_lp.RDS"),
                by=c("series","kernel", "method"))
all_rev_fe <- select_var(readRDS("results_simul/compile_revisions/lp_fe_rev.RDS"))
all_rev_ce <- select_var(readRDS("results_simul/compile_revisions/lp_ce_rev.RDS"))
all_tp <- select_var(all_tp)

all_tp_rkhs <- select_var(
  merge(readRDS("results_simul/compile_tp/troughs_rkhs.RDS"),
                readRDS("results_simul/compile_tp/peaks_rkhs.RDS"),
                by=c("series","kernel", "method"))
  )
all_tp_rkhs <- rbind(all_tp_rkhs,all_tp %>% filter(kernel == "henderson",method=="ql"))
all_rev_rkhs_fe <- select_var(readRDS("results_simul/compile_revisions/rkhs_fe_rev.RDS"))
all_rev_rkhs_ce <- select_var(readRDS("results_simul/compile_revisions/rkhs_ce_rev.RDS"))
all_rev_rkhs_fe <- rbind(all_rev_rkhs_fe,all_rev_fe %>% filter(kernel == "henderson",method=="ql"))
all_rev_rkhs_ce <- rbind(all_rev_rkhs_ce,all_rev_ce %>% filter(kernel == "henderson",method=="ql"))


all_tp$method <- factor(all_tp$method,levels = c("lc","ql","cq","daf"),ordered = TRUE)
all_rev_fe$method = factor(all_rev_fe$method,levels = c("lc","ql","cq","daf"), ordered = TRUE)
all_rev_fe$Group = factor(all_rev_fe$Group,levels = c("total","normal","turning-point"), ordered = TRUE)

all_rev_ce$method = factor(all_rev_ce$method,levels = c("lc","ql","cq","daf"), ordered = TRUE)
all_rev_ce$Group = factor(all_rev_ce$Group,levels = c("total","normal","turning-point"), ordered = TRUE)

all_tp_rkhs$method <- factor(all_tp_rkhs$method,levels = c("ql","accuracy","smoothness","phase","frf"),ordered = TRUE)
all_rev_rkhs_fe$method = factor(all_rev_rkhs_fe$method,levels = c("ql","accuracy","smoothness","phase","frf"), ordered = TRUE)
all_rev_rkhs_fe$Group = factor(all_rev_rkhs_fe$Group,levels = c("total","normal","turning-point"), ordered = TRUE)

all_rev_rkhs_ce$method = factor(all_rev_rkhs_ce$method,levels = c("ql","accuracy","smoothness","phase","frf"), ordered = TRUE)
all_rev_rkhs_ce$Group = factor(all_rev_rkhs_ce$Group,levels = c("total","normal","turning-point"), ordered = TRUE)


pivot_data <- all_tp %>% 
  tidyr::pivot_longer(
    cols = starts_with("x"), 
    names_to = "name",
    values_to = "value"
  )
pivot_data_rkhs <- all_tp_rkhs %>% 
  tidyr::pivot_longer(
    cols = starts_with("x"), 
    names_to = "name",
    values_to = "value"
  )

ggplot(pivot_data[pivot_data$kernel=="henderson",],aes(x=method, y = value))+ 
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + AQLTools:::theme_aqltools() +
  labs(title="Dephasage")
ggsave("point_these/img/simul/lp_fixed_kernel_tp.pdf",
       width = 7, height = 5)

ggplot(all_rev_fe[(all_rev_fe$kernel=="henderson")&(all_rev_fe$stats=="RMSE"),],aes(x=method, y = rev.q0))+ 
  geom_boxplot() +
  facet_wrap(vars(variability, Group), ncol = 3,
             scales = "free_y") + AQLTools:::theme_aqltools()+
  labs(title="RMSE-FE q0")
ggsave("point_these/img/simul/lp_fixed_kernel_feq0.pdf",
       width = 7, height = 5)

ggplot(all_rev_fe[(all_rev_fe$kernel=="henderson")&(all_rev_fe$stats=="RMSE"),],aes(x=method, y = rev.q1))+ 
  geom_boxplot() +
  facet_wrap(vars(variability, Group), ncol = 3,
             scales = "free_y") + AQLTools:::theme_aqltools()+
  labs(title="RMSE-FE q1")
ggsave("point_these/img/simul/lp_fixed_kernel_feq1.pdf",
       width = 7, height = 5)

ggplot(all_rev_ce[(all_rev_ce$kernel=="henderson")&(all_rev_ce$stats=="RMSE"),],aes(x=method, y = rev.q0))+ 
  geom_boxplot() +
  facet_wrap(vars(variability, Group), ncol = 3,
             scales = "free_y") + AQLTools:::theme_aqltools()+
  labs(title="RMSE-CE q0")
ggsave("point_these/img/simul/lp_fixed_kernel_ceq0.pdf",
       width = 7, height = 5)

ggplot(all_rev_ce[(all_rev_ce$kernel=="henderson")&(all_rev_ce$stats=="RMSE"),],aes(x=method, y = rev.q1))+ 
  geom_boxplot() +
  facet_wrap(vars(variability, Group), ncol = 3,
             scales = "free_y") + AQLTools:::theme_aqltools()+
  labs(title="RMSE-CE q1")
ggsave("point_these/img/simul/lp_fixed_kernel_ceq1.pdf",
       width = 7, height = 5)

ggplot(pivot_data[pivot_data$method=="ql",],aes(x=kernel, y = value))+ 
  geom_boxplot() +
  facet_wrap(vars(variability),nrow = 2) + AQLTools:::theme_aqltools() +
  labs(title = "Dephasage QL")

ggsave("point_these/img/simul/lp_fixed_method_tp.pdf",
       width = 7, height = 5)

all_rev_fe[all_rev_fe$kernel=="triangular","rev.q0"] <- NA
all_rev_fe[all_rev_fe$kernel=="parabolic","rev.q0"] <- NA

ggplot(all_rev_fe[(all_rev_fe$method=="ql")&(all_rev_fe$stats=="RMSE")&(all_rev_fe$Group=="turning-point"),],aes(x=kernel, y = rev.q0))+ 
  geom_boxplot() +
  facet_wrap(vars(variability, Group), ncol = 1,
             scales = "free_y") + AQLTools:::theme_aqltools()+
  labs(title = "RMSE-FE QL q0")

ggsave("point_these/img/simul/lp_fixed_method_feq0.pdf",
       width = 7, height = 5)

all_rev_fe[all_rev_fe$kernel=="triangular","rev.q1"] <- NA
all_rev_fe[all_rev_fe$kernel=="parabolic","rev.q1"] <- NA
ggplot(all_rev_fe[(all_rev_fe$method=="ql")&(all_rev_fe$stats=="RMSE")&(all_rev_fe$Group=="turning-point"),],aes(x=kernel, y = rev.q1))+ 
  geom_boxplot() +
  facet_wrap(vars(variability, Group), ncol = 1,
             scales = "free_y") + AQLTools:::theme_aqltools()+
  labs(title = "RMSE-FE QL q1")
ggsave("point_these/img/simul/lp_fixed_method_feq1.pdf",
       width = 7, height = 5)

all_rev_ce[all_rev_ce$kernel=="gaussian","rev.q0"] <- NA
all_rev_ce[all_rev_ce$kernel=="gaussian","rev.q1"] <- NA
ggplot(all_rev_ce[(all_rev_ce$method=="ql")&(all_rev_ce$stats=="RMSE")&(all_rev_ce$Group=="turning-point"),],aes(x=kernel, y = rev.q0))+ 
  geom_boxplot() +
  facet_wrap(vars(variability, Group), ncol = 1,
             scales = "free_y") + AQLTools:::theme_aqltools()+
  labs(title = "RMSE-CE QL q0")
ggsave("point_these/img/simul/lp_fixed_method_ceq0.pdf",
       width = 7, height = 5)

ggplot(all_rev_ce[(all_rev_ce$method=="ql")&(all_rev_ce$stats=="RMSE")&(all_rev_ce$Group=="turning-point"),],aes(x=kernel, y = rev.q1))+ 
  geom_boxplot() +
  facet_wrap(vars(variability, Group), ncol = 1,
             scales = "free_y") + AQLTools:::theme_aqltools()+
  labs(title = "RMSE-FE QL q1")
ggsave("point_these/img/simul/lp_fixed_method_ceq1.pdf",
       width = 7, height = 5)
######################################
#########" RKHS


ggplot(pivot_data_rkhs,aes(x=method, y = value))+ 
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + AQLTools:::theme_aqltools() +
  labs(title="Dephasage")
ggsave("point_these/img/simul/rkhs_tp.pdf",
       width = 7, height = 4.5)

ggplot(all_rev_rkhs_fe[(all_rev_rkhs_fe$stats=="RMSE"),],aes(x=method, y = rev.q0))+ 
  geom_boxplot() +
  facet_wrap(vars(variability, Group), ncol = 3,
             scales = "free_y") + AQLTools:::theme_aqltools()+
  labs(title="RMSE-FE q0")
ggsave("point_these/img/simul/rkhs_feq0.pdf",
       width = 7, height = 5)

ggplot(all_rev_rkhs_fe[(all_rev_rkhs_fe$stats=="RMSE"),],aes(x=method, y = rev.q1))+ 
  geom_boxplot() +
  facet_wrap(vars(variability, Group), ncol = 3,
             scales = "free_y") + AQLTools:::theme_aqltools()+
  labs(title="RMSE-FE q1")
ggsave("point_these/img/simul/rkhs_feq1.pdf",
       width = 7, height = 5)

ggplot(all_rev_rkhs_ce[(all_rev_rkhs_ce$stats=="RMSE"),],aes(x=method, y = rev.q0))+ 
  geom_boxplot() +
  facet_wrap(vars(variability, Group), ncol = 3,
             scales = "free_y") + AQLTools:::theme_aqltools()+
  labs(title="RMSE-CE q0")
ggsave("point_these/img/simul/rkhs_ceq0.pdf",
       width = 7, height = 5)

ggplot(all_rev_rkhs_ce[(all_rev_rkhs_ce$stats=="RMSE"),],aes(x=method, y = rev.q1))+ 
  geom_boxplot() +
  facet_wrap(vars(variability, Group), ncol = 3,
             scales = "free_y") + AQLTools:::theme_aqltools()+
  labs(title="RMSE-CE q1")
ggsave("point_these/img/simul/rkhs_ceq1.pdf",
       width = 7, height = 5)

cat(sprintf('### Résultats avec les polynômes locaux\n\n```{r,out.width="100%s"}\nknitr::include_graphics("%s")\n```\n',"%",gsub("point_these/","", list.files("point_these/img/simul",full.names = TRUE))),sep = "\n")

all_f <- file.info(list.files("point_these/img/fred",full.names = TRUE))
all_f <- all_f[order(all_f$mtime),]
all_f <- rownames(all_f)
nom_f <- rep(TRUE,length(all_f))
nom_f[grep("rkhs",all_f)] <- FALSE
nom_f <- ifelse(nom_f, "polynômes locaux", "RKHS")
cat(sprintf('### Résultats avec les %s\n\n```{r,out.width="100%s"}\nknitr::include_graphics("%s")\n```\n',
            nom_f,
            "%",
            gsub("point_these/","", all_f)),sep = "\n")

        