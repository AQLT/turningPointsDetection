source("R_fred/4-utils.R",encoding = "UTF-8")
library(ggplot2)

all_tp <- merge(readRDS("results_nber/compile_tp/troughs_lp.RDS"),
                readRDS("results_nber/compile_tp/peaks_lp.RDS"),
                by=c("series","kernel", "method"))
all_rev_fe <- select_series(readRDS("results_nber/compile_revisions/lp_fe_rev.RDS"))
all_rev_ce <- select_series(readRDS("results_nber/compile_revisions/lp_ce_rev.RDS"))
all_tp <- select_var(all_tp)

all_tp_rkhs <- select_var(
  merge(readRDS("results_nber/compile_tp/troughs_rkhs.RDS"),
        readRDS("results_nber/compile_tp/peaks_rkhs.RDS"),
        by=c("series","kernel", "method"))
)
all_tp_rkhs <- rbind(all_tp_rkhs,all_tp %>% filter(kernel == "henderson",method=="ql"))
all_rev_rkhs_fe <- select_series(readRDS("results_nber/compile_revisions/rkhs_fe_rev.RDS"))
all_rev_rkhs_ce <- select_series(readRDS("results_nber/compile_revisions/rkhs_ce_rev.RDS"))
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
  ) %>% 
  filter(length == 13)
pivot_data_rkhs <- all_tp_rkhs %>% 
  tidyr::pivot_longer(
    cols = starts_with("x"), 
    names_to = "name",
    values_to = "value"
  ) %>% 
  filter(length == 13)
pivot_data_rkhs
pivot_data %>% filter(kernel == "henderson",method=="ql") %>% group_by(method) %>% summarise(nb = sum(!is.na(value)))
pivot_data_rkhs %>% filter(kernel == "henderson",method=="ql") %>% group_by(method) %>% summarise(nb = sum(!is.na(value)))


all_rev_fe <- all_rev_fe[which((all_rev_fe$rev.q0<0.75)&(all_rev_fe$rev.q1<5)),]
all_rev_ce <- all_rev_ce[which((all_rev_ce$rev.q0<0.4)&(all_rev_ce$rev.q1<0.5)),]

pivot_data[pivot_data$kernel=="henderson",] %>% group_by(method) %>% 
  summarise(quantile(value,0.5, na.rm = TRUE))

(ggplot(na.omit(pivot_data[pivot_data$kernel=="henderson",]),aes(x=method, y = value))+ 
  geom_boxplot() + AQLTools:::theme_aqltools() +
  labs(title="Dephasage") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)))/(ggplot(na.omit(pivot_data[pivot_data$kernel=="henderson",]),aes(x=method, y = value))+ 
  geom_violin(draw_quantiles = c(0.5)) + AQLTools:::theme_aqltools() +
  labs(title="Dephasage") + scale_y_continuous(breaks = scales::pretty_breaks(n = 10)))


ggplot(na.omit(pivot_data[pivot_data$kernel=="henderson",]),aes(x=method, y = value))+ 
  geom_boxplot() + AQLTools:::theme_aqltools() +
  labs(title="Dephasage") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
ggsave("point_these/img/fred/lp_fixed_kernel_tp.pdf",
       width = 7, height = 5)

ggplot(all_rev_fe[(all_rev_fe$kernel=="henderson")&(all_rev_fe$stats=="RMSE"),],aes(x=method, y = rev.q0))+ 
  geom_boxplot() +
  facet_wrap(vars(Group), ncol = 3,
             scales = "free_y") + AQLTools:::theme_aqltools()+
  labs(title="RMSE-FE q0")
ggsave("point_these/img/fred/lp_fixed_kernel_feq0.pdf",
       width = 7, height = 5)

ggplot(all_rev_fe[(all_rev_fe$kernel=="henderson")&(all_rev_fe$stats=="RMSE"),],aes(x=method, y = rev.q1))+ 
  geom_boxplot() +
  facet_wrap(vars(Group), ncol = 3,
             scales = "free_y") + AQLTools:::theme_aqltools()+
  labs(title="RMSE-FE q1")
ggsave("point_these/img/fred/lp_fixed_kernel_feq1.pdf",
       width = 7, height = 5)

ggplot(all_rev_ce[(all_rev_ce$kernel=="henderson")&(all_rev_ce$stats=="RMSE"),],aes(x=method, y = rev.q0))+ 
  geom_boxplot() +
  facet_wrap(vars(Group), ncol = 3,
             scales = "free_y") + AQLTools:::theme_aqltools()+
  labs(title="RMSE-CE q0")
ggsave("point_these/img/fred/lp_fixed_kernel_ceq0.pdf",
       width = 7, height = 5)

ggplot(all_rev_ce[(all_rev_ce$kernel=="henderson")&(all_rev_ce$stats=="RMSE"),],aes(x=method, y = rev.q1))+ 
  geom_boxplot() +
  facet_wrap(vars(Group), ncol = 3,
             scales = "free_y") + AQLTools:::theme_aqltools()+
  labs(title="RMSE-CE q1")
ggsave("point_these/img/fred/lp_fixed_kernel_ceq1.pdf",
       width = 7, height = 5)

ggplot(pivot_data[pivot_data$method=="ql",],aes(x=kernel, y = value))+ 
  geom_boxplot() +
  AQLTools:::theme_aqltools() +
  labs(title = "Dephasage QL")

ggsave("point_these/img/fred/lp_fixed_method_tp.pdf",
       width = 7, height = 5)

ggplot(all_rev_fe[(all_rev_fe$method=="ql")&(all_rev_fe$stats=="RMSE")&(all_rev_fe$Group=="turning-point"),],aes(x=kernel, y = rev.q0))+ 
  geom_boxplot() +
  facet_wrap(vars(Group), ncol = 1,
             scales = "free_y") + AQLTools:::theme_aqltools()+
  labs(title = "RMSE-FE QL q0")

ggsave("point_these/img/fred/lp_fixed_method_feq0.pdf",
       width = 7, height = 5)

ggplot(all_rev_fe[(all_rev_fe$method=="ql")&(all_rev_fe$stats=="RMSE")&(all_rev_fe$Group=="turning-point"),],aes(x=kernel, y = rev.q1))+ 
  geom_boxplot() +
  facet_wrap(vars(Group), ncol = 1,
             scales = "free_y") + AQLTools:::theme_aqltools()+
  labs(title = "RMSE-FE QL q1")
ggsave("point_these/img/fred/lp_fixed_method_feq1.pdf",
       width = 7, height = 5)

ggplot(all_rev_ce[(all_rev_ce$method=="ql")&(all_rev_ce$stats=="RMSE")&(all_rev_ce$Group=="turning-point"),],aes(x=kernel, y = rev.q0))+ 
  geom_boxplot() +
  facet_wrap(vars(Group), ncol = 1,
             scales = "free_y") + AQLTools:::theme_aqltools()+
  labs(title = "RMSE-CE QL q0")
ggsave("point_these/img/fred/lp_fixed_method_ceq0.pdf",
       width = 7, height = 5)

ggplot(all_rev_ce[(all_rev_ce$method=="ql")&(all_rev_ce$stats=="RMSE")&(all_rev_ce$Group=="turning-point"),],aes(x=kernel, y = rev.q1))+ 
  geom_boxplot() +
  facet_wrap(vars(Group), ncol = 1,
             scales = "free_y") + AQLTools:::theme_aqltools()+
  labs(title = "RMSE-FE QL q1")
ggsave("point_these/img/fred/lp_fixed_method_ceq1.pdf",
       width = 7, height = 5)
######################################
#########" RKHS

ggplot(pivot_data_rkhs,aes(x=method, y = value))+ 
  geom_boxplot() +
  AQLTools:::theme_aqltools() +
  labs(title="Dephasage")
ggsave("point_these/img/fred/rkhs_tp.pdf",
       width = 7, height = 5)


all_rev_rkhs_fe <- all_rev_rkhs_fe[which((all_rev_rkhs_fe$rev.q0<0.2)&(all_rev_rkhs_fe$rev.q1<5)),]
all_rev_rkhs_ce <- all_rev_rkhs_ce[which((all_rev_rkhs_ce$rev.q0<.2)&(all_rev_rkhs_ce$rev.q1<0.5)),]

ggplot(all_rev_rkhs_fe[(all_rev_rkhs_fe$stats=="RMSE"),],aes(x=method, y = rev.q0))+ 
  geom_boxplot() +
  facet_wrap(vars(Group), ncol = 3,
             scales = "free_y") + AQLTools:::theme_aqltools()+
  labs(title="RMSE-FE q0")
ggsave("point_these/img/fred/rkhs_feq0.pdf",
       width = 7, height = 5)

ggplot(all_rev_rkhs_fe[(all_rev_rkhs_fe$stats=="RMSE"),],aes(x=method, y = rev.q1))+ 
  geom_boxplot() +
  facet_wrap(vars(Group), ncol = 3,
             scales = "free_y") + AQLTools:::theme_aqltools()+
  labs(title="RMSE-FE q1")
ggsave("point_these/img/fred/rkhs_feq1.pdf",
       width = 7, height = 5)

ggplot(all_rev_rkhs_ce[(all_rev_rkhs_ce$stats=="RMSE"),],aes(x=method, y = rev.q0))+ 
  geom_boxplot() +
  facet_wrap(vars(Group), ncol = 3,
             scales = "free_y") + AQLTools:::theme_aqltools()+
  labs(title="RMSE-CE q0")
ggsave("point_these/img/fred/rkhs_ceq0.pdf",
       width = 7, height = 5)

ggplot(all_rev_rkhs_ce[(all_rev_rkhs_ce$stats=="RMSE"),],aes(x=method, y = rev.q1))+ 
  geom_boxplot() +
  facet_wrap(vars(Group), ncol = 3,
             scales = "free_y") + AQLTools:::theme_aqltools()+
  labs(title="RMSE-CE q1")
ggsave("point_these/img/fred/rkhs_ceq1.pdf",
       width = 7, height = 5)
