library(plot3D)
source("R_simul/4-utils.R",encoding = "UTF-8")

all_tp <- merge(readRDS("results_simul/compile_tp/troughs_dfa.RDS"),
                readRDS("results_simul/compile_tp/peaks_dfa.RDS"),
                by=c("series","degree", "weight"))
all_tp2 <- all_tp <- select_var(all_tp)

all_rev_fe2 <- all_rev_fe <- select_series(readRDS("results_simul/compile_revisions/dfa_fe_rev.RDS"))
all_rev_ce2 <- all_rev_ce <- select_series(readRDS("results_simul/compile_revisions/dfa_ce_rev.RDS"))

all_pond <- do.call(rbind,
                    lapply(0:3, function(degree){
                      data = readRDS(sprintf("filters/dfa_pdegree%i.RDS", degree))$weights
                      data$degree = degree
                      data$weight = sprintf("weight%i", seq_len(nrow(data)))
                      data
                    }))



for(variability in c("lowvariability","highvariability")){
  all_tp = all_tp2[all_tp2$variability == variability,]
  all_rev_fe = all_rev_fe2[all_rev_fe2$variability == variability,]
  all_rev_ce = all_rev_ce2[all_rev_ce2$variability == variability,]

global_data <- all_tp %>% 
  tidyr::pivot_longer(
    cols = starts_with("x"), 
    names_to = "name",
    values_to = "value"
  ) %>% 
  group_by(weight, degree, length) %>% 
  summarise(
    moy = mean(value,na.rm = TRUE),
    med = median(value, na.rm = TRUE),
    min = mean(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    nb_obs = nb_obs(value)
  )  %>% 
  na.omit %>% 
  arrange(length, weight, degree) %>% 
  left_join(all_pond) %>% data.frame

mean_100 <- function(x){
  (mean(x,na.rm = TRUE))
}
data_rev_fe <- all_rev_fe %>% 
  group_by(stats, Group, weight, degree, length) %>% 
  summarise_at(vars(rev.q0:rev.q10), median)%>% 
  left_join(all_pond) %>% data.frame

data_rev_ce <- all_rev_ce %>% 
  group_by(stats, Group, weight, degree, length) %>% 
  summarise_at(vars(rev.q0:rev.q10), median)%>% 
  left_join(all_pond) %>% data.frame

# 
# plot_data <- data_rev_fe
# par(mfrow=c(1,2))
# plot_dfa_tp(global_data, degree = 0)
# plot_dfa_rev(data_rev_fe, degree = 0,color = "rev.q0")
# plot_dfa_tp(global_data, degree = 1)
# plot_dfa_rev(data_rev_fe, degree = 1,color = "rev.q0")
# plot_dfa_tp(global_data, degree = 2)
# plot_dfa_rev(data_rev_fe, degree = 2,color = "rev.q0")
# plot_dfa_tp(global_data, degree = 3)
# plot_dfa_rev(data_rev_fe, degree = 3,color = "rev.q0")

pdf(file = sprintf("point_these/img/simul/dfa_limits_%s.pdf", variability),width = 7,height = 5)
par(mfrow = c(2,2), 
    mar = 0 + c(1, 0, 1, 0),
    mai = c(0.2, 0.2, 0.2, 0.2))
for(i in 0:3){
  plot_dfa_tp(global_data, degree = i, title = sprintf("degree %s", i),
              color = "med")
}
dev.off()

pdf(file = sprintf("point_these/img/simul/dfa_nolimits_%s.pdf", variability),width = 7,height = 5)
par(mfrow = c(2,2), 
    mar = 0 + c(1, 0, 1, 0),
    mai = c(0.2, 0.2, 0.2, 0.2))
for(i in 0:3){
  plot_dfa_tp(global_data, degree = i, title = sprintf("degree %s", i),
              color = "med",limits = FALSE)
}
dev.off()

pdf(file = sprintf("point_these/img/simul/dfa_limits_feq0_%s.pdf", variability),width = 7,height = 5)
par(mfrow = c(2,2), 
    mar = 0 + c(1, 0, 1, 0),
    mai = c(0.2, 0.2, 0.2, 0.2))
for(i in 0:3){
  plot_dfa_rev(data_rev_fe, degree = i, title = sprintf("degree %s", i),
               color = "rev.q0",limits = TRUE)
}
dev.off()

pdf(file = sprintf("point_these/img/simul/dfa_limits_feq1_%s.pdf", variability),width = 7,height = 5)
par(mfrow = c(2,2), 
    mar = 0 + c(1, 0, 1, 0),
    mai = c(0.2, 0.2, 0.2, 0.2))
for(i in 0:3){
  plot_dfa_rev(data_rev_fe, degree = i, title = sprintf("degree %s", i),
               color = "rev.q1",limits = TRUE)
}
dev.off()

pdf(file = sprintf("point_these/img/simul/dfa_nolimits_feq0_%s.pdf", variability),width = 7,height = 5)
par(mfrow = c(2,2), 
    mar = 0 + c(1, 0, 1, 0),
    mai = c(0.2, 0.2, 0.2, 0.2))
for(i in 0:3){
  plot_dfa_rev(data_rev_fe, degree = i, title = sprintf("degree %s", i),
               color = "rev.q0",limits = FALSE)
}
dev.off()

pdf(file = sprintf("point_these/img/simul/dfa_nolimits_feq1_%s.pdf", variability),width = 7,height = 5)
par(mfrow = c(2,2), 
    mar = 0 + c(1, 0, 1, 0),
    mai = c(0.2, 0.2, 0.2, 0.2))
for(i in 0:3){
  plot_dfa_rev(data_rev_fe, degree = i, title = sprintf("degree %s", i),
               color = "rev.q1",limits = FALSE)
}
dev.off()

###
pdf(file = sprintf("point_these/img/simul/dfa_limits_ceq0_%s.pdf", variability),width = 7,height = 5)
par(mfrow = c(2,2), 
    mar = 0 + c(1, 0, 1, 0),
    mai = c(0.2, 0.2, 0.2, 0.2))
for(i in 0:3){
  plot_dfa_rev(data_rev_ce, degree = i, title = sprintf("degree %s", i),
               color = "rev.q0",limits = TRUE)
}
dev.off()

pdf(file = sprintf("point_these/img/simul/dfa_nolimits_ceq0_%s.pdf", variability),width = 7,height = 5)
par(mfrow = c(2,2), 
    mar = 0 + c(1, 0, 1, 0),
    mai = c(0.2, 0.2, 0.2, 0.2))
for(i in 0:3){
  plot_dfa_rev(data_rev_ce, degree = i, title = sprintf("degree %s", i),
               color = "rev.q0",limits = FALSE)
}
dev.off()

pdf(file = sprintf("point_these/img/simul/dfa_limits_ceq1_%s.pdf", variability),width = 7,height = 5)
par(mfrow = c(2,2), 
    mar = 0 + c(1, 0, 1, 0),
    mai = c(0.2, 0.2, 0.2, 0.2))
for(i in 0:3){
  plot_dfa_rev(data_rev_ce, degree = i, title = sprintf("degree %s", i),
               color = "rev.q1",limits = TRUE)
}
dev.off()

pdf(file = sprintf("point_these/img/simul/dfa_nolimits_ceq1_%s.pdf", variability),width = 7,height = 5)
par(mfrow = c(2,2), 
    mar = 0 + c(1, 0, 1, 0),
    mai = c(0.2, 0.2, 0.2, 0.2))
for(i in 0:3){
  plot_dfa_rev(data_rev_ce, degree = i, title = sprintf("degree %s", i),
               color = "rev.q1",limits = FALSE)
}
dev.off()
}

all_f <- file.info(list.files("point_these/img/simul",pattern = "fst",full.names = TRUE))
all_f <- all_f[order(all_f$mtime),]
all_f <- rownames(all_f)
nom_f <- rep("dephasage",length(all_f))
nom_f[grep("feq0", all_f)] <- "RMSE - FE q0"
nom_f[grep("feq1", all_f)] <- "RMSE - FE q1"
nom_f[grep("ceq0", all_f)] <- "RMSE - CE q0"
nom_f[grep("ceq1", all_f)] <- "RMSE - CE q1"
type = rep("low variability", length(all_f))
type[grep("high", all_f)] = "high variability"
nom_f = paste(nom_f, type)
cat(sprintf('### Résultats FST %s\n\n```{r,out.width="100%s"}\nknitr::include_graphics("%s")\n```\n',
            nom_f,
            "%",
            gsub("point_these/","", all_f)),sep = "\n")

file.remove(list.files("point_these/img/simul",pattern = "fst",full.names = TRUE))
file.remove(list.files("point_these/img/simul",pattern = "dfa",full.names = TRUE))



all_f <- file.info(list.files("point_these/img/simul",pattern = "fst",full.names = TRUE))
all_f <- all_f[order(all_f$mtime),]
all_f <- rownames(all_f)
nom_f <- rep("dephasage",length(all_f))
nom_f[grep("feq0", all_f)] <- "RMSE - FE q0"
nom_f[grep("feq1", all_f)] <- "RMSE - FE q1"
nom_f[grep("ceq0", all_f)] <- "RMSE - CE q0"
nom_f[grep("ceq1", all_f)] <- "RMSE - CE q1"
type = rep("low variability", length(all_f))
type[grep("high", all_f)] = "high variability"
nom_f = paste(nom_f, type)
cat(sprintf('### Résultats FST %s\n\n```{r,out.width="100%s"}\nknitr::include_graphics("%s")\n```\n',
            nom_f,
            "%",
            gsub("point_these/","", all_f)),sep = "\n")



all_f <- file.info(list.files("point_these/img/simul",pattern = "dfa",full.names = TRUE))
all_f <- all_f[order(all_f$mtime),]
all_f <- rownames(all_f)
nom_f <- rep("dephasage",length(all_f))
nom_f[grep("feq0", all_f)] <- "RMSE - FE q0"
nom_f[grep("feq1", all_f)] <- "RMSE - FE q1"
nom_f[grep("ceq0", all_f)] <- "RMSE - CE q0"
nom_f[grep("ceq1", all_f)] <- "RMSE - CE q1"
type = rep("low variability", length(all_f))
type[grep("high", all_f)] = "high variability"
nom_f = paste(nom_f, type)
cat(sprintf('### Résultats DFA %s\n\n```{r,out.width="100%s"}\nknitr::include_graphics("%s")\n```\n',
            nom_f,
            "%",
            gsub("point_these/","", all_f)),sep = "\n")
