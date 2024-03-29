library(plot3D)
source("R_fred/4-utils.R",encoding = "UTF-8")

all_tp <- merge(readRDS("results_nber/compile_tp/troughs_fst.RDS"),
                readRDS("results_nber/compile_tp/peaks_fst.RDS"),
                by=c("series","degree", "weight"))
all_tp <- select_var(all_tp)

all_rev_fe <- select_series(readRDS("results_nber/compile_revisions/fst_fe_rev.RDS"))
all_rev_ce <- select_series(readRDS("results_nber/compile_revisions/fst_ce_rev.RDS"))

all_pond <- do.call(rbind,
        lapply(0:3, function(degree){
  data = readRDS(sprintf("filters/fst_pdegree%i.RDS", degree))$weights
  data$degree = degree
  data$weight = sprintf("weight%i", seq_len(nrow(data)))
  data
}))

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
  mean(x,na.rm = TRUE)
}
cut_big <- function(x, cutoff = 10^5){
  x[which(x > 10^4)] <- NA
  x
}
data_rev_fe <- all_rev_fe %>% 
  mutate(across(rev.q0:rev.q10, cut_big))%>% 
  group_by(stats, Group, weight, degree, length) %>% 
  summarise_at(vars(rev.q0:rev.q10), mean_100)%>% 
  left_join(all_pond) %>% data.frame

data_rev_ce <- all_rev_ce %>% 
  mutate(across(rev.q0:rev.q10, cut_big)) %>% 
  group_by(stats, weight, degree, length) %>% 
  summarise_at(vars(rev.q0:rev.q10), mean_100)%>% 
  left_join(all_pond) %>% data.frame


plot_data <- data_rev_fe
par(mfrow=c(1,1))
plot_fst_tp(global_data, degree = 0)
plot_fst_rev(data_rev_fe, degree = 0,color = "rev.q0",limits = F)
plot_fst_tp(global_data, degree = 1)
plot_fst_rev(data_rev_fe, degree = 1,color = "rev.q0")
plot_fst_tp(global_data, degree = 2)
plot_fst_rev(data_rev_fe, degree = 2,color = "rev.q0")
plot_fst_tp(global_data, degree = 3)
plot_fst_rev(data_rev_fe, degree = 3,color = "rev.q0")

pdf(file = sprintf("point_these/img/fred/fst_limits.pdf"),width = 7,height = 5)
par(mfrow = c(2,2), 
    mar = 0 + c(1, 0, 1, 0),
    mai = c(0.2, 0.2, 0.2, 0.2))
plot_fst_tp(global_data, degree = 0, title = "degree = 0")
plot_fst2(global_data, degree = 0)
plot_fst_tp(global_data, degree = 1, title = "degree = 1")
plot_fst_tp(global_data, degree = 2, title = "degree = 2")
plot_fst_tp(global_data, degree = 3, title = "degree = 3")
dev.off()

pdf(file = sprintf("point_these/img/fred/fst_nolimits.pdf"),width = 7,height = 5)
par(mfrow = c(2,2), 
    mar = 0 + c(1, 0, 1, 0),
    mai = c(0.2, 0.2, 0.2, 0.2))
plot_fst_tp(global_data, degree = 0,limits = FALSE, title = "degree = 0")
plot_fst_tp(global_data, degree = 1,limits = FALSE, title = "degree = 1")
plot_fst_tp(global_data, degree = 2,limits = FALSE, title = "degree = 2")
plot_fst_tp(global_data, degree = 3,limits = FALSE, title = "degree = 3")
dev.off()


pdf(file = sprintf("point_these/img/simul/fst_limits_feq0.pdf"),width = 7,height = 5)
par(mfrow = c(2,2), 
    mar = 0 + c(1, 0, 1, 0),
    mai = c(0.2, 0.2, 0.2, 0.2))
for(i in 0:3){
  plot_fst_rev(data_rev_fe, degree = i, title = sprintf("degree %s", i),
               color = "rev.q0",limits = TRUE)
}
dev.off()
pdf(file = sprintf("point_these/img/simul/fst_nolimits_feq0.pdf"),width = 7,height = 5)
par(mfrow = c(2,2), 
    mar = 0 + c(1, 0, 1, 0),
    mai = c(0.2, 0.2, 0.2, 0.2))
for(i in 0:3){
  plot_fst_rev(data_rev_fe, degree = i, title = sprintf("degree %s", i),
               color = "rev.q0",limits = FALSE)
}
dev.off()

pdf(file = sprintf("point_these/img/simul/fst_limits_feq1.pdf"),width = 7,height = 5)
par(mfrow = c(2,2), 
    mar = 0 + c(1, 0, 1, 0),
    mai = c(0.2, 0.2, 0.2, 0.2))
for(i in 0:3){
  plot_fst_rev(data_rev_fe, degree = i, title = sprintf("degree %s", i),
               color = "rev.q1",limits = TRUE)
}
dev.off()

pdf(file = sprintf("point_these/img/simul/fst_nolimits_feq1.pdf"),width = 7,height = 5)
par(mfrow = c(2,2), 
    mar = 0 + c(1, 0, 1, 0),
    mai = c(0.2, 0.2, 0.2, 0.2))
for(i in 0:3){
  plot_fst_rev(data_rev_fe, degree = i, title = sprintf("degree %s", i),
               color = "rev.q1",limits = FALSE)
}
dev.off()

###
pdf(file = sprintf("point_these/img/simul/fst_limits_ceq0.pdf"),width = 7,height = 5)
par(mfrow = c(2,2), 
    mar = 0 + c(1, 0, 1, 0),
    mai = c(0.2, 0.2, 0.2, 0.2))
for(i in 0:3){
  plot_fst_rev(data_rev_ce, degree = i, title = sprintf("degree %s", i),
               color = "rev.q0",limits = TRUE)
}
dev.off()

pdf(file = sprintf("point_these/img/simul/fst_nolimits_ceq0.pdf"),width = 7,height = 5)
par(mfrow = c(2,2), 
    mar = 0 + c(1, 0, 1, 0),
    mai = c(0.2, 0.2, 0.2, 0.2))
for(i in 0:3){
  plot_fst_rev(data_rev_ce, degree = i, title = sprintf("degree %s", i),
               color = "rev.q0",limits = FALSE)
}
dev.off()

pdf(file = sprintf("point_these/img/simul/fst_limits_ceq1.pdf"),width = 7,height = 5)
par(mfrow = c(2,2), 
    mar = 0 + c(1, 0, 1, 0),
    mai = c(0.2, 0.2, 0.2, 0.2))
for(i in 0:3){
  plot_fst_rev(data_rev_ce, degree = i, title = sprintf("degree %s", i),
               color = "rev.q1",limits = TRUE)
}
dev.off()

pdf(file = sprintf("point_these/img/simul/fst_nolimits_ceq1.pdf"),width = 7,height = 5)
par(mfrow = c(2,2), 
    mar = 0 + c(1, 0, 1, 0),
    mai = c(0.2, 0.2, 0.2, 0.2))
for(i in 0:3){
  plot_fst_rev(data_rev_ce, degree = i, title = sprintf("degree %s", i),
               color = "rev.q1",limits = FALSE)
}
dev.off()

library(plotly)
plot_fst2 <- function(plot_data, degree = 0, length = 13, color = "moy"){

  title = sprintf("color variable = %s, degree = %i, length = %i",
                  color, degree, length)
  plot_data <- plot_data[plot_data$length %in% length,]
  limits = range(plot_data[,color])

  plot_data <- plot_data[plot_data$degree %in% degree,]

  text_legend = sprintf(paste("</br> (s, f, t) = (%.2f ; %.2f ; %.2f)",
                              "min = %.2f ; med = %f ;",
                              "moy = %.2f ; max = %.2f",
                              sep = "</br>"),
                        plot_data$smoothness.weight,
                        plot_data$fidelity.weight,
                        plot_data$timeliness.weight,
                        plot_data$min, plot_data$med,
                        plot_data$moy, plot_data$max)
  plot_ly(x=plot_data[,"smoothness.weight"],
          y=plot_data[,"fidelity.weight"],
          z=plot_data[,"timeliness.weight"],
          type="scatter3d", mode="markers",
          hoverinfo = 'text',
          text = text_legend,
          marker = list(color=plot_data[,color],
                        showscale=TRUE,
                        cmin=limits[1],
                        cmax=limits[2])
  ) %>%
    layout(
      title = title,
      scene = list(
        xaxis = list(title = "Smoothness"),
        yaxis = list(title = "Fidelity"),
        zaxis = list(title = "Timeliness")
      ))
}
