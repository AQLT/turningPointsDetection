library(dplyr)

length_info <- 
  sapply(list.files(path = "data_simul/byseriesinfo",full.names = TRUE), 
         function(f){
           info <- readRDS(f)
           13
         })
length_info <- data.frame(series = gsub(".RDS", "", basename(names(length_info))),
                          length = as.numeric(length_info))

select_var <- function(x){
  x = select_series(x)
  x
}
select_series <- function(x){
  x = merge(x, length_info, all.x = TRUE, all.y = FALSE, by = "series")
  x$variability <- gsub("\\d","", x$series)
  x
}
nb_obs <- function(x){
  sum(!is.na(x))
}

plot_fst_tp <- function(plot_data, degree = 0, length = 13, color = "moy",
                        limits = TRUE, title = NULL){
  if(missing(title))
    title = sprintf("color variable = %s, degree = %i, length = %i",
                    color, degree, length)
  plot_data <- plot_data[plot_data$length %in% length,]
  limits_ = range(plot_data[,color])
  
  plot_data <- plot_data[plot_data$degree %in% degree,
                         # c("fidelity.weight","smoothness.weight", "timeliness.weight",color)
                         ]
  
  if(limits){
    plot_data <- rbind(plot_data, 
                       c(-1, -1, -1, limits_[1]),
                       c(-1, -1, -1, limits_[2]))
  }
  with(plot_data, 
       scatter3D(x = fidelity.weight,
                 y = smoothness.weight,
                 z = timeliness.weight,
                 colvar = plot_data[,color], 
                 phi = 40, theta = 120,
                 # bty = "g",
                 pch = 1,
                 # cex = 0.1, alpha = 0.4,
                 ticktype = "detailed",
                 xlim = c(0,1),
                 ylim = c(0,1),
                 zlim = c(0,1), 
                 xlab = "\n\nFidelity",
                 ylab ="\n\nSmoothness",
                 zlab = "\n\nTimeliness",
                 main = title))
}

plot_fst_rev <- function(plot_data, degree = 0, length = 13, color = "rev.q0",
                         limits = TRUE, group = "total", stats = "RMSE",
                         title = NULL){
  if(missing(title))
    title = sprintf("color variable = %s of %s, degree = %i, length = %i",
                    stats,
                    color, degree, length)
  plot_data <- plot_data[(plot_data$length %in% length) &
                           (plot_data$Group == group) &
                           (plot_data$stats == stats),]
  limits_ = range(plot_data[,color])
  
  plot_data <- plot_data[plot_data$degree %in% degree,
                         # c("fidelity.weight","smoothness.weight", "timeliness.weight",color)
                         ]
  
  if(limits){
    plot_data <- rbind(plot_data, 
                       c(-1, -1, -1, limits_[1]),
                       c(-1, -1, -1, limits_[2]))
  }
  with(plot_data, 
       scatter3D(x = fidelity.weight,
                 y = smoothness.weight,
                 z = timeliness.weight,
                 colvar = plot_data[,color], 
                 phi = 40, theta = 120,
                 # bty = "g",
                 pch = 1,
                 # cex = 0.1, alpha = 0.4,
                 ticktype = "detailed",
                 xlim = c(0,1),
                 ylim = c(0,1),
                 zlim = c(0,1), 
                 xlab = "\n\nFidelity",
                 ylab ="\n\nSmoothness",
                 zlab = "\n\nTimeliness",
                 main = title))
}


plot_dfa_tp <- function(plot_data, degree = 0, length = 13, color = "moy",
                        limits = TRUE, title = NULL){
  if(missing(title))
    title = sprintf("color variable = %s, degree = %i, length = %i",
                    color, degree, length)
  plot_data <- plot_data[plot_data$length %in% length,]
  limits_ = range(plot_data[,color])
  
  plot_data <- plot_data[plot_data$degree %in% degree,
                         c("accuracy.weight","smoothness.weight", "timeliness.weight",color)]
  
  if(limits){
    plot_data <- rbind(plot_data, 
                       c(-1, -1, -1, limits_[1]),
                       c(-1, -1, -1, limits_[2]))
  }
  with(plot_data, 
       scatter3D(x = accuracy.weight,
                 y = smoothness.weight,
                 z = timeliness.weight,
                 colvar = plot_data[,color], 
                 phi = 40, theta = 120,
                 # bty = "g",
                 pch = 1,
                 # cex = 0.1, alpha = 0.4,
                 ticktype = "detailed",
                 xlim = c(0,1),
                 ylim = c(0,1),
                 zlim = c(0,1), 
                 xlab = "\n\nAccuracy",
                 ylab ="\n\nSmoothness",
                 zlab = "\n\nTimeliness",
                 main = title))
}

plot_dfa_rev <- function(plot_data, degree = 0, length = 13, color = "rev.q0",
                         limits = TRUE, group = "total", stats = "RMSE",
                         title = NULL){
  if(missing(title))
    title = sprintf("color variable = %s of %s, degree = %i, length = %i",
                    stats,
                    color, degree, length)
  plot_data <- plot_data[(plot_data$length %in% length) &
                           (plot_data$Group == group) &
                           (plot_data$stats == stats),]
  limits_ = range(plot_data[,color])
  
  plot_data <- plot_data[plot_data$degree %in% degree,
                         c("accuracy.weight","smoothness.weight", "timeliness.weight",color)]
  
  if(limits){
    plot_data <- rbind(plot_data, 
                       c(-1, -1, -1, limits_[1]),
                       c(-1, -1, -1, limits_[2]))
  }
  with(plot_data, 
       scatter3D(x = accuracy.weight,
                 y = smoothness.weight,
                 z = timeliness.weight,
                 colvar = plot_data[,color], 
                 phi = 40, theta = 120,
                 # bty = "g",
                 pch = 1,
                 # cex = 0.1, alpha = 0.4,
                 ticktype = "detailed",
                 xlim = c(0,1),
                 ylim = c(0,1),
                 zlim = c(0,1), 
                 xlab = "\n\nAccuracy",
                 ylab ="\n\nSmoothness",
                 zlab = "\n\nTimeliness",
                 main = title))
}