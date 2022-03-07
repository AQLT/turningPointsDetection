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
select_mae <- function(x){
  x %>% 
    filter(Group == "total",
           stats == "MAE") %>% 
    select(!c(Group, stats))
}

plot_fst_tp <- function(plot_data, degree = 0, length = 13, color = "moy",
                        limits = TRUE, title = NULL,
                        by_ = 0.1){
  if(missing(title))
    title = sprintf("color variable = %s, degree = %i, length = %i",
                    color, degree, length)
  plot_data <- plot_data[plot_data$length %in% length,]
  # plot_data <- plot_data[plot_data$timeliness.weight %in% seq(0,1,length.out = 21) &
  #                          plot_data$smoothness.weight %in% seq(0,1,length.out = 21)&
  #                          plot_data$fidelity.weight %in% seq(0,1,length.out = 21),]
  if(limits){
    all_values <- plot_data[,color]
  }else{
    all_values <- plot_data[plot_data$degree %in% degree,color]
  }
  limits_ = range(all_values)
  
  plot_data <- plot_data[plot_data$degree %in% degree,
                         c("fidelity.weight","smoothness.weight", "timeliness.weight",color)
                         ]

  
  breaks_lim <- (limits_ %/% by_)*by_
  breaks = seq(from = breaks_lim[1],
               to = breaks_lim[2]+by_, by = by_)
  breaks_up <- breaks[1:2]
  for(i in 3:length(breaks)){
    if(!any(all_values >= breaks_up[length(breaks_up)] & all_values <= breaks[i])){
      breaks_up[length(breaks_up)] <- breaks[i]
    }else{
      breaks_up <- c(breaks_up, breaks[i])
    }
  }
  col_inf_1_fun <- colorRampPalette(c("#2B00FF","#B3FFFF"))
  col_sup_1_fun <- colorRampPalette(c(#"#FFFFCC",
                                      "#FFCCA7",
                                      "#FF4D4D", "#660000"))
  col_inf <- col_inf_1_fun(sum(breaks_up<1))
  col_sup <-  col_sup_1_fun(length(breaks_up) - length(col_inf) - 1)
  
  colkeydef <- list(side = 4, 
                    plot = TRUE, # changer,
                    length = 1,
                    width = 1,
                    dist = -0.15, shift = 0,
                    addlines = FALSE, 
                    col.clab = NULL, 
                    cex.clab = par("cex.lab"), 
                    side.clab = NULL, 
                    line.clab = NULL, 
                    adj.clab = NULL, 
                    font.clab = NULL)
  with(plot_data, 
       scatter3D(x = fidelity.weight,
                 y = smoothness.weight,
                 z = timeliness.weight,
                 colvar = plot_data[,color],
                 colkey = colkeydef,
                 breaks = breaks_up,
                 col = c(col_inf, col_sup),
                 # clim = limits_,
                 phi = 40, theta = 120,
                 # bty = "g",
                 pch = 16,
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
                         limits = TRUE,
                         title = NULL,
                         by_ = 2){
  # plot_data = data_rev_fe; degree = 1; title = sprintf("degree %s", 1);
  #  color = "rev.q0";limits = TRUE; length = 13
  if(missing(title))
    title = sprintf("color variable = %s of %s, degree = %i, length = %i",
                    stats,
                    color, degree, length)
  plot_data <- plot_data[(plot_data$length %in% length),]
  
  if(limits){
    all_values <- plot_data[,color]
  }else{
    all_values <- plot_data[plot_data$degree %in% degree,color]
  }
  limits_ = range(all_values)
  plot_data <- plot_data[plot_data$degree %in% degree,
                          c("fidelity.weight","smoothness.weight", "timeliness.weight",color)
                         ]
  
  breaks_lim <- (limits_ %/% by_)*by_
  if(limits_[1] > 1  & breaks_lim[1] <=1)
    breaks_lim[1] <- 1
  # if(limits_[1] < 1  & breaks_lim[1] <=1)
  #   breaks_lim[1] <- 1
  breaks = unique(c(seq(from = breaks_lim[1],
               to = breaks_lim[2]+by_, by = by_),
             breaks_lim[2]+by_))
  if((! 1%in% breaks) & breaks[1]<1){
    breaks = c(breaks[breaks<1], 1, breaks[breaks>1])
  }
  if(breaks[1] == 0){
    breaks = c(round(limits_[1],2) - 0.01, breaks[breaks>limits_[1]])
  }
  breaks_up <- breaks[1:2]
  for(i in 3:length(breaks)){
    if(!any(all_values >= breaks_up[length(breaks_up)] & all_values <= breaks[i])){
      breaks_up[length(breaks_up)] <- breaks[i]
    }else{
      breaks_up <- c(breaks_up, breaks[i])
    }
  }
  col_inf_1_fun <- colorRampPalette(c("#2B00FF","#B3FFFF"))
  # col_sup_1_fun <- colorRampPalette(c("#FF99FF", "#FF4D4D", "#660000"))
  col_sup_1_fun <- colorRampPalette(c(#"#FFFFCC",
    "#FFCCA7",
    "#FF4D4D", "#660000"))
  col_inf <- col_inf_1_fun(sum(breaks_up<1))
  col_sup <-  col_sup_1_fun(length(breaks_up) - length(col_inf) - 1)
  
  colkeydef <- list(side = 4, 
                    plot = TRUE, # changer,
                    length = 1,
                    width = 1,
                    dist = -0.15, shift = 0,
                    addlines = FALSE, 
                    col.clab = NULL, 
                    cex.clab = par("cex.lab"), 
                    side.clab = NULL, 
                    line.clab = NULL, 
                    adj.clab = NULL, 
                    font.clab = NULL)
  with(plot_data, 
       scatter3D(x = fidelity.weight,
                 y = smoothness.weight,
                 z = timeliness.weight,
                 colvar = plot_data[,color],
                 colkey = colkeydef,
                 breaks = breaks_up,
                 col = c(col_inf, col_sup),
                 clim = limits_,
                 phi = 40, theta = 120,
                 # bty = "g",
                 pch = 16,
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


unique_series_pivot <- function(x){
  to_remove = x %>% group_by(series, name) %>%
    mutate(remove = any(is.na(value))) %>% 
    data.frame
  x[to_remove$remove,"value"] <- NA
  x
}

unique_series <- function(x){
  to_remove = x %>% group_by(series) %>%
    mutate_at(grep("^X", colnames(x), value = TRUE), \(x) any(is.na(x))) %>% 
    data.frame
  x[,grep("^X", colnames(x))][as.matrix(to_remove[,grep("^X", colnames(to_remove))])] <- NA
  x
}
