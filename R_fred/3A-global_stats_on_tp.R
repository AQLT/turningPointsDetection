library(AQLThesis)
library(zoo)
library(ggplot2)

series_with_tp <- readRDS("results/series_to_study.RDS")

all_files <- list.files("results/lp/",pattern = "_tp",full.names = TRUE)
all_files <- grep(paste(sprintf("(%s)",series_with_tp), collapse = "|"),
                  all_files, value = TRUE)
all_files <- grep("henderson",
                  all_files, value = TRUE)
all_files <- grep("lc",
                  all_files, value = TRUE)

dephasage = function(data,
         peaks = nber_tp_m[,"Peak"],
         troughs = nber_tp_m[,"Trough"],
         frequency = 12){
  peaks <- na.omit(peaks)
  troughs <- na.omit(troughs)
  peaks <- peaks[peaks>=as.numeric(names(data)[1])]
  troughs <- troughs[troughs>=as.numeric(names(data)[1])]
  
  last_tp <- lapply(data[[length(data)]], function(x) x[x>=as.numeric(names(data)[1])])
  last_tp <- do.call(c, last_tp)
  
  peaks_timelag <- sapply(peaks, function(x){
    deph = last_tp - x
    round(deph[which.min(abs(deph))]*frequency)
  })
  names(peaks_timelag) <- peaks
  
  
  troughs_timelag <- sapply(troughs, function(x){
    deph = last_tp - x
    round(deph[which.min(abs(deph))]*frequency)
  })
  names(troughs_timelag) <- troughs
  if(length(peaks_timelag) ==0 | length(troughs_timelag) ==0)
    return(NULL)
  peaks_timelag[abs(peaks_timelag) >= 1.5*frequency] <- NA
  troughs_timelag[abs(troughs_timelag) >= 1.5*frequency] <- NA
  rbind(data.frame(tp = peaks, time_lag = peaks_timelag,type = "peaks"),
        data.frame(tp = troughs, time_lag = troughs_timelag,type = "troughs"))
}


all_tp_lp <- do.call(rbind, lapply(seq_along(all_files), function(i){
  # print(i)
  f = all_files[i]
  res = dephasage(readRDS(f))
  if(is.null(res))
    return(res)
  res$series = strsplit(basename(f),"_")[[1]][[1]]
  res
}))
info_base = AQLThesis::fred_md_description
info_base$fred = gsub("&|:| ",".", info_base$fred)
info_base = info_base[,c("fred", "description","Group","Group_number")]
info_base$Group = gsub("and", "and\n", info_base$Group)
data_group = unique(info_base[,c("Group","Group_number")])
data_group = data_group[order(data_group$Group_number,decreasing = FALSE),]
info_base$Group = factor(info_base$Group,
                         levels = data_group$Group,
                         ordered = TRUE)
all_tp2 = merge(all_tp_lp, info_base, by.x = "series", by.y = "fred", all.x = TRUE, all.y = FALSE)

all_tp2$tp_label <- factor(as.character(zoo::as.yearmon(all_tp2$tp)),
                           levels=as.character(zoo::as.yearmon((sort(unique(all_tp2$tp))))), 
                           ordered = TRUE)
all_tp2 <- na.omit(all_tp2)

p <- ggplot(all_tp2, aes(x=time_lag, y=Group_number, color = Group)) + 
  geom_point() + 
  facet_wrap(vars(type, tp_label)) +
  geom_vline(xintercept = 0) +
  theme(axis.title.y = element_blank())
p

ggsave("point_these/img/fred_global_stats.pdf",plot = p,
       width = 7, height = 4)





