library(AQLThesis)
library("future")
library(rjdfilters)
plan(multisession)
if(!dir.exists("data_simul/byseriespente_final"))
  dir.create("data_simul/byseriespente_final")
if(!dir.exists("data/byseriespente_final_nber"))
  dir.create("data/byseriespente_final_nber")
X_gen <- function(d = 1, p = 6, q = p){
  sapply(0:d, function(exp) seq(-p, q)^exp)
}

gen_MM <- function(p=6, q=p, d=2){
  k = rjdfilters::get_kernel("Henderson", h = p)
  k
  k = c(rev(k$coef[-1]), k$coef[seq(0,q)+1])
  k
  K = diag(k)
  X = X_gen(d=d, p = p, q = q)
  e1 = e2 = e3 = matrix(0, ncol = 1, nrow = d+1)
  e1[1] = 1
  e2[2] = 1
  e3[3] = 1
  M1 = K %*% X %*% solve(t(X) %*% K %*% X, e1)
  M2 = K %*% X %*% solve(t(X) %*% K %*% X, e2)
  M3 = K %*% X %*% solve(t(X) %*% K %*% X, e3)
  cbind(M1, M2, M3)
}


MM = lapply(3:6, function(h){
  list(`d=2` = gen_MM(p=h, d=2)[,2],
       `d=3` = gen_MM(p = h, d=3)[,2],
       `deriv2` = gen_MM(p = h, d=3)[,3],
       henderson = lp_filter(horizon = h)$filters.coef[,sprintf("q=%i",h)])
})
names(MM) <- sprintf("h=%i", 3:6)
s = list.files("data_simul/byseries",full.names = TRUE)[1]
d = 2
for(s in list.files("data_simul/byseries",full.names = TRUE)){
  print(s)
  for(h in 3:6){
    new_f = sprintf("data_simul/byseriespente_final/%s_h%i.RDS",
                    gsub(".RDS", "",basename(s)),h)
    print(new_f)
    MM_h = MM[[sprintf("h=%i", h)]]
    hend_filter = lp_filter(horizon = h)$filters.coef[,h+1]
    if(!file.exists(new_f)){
      data <- readRDS(s)
      last_est = data[[length(data)]]
      res2 = zoo::na.locf(jasym_filter(last_est, MM_h[[sprintf("d=%i",2)]],-h))
      res3 = zoo::na.locf(jasym_filter(last_est, MM_h[[sprintf("d=%i",3)]],-h))
      resderiv2 = zoo::na.locf(jasym_filter(last_est, MM_h[["deriv2"]],-h))
      
      info_fs <- lapply(data, function(x){
        future({
          sigma2 =  mean((x -jasym_filter(x, hend_filter, -h))^2,
                         na.rm = TRUE)
          sigma2 = sigma2/(1- 2*MM_h[["henderson"]]["t"] + sum(MM_h[["henderson"]]^2))
          names(sigma2) <- NULL
          list(`d=2` = tail(window(res2, end = end(x)), 6),
               `d=3` = tail(window(res3, end = end(x)), 6),
               `deriv2` = tail(window(resderiv2, end = end(x)), 6),
               `sigma2` = sigma2
          )
        }
        )
      })
      info <- lapply(info_fs, value)
      saveRDS(info, new_f)
    }
  }
}


for(s in list.files("data/byseries_nber",full.names = TRUE)){
  print(s)
  for(h in 3:6){
    new_f = sprintf("data/byseriespente_final_nber/%s_h%i.RDS", gsub(".RDS", "",basename(s)),h)
    print(new_f)
    MM_h = MM[[sprintf("h=%i", h)]]
    hend_filter = lp_filter(horizon = h)$filters.coef[,h+1]
    if(!file.exists(new_f)){
      data <- readRDS(s)
      last_est = data[[length(data)]]
      res2 = zoo::na.locf(jasym_filter(last_est, MM_h[[sprintf("d=%i",2)]],-h))
      res3 = zoo::na.locf(jasym_filter(last_est, MM_h[[sprintf("d=%i",3)]],-h))
      resderiv2 = zoo::na.locf(jasym_filter(last_est, MM_h[["deriv2"]],-h))
      
      info_fs <- lapply(data, function(x){
        future({
          sigma2 =  mean((x -jasym_filter(x, hend_filter, -h))^2,
                         na.rm = TRUE)
          sigma2 = sigma2/(1- 2*MM_h[["henderson"]]["t"] + sum(MM_h[["henderson"]]^2))
          names(sigma2) <- NULL
          list(`d=2` = tail(window(res2, end = end(x)), 6),
               `d=3` = tail(window(res3, end = end(x)), 6),
               `deriv2` = tail(window(resderiv2, end = end(x)), 6),
               `sigma2` = sigma2
          )
        }
        )
      })
      info <- lapply(info_fs, value)
      saveRDS(info, new_f)
    }
  }
}