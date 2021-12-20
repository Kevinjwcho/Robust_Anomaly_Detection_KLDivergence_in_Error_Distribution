# setwd("C:/Users/kevin/OneDrive/Desktop/190423-SWaT-MDN")
library(data.table)

err_files <- list.files(path = "C:/Users/kevin/OneDrive/Desktop/우사이먼교수님_프로젝트/seq2seq", full.names = TRUE, recursive = TRUE)
err_list <- lapply(err_files, fread)
# flag <- read.csv("truelabels.csv")

for(i in 1:5){
  assign(paste0("p", i), as.data.frame(err_list[[i]]))
}

# p1 <- p3
data <- p1
nr <- nrow(data)
nc <- ncol(data)
name <- colnames(data)


##########################
### Normalization --------
##########################

### min max ------------
p1_mm <- cbind(data[,c(1,2)], matrix(nrow = (nr), ncol = (nc-2)))


for(i in 3:(nc)){
  ma <- max(data[,i])
  mi <- min(data[,i])
  p1_mm[,i] <- (data[,i]-ma)/(ma-mi) 
}
colnames(p1_mm) <- colnames(data)


distance_mm <- numeric(nr)

for(i in 3:(nc-1)){
  sub_dist1 <- numeric(nr)
  for(j in i:(nc-1)){
    sub_dist2 <- abs(p1_mm[,j]-p1_mm[,(j+1)])
    # sub_dist1 <- sub_dist1+log(sub_dist2)
    sub_dist1 <- sub_dist1+(sub_dist2)
  }
  distance_mm <- distance_mm+sub_dist1
}



#############################
### Smoothing ---------------
#############################

library(zoo)
d_mm_ma90 <- rollmean(distance_mm, k = 90, na.pad = T)

### Differencing -------------
d_mm_ma90_dif <- diff(d_mm_ma90, lag = 3600)


d_mm_ma90_dif <- d_mm_ma90_dif[!is.na(d_mm_ma90_dif)]

n <- length(d_mm_ma90_dif)
win_size <- 360
jump_size <- 10
row_size <- round((n-win_size)/jump_size)

window_data <- matrix(nrow = row_size, ncol = win_size)
for(i in 1 : round((n-win_size)/jump_size)){
  if(i==1) j <- 1
  cut <- d_mm_ma90_dif[j:(j+win_size-1)]
  window_data[i, ] <- cut
  j <- jump_size+j
  if(j>n) break;
}

dim(window_data)
# n_tr <- 100
# start_te <- 40000
# window_data_tr <- window_data[1:n_tr,]
# window_data_te <- window_data[-c(1:start_te),]
# n_te <- nrow(window_data_te)
# 
Kulback <- function(den1, den2){
  x2<- den2$x
  y2 <- den2$y
  x1 <- den1$x
  y1 <- den1$y

  KL <- 0
  for(i in 1:(length(x2)-1)){
    candi <- which(x2[i]<x1)
    if(length(candi) == 0) sub_sum <- 0
    else{
      qx <- y1[min(candi)]*abs(x2[i+1]-x2[i])
      px <- y2[i]*abs(x2[i+1]-x2[i])
      if(px == 0 || qx == 0) sub_sum <- 0
      else{
        sub_sum <- px*log(px/qx)
        KL <- KL+sub_sum
      }
      # if(is.nan(KL)) break;
      # if(is.infinite(KL)) break;
    }
  }

  return(KL)
}


### Simple algorithm -----------
# N <- nrow(window_data)
# KL <- numeric(N-1)
# for(j in 1: (N-1)){
#     den1 <- density(window_data[j,])
#     den2 <- density(window_data[(j+1),])
#     KL[j] <- Kulback(den1, den2)
# }



# ### Fixed lambda-----------------
# N <- nrow(window_data)
# KL <- numeric(N-1)
# KL_s <- 0
# lambda <- 1
# for(j in 1: (N-1)){
#   if(KL_s < lambda){
#     den1 <- density(window_data[j,])
#     den2 <- density(window_data[(j+1),])
#     KL[j] <- Kulback(den1, den2)
#     KL_s <- KL[j]
#     den_p <- den1
#   } else{
#     den2 <- density(window_data[(j+1),])
#     KL[j] <- Kulback(den_p, den2)
#     KL_s <- KL[j]
#   }
# 
# 
# }



## Varying lambda ------------
N <- nrow(window_data)
KL <- numeric(N-1)
KL_s <- 0
lambda_p <- 10
epsilon <- .1
lambda <- lambda*epsilon

for(j in 1: (N-1)){
  if(j %in% c(1,2)){
    den1 <- density(window_data[j,])
    den2 <- density(window_data[(j+1),])
    KL[j] <- Kulback(den1, den2)
  }
  else if(KL_s < lambda){
    den1 <- density(window_data[j,])
    den2 <- density(window_data[(j+1),])
    KL[j] <- Kulback(den1, den2)
    KL_s <- KL[j]
    den_p <- den1
    lambda <- lambda_p*(KL[j-2]+epsilon)
  } else{
    den2 <- density(window_data[(j+1),])
    KL[j] <- Kulback(den_p, den2)
    KL_s <- KL[j]
  }


}


  


n_flag_origin <- length(p1_mm$flag)
flag_ma <- p1_mm$flag[45:(n_flag_origin-45)]
flag_mm_ma <- flag_ma[3601:length(flag_ma)]
length(flag_mm_ma);n

window_data_flag <- numeric(round((n-win_size)/jump_size))
# window_data_flag <- matrix(nrow = row_size, ncol = win_size)
for(i in 1 : round((n-win_size)/jump_size)){
  if(i==1) j <- 1
  cut <- flag_mm_ma[j:(j+win_size-1)]
  cut_sum <- sum(cut)
  if(cut_sum!= 0){
    window_data_flag[i] <- 1
  }
  j <- jump_size+j
  if(j>n) break;
}

plot(ts(KL)[40000:N], main = paste0("lam", lambda, "_win", win_size, "_jump", jump_size),
     ylim = c(-1.5, 15), type="l", col = "orange", xlab = "Time", ylab = "")
lines(window_data_flag[40000:N]*(-1), col = "dodgerblue")
abline(h=1)
legend("topleft", legend = c("Real attack", "KL divergence"),
       col = c("dodgerblue", "orange"), lty = c(1,1))

result <- cbind(KL[40000:N], window_data_flag[40000:N])
colnames(result) <- c("KL", "attack")
head(result)
  #FP
  length(which(result[,1] > 1 & result[,2] == 0)) / length(which(result[,1] > 1))
  #TN
  length(which(result[,1] <= 1 & result[,2] == 0)) / length(which(result[,1] <= 1))
  #TP
  length(which(result[,1] > 1 & result[,2] == 1)) / length(which(result[,1] > 1))
  #FN
  length(which(result[,1] <= 1 & result[,2] == 1)) / length(which(result[,1] <= 1))
