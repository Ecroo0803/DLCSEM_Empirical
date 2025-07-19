library(readxl)
library(dplyr)
library(purrr)
library(mvtnorm)
library(MASS)
library(R2jags)
library(Matrix)
library(loo)
options(max.print=100000)
dat<-read_excel('final_data.xlsx')
dat$diagnose<-ifelse(dat$Diagnose1 !='NA'|dat$Diagnose2!='NA',1,0)
sum(is.na(dat$diagnose))

dat$diagnose[is.na(dat$diagnose)] <- 0
dat$diagnose<-as.factor(dat$diagnose)
dat1<-subset(dat,dat$diagnose==1) 

#unique_users1 <- dat %>% distinct(User)#671 users
df <-dat1%>% dplyr::select('User','day_cnt','item9','item11','item12','item13','item14','item17','item31','item32', 'item35')

df[is.na(df)]<-0

df1 <- df %>%
  group_by(User, week_group = (day_cnt - 1) %/% 7 + 1) %>%  # Create 7-day blocks
  summarise(across(starts_with("item"), mean, na.rm = TRUE), .groups = "drop")  # Calculate mean
N<-length(levels(as.factor(df1$User)))#315
Ntmax <- max(df1$week_group)

usernom<-levels(as.factor(df$User))
itemnom<-c('item9','item11','item12','item13','item14','item17')


yt <- array(NA, dim = c(N, Ntmax, 6))

for (i in 1:N) {
  user_data <- df1 %>% filter(User == usernom[i])
  
  for (j in 1:nrow(user_data)) {
    yt[i, j, ] <- unlist(user_data[j, itemnom])
  }
}
sum(is.na(yt))

# Compute Nt for each user
Nt <- c()
Nt0 <- matrix(0, N, Ntmax - 1)
str(Nt)
for (i in 1:N) {
  for (j in 1:(Ntmax - 2)) {
    if (mean(apply(is.na(yt[i, -(1:j), ]), 1, mean)) == 1) {
      Nt0[i, j] <- 1
    }
    
    if (mean(is.na(yt[i, -(1:(Ntmax - 1)), ])) == 1) {
      Nt0[i, Ntmax - 1] <- 1
    }
  }
  Nt[i] <- Ntmax - sum(Nt0[i, ])
}
# Standardize data based on initial time point
colmeans <- apply(yt[, 1, ], 2, mean, na.rm = TRUE)
colsd <- apply(yt[, 1, ], 2, sd, na.rm = TRUE)

yt.center <- yt
nitems <- length(itemnom)

for (j in 1:Ntmax) {
  for (k in 1:nitems) {
    yt.center[, j, k] <- (yt[, j, k] - colmeans[k]) / colsd[k]
  }
}

data.jags <- list(N=N,Nt=Nt,y1=yt.center,mu0=c(0,0,0,0,0,0),PHinv=diag(6))
params_1 <- c('est1.S1', 'est1.S2', 'b3', 'b1.S1','b2.S1','phi.S1', 'phi.S2', 'ly1.S1', 'ly1.S2',#'PS1',#'S',
		'sigmazeta1.S1', 'sigmazeta1.S2',"sigmazeta2.S1",'sigmazeta2.S2','PS1','S')
#params_1<-c('PS1','S')
model1 <- jags.parallel(data = data.jags, 
                        parameters.to.save = params_1, 
                        model.file = "switchmdl4itm_3.txt", 
                        n.chains = 3, 
                        n.iter = 50000, 
                        n.burnin = 25000,
                        n.thin = 1)

est1 <- round(model1$BUGSoutput$summary,3)
write.csv(est1,file='mdl3i6wk7_2.csv')
