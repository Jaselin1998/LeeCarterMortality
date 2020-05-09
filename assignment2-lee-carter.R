library(dplyr)
library(ggplot2)
library(demography)
library(readxl)
library(forecast)
library(StMoMo)
library(gridExtra)
#load datasets
female_Belgium <- read_excel("life tables_Belgium.xlsx", sheet = "females")
male_Belgium <- read_excel("life tables_Belgium.xlsx", sheet = "males")
all_Belgium <- read_excel("life tables_Belgium.xlsx", sheet = "total")

names(all_Belgium)
attach(female_Belgium)
#inspect data ranges
cat("Female age ranges ", "from", min(female_Belgium$Age),
    "to",max(female_Belgium$Age),"during",min(female_Belgium$Year),"and",
    max(female_Belgium$Year))
cat("Male age ranges ", "from", min(male_Belgium$Age),
    "to",max(male_Belgium$Age),"during",min(male_Belgium$Year),"and",
    max(male_Belgium$Year))
cat("All age ranges ", "from", min(all_Belgium$Age),
    "to",max(all_Belgium$Age),"during",min(all_Belgium$Year),"and",
    max(all_Belgium$Year))
#descriptive analysis
plot1<-ggplot(male_Belgium,aes(x= Year, y = log(qx)))+geom_point()+
  geom_line(stat='identity')+ggtitle("Beigium - males, 1840-2018")+labs(y = bquote(ln(q[x])))
plot2 <-ggplot(female_Belgium,aes(x= Year, y = log(qx)))+geom_point()+
  geom_line(stat='identity')+ggtitle("Beigium - females, 1840-2018")+labs(y = bquote(ln(q[x])))
plot3<- ggplot(all_Belgium,aes(x= Year, y = log(qx)))+geom_point()+
  geom_line(stat='identity')+ggtitle("Beigium - all, 1840-2018")+labs(y = bquote(ln(q[x])))
grid.arrange(plot1,plot2,plot3, ncol = 2)
#a gap between 1914 and 1918, so we start from 1919
male <- male_Belgium%>%filter(Year >=1919)
female <- female_Belgium%>%filter(Year >=1919)
total <- all_Belgium%>% filter(Year >=1919)
#check statistics
checkstatistics <- function(male){
  k1<-ggplot(male, aes(Year,y = log(mx),color = Age))+geom_point()+geom_line(stat = 'identity')+
    theme(legend.position = "none")+ggtitle(paste("Belgium",gender," 1919-2018"))+ylab("Logrithmn of central death rate")
  #above: indicating a linear year effect on log(mx) for all ages 
  k2<-ggplot(male, aes(Age,y = log(mx),color = Year))+geom_point()+geom_line(aes(x= Age))+
    theme(legend.position = "none")+ggtitle(paste("Belgium",gender," 1919-2018"))+ylab("Logrithmn of central death rate")
  # above: indicating a different year effect on log(mx) in all ages 
  # for a group of a children, the death rate is significantly higher, so we decide to leave them out
  new_data <- male%>%mutate(logmx = log(mx))%>%filter(Age>20|logmx < -2.5 )
  k3 <-ggplot(new_data, aes(Age,y = log(mx),color = Year))+geom_point()+geom_line(aes(x= Age))+
    theme(legend.position = "none")+ggtitle(paste("Belgium",gender," 1919-2018"," aged 20-110"))+ylab("Logrithmn of central death rate")
  grid.arrange(k1,k2,k3,ncol = 2)
  return(new_data)
  }

##------------
#Choose 2018 as an example
male_2018 <- male_new %>% filter(Year ==2018)
ggplot(male_2018,aes(Age, y = log(mx)))+geom_point()+geom_line()
#A linear trend confirmed
##------------
## assumptions: log(mu_{x,t}) = a_x+b_{x,t}*r_t+ error with least square
##---------
#find_alpha  : to return a plot of alpha values and the list of alpha values
gender = "male"
find_alpha <- function(new){
  X <- model.matrix(
    ~ as.factor(new$Age) 
    - 1)
  dim(X)
  min(new$Age)
  max(new$Age)
  y <- log(new$mx)
  agemodel <- lm(y ~ -1+as.factor(new$Age))
  a_x <- agemodel$coefficients
  a_list <- data.frame(Age =agemodel$xlevels$`as.factor(new$Age)`,alpha = a_x)
  #visualize a list
  alphaplot <- ggplot(a_list,aes(x = Age, y = alpha))+geom_smooth(se=FALSE)+geom_point()+
    geom_line(aes(Age,alpha),stat = "identity")+
    ggtitle(paste("Belgium",gender, "1919-2018"))+ylab("Alpha")+theme_bw()
  ls <- list("plot" = alphaplot,"alist"=a_list)
  return(ls)
}
#find_kappa : return a plot of kappa values and big dataset with alpha, kappa values, and a single kappa values' dataset
find_kappa <-function(new,a_list){
  #estimate year effect
  #new variable z = log(mx)-a
  with_a <- left_join(new,a_list,by = "Age")
  z <- with_a$logmx - with_a$alpha
  yearmodel <- lm(z~-1+as.factor(new$Year))
  kappa_list <- data.frame(Year = yearmodel$xlevels$`as.factor(new$Year)`,kappa = yearmodel$coefficients)
  #visualize kappa
  kappaplot<- ggplot(kappa_list,aes(x = Year, y = kappa))+geom_point()+geom_smooth(se=FALSE)+
    theme_bw()+ggtitle(paste("Belgium",gender, "1919-2018"," starting values"))
  #combine kappa
  with_a$Year <- as.factor(with_a$Year)
  with_kappa <- left_join(with_a,kappa_list,by = "Year")
  ls <- list("plot"=kappaplot,"with_kappa"=with_kappa,"kappa_list"= kappa_list,"Z" = z)
  return(ls)
}
#find_beta : return a plot of beta values and big dataset with alpha,kappa and beta values, and a single beta values' dataset
find_beta <-function(with_kappa,z){
  betamodel <- lm(z~-1+as.factor(with_kappa$Age):with_kappa$kappa)
  beta_list <- data.frame(name = betamodel$xlevels$`as.factor(with_kappa$Age)`,
                          betas = betamodel$coefficients)
  #visualize betas
  beta_plot <- ggplot(beta_list,aes(x = name, y= betas))+geom_point()+geom_line()+
    ggtitle(paste("Belgium ",male," 1919-2018, starting values"))
  with_beta <- left_join(with_kappa,beta_list,by = c("Age" = "name"))
  ls <-list("plot"= beta_plot,"beta_list" = beta_list,"with_beta" = with_beta)
  return(ls)
}
#doconvergence : it is simply the convergence process and return the final converged beta/ kappa values
doconvergence <- function(beta_list,kappa_list,with_beta,z) {
  #convergence
  converged = F
  iter      = 1
  beta_est  = beta_list$betas
  kappa_est = kappa_list$kappa
  while(!converged){  
    beta_est_old  = beta_est
    kappa_est_old = kappa_est
    # (2): estimate kappa's
    var_beta = with_beta$betas
    X        = model.matrix(~ as.factor(with_beta$Year):var_beta - 1)
    kappa_est = solve(crossprod(X)) %*% t(X) %*% z
    
    # (3): estimate beta's
    var_kappa = with_beta$kappa
    X         = model.matrix(~ as.factor(with_beta$Age):var_kappa - 1)
    beta_est   = solve(crossprod(X)) %*% t(X) %*% z 
    
    # stopping criterion
    converged = 
      max(abs(beta_est - beta_est_old) / abs(beta_est_old), abs(kappa_est - kappa_est_old) / abs(kappa_est_old)) < 1e-8
    iter = iter + 1
    if(iter %% 1e2 == 0)
      cat("\n\nIteration number", iter, "\n\n")
 
  }
  ls <-list("beta_est"= beta_est,"kappa_est"= kappa_est)
  return(ls)
}
#visualize convergence: return final kappa and beta values for the final ploting and comparison
visualize_convergence <- function(kappa_list,beta_list,kappa_est,beta_est){
  kappafinal <- cbind(kappa_list,kappa_est)
  betafinal <- cbind(beta_list,beta_est)
  ggplot(kappafinal,aes(x=as.numeric(Year)))+geom_line(aes(y = kappa))+
    geom_line(aes(y = kappa_est),color="red")+xlab("Years")+ggtitle("Kappa final values V.S. starting values")
  ggplot(betafinal,aes(x = as.numeric(name)))+geom_point(aes(y = betas),color="black")+
    geom_line(aes(y = beta_est),color = "red")+xlab("Age")+ggtitle("Beta final values V.S. starting values")
  #apply constraints
  #sum(kappa_{x,t} = 0)
  #sum(beta_{x,t} = 1)
  ls <- list("kappafinal" = kappafinal, "betafinal" = betafinal)
  return(ls)
}
#find_ls: return the least-square estimates
find_ls <- function(beta_est,kappa_est,a_list){
  beta_est <- apply(beta_est,2,as.numeric)
  kappa_est <- apply(kappa_est,2,as.numeric)
  alpha_ls <-apply(a_list,2,as.numeric)
  beta_ls <-beta_est/sum(beta_est)
  kappa_ls <- (kappa_est-mean(kappa_est))*sum(beta_est)
  alpha_ls <-a_list$alpha + beta_est*mean(kappa_est)
  cat("The sum of beta_ls is ",sum(beta_ls),"\n")
  cat("The sum of kappa_ls is ", sum(kappa_ls))
  ls <- list("bls" = beta_ls,"kls" = kappa_ls,"als"=alpha_ls)
  return(ls)
}
#plotfinals: final plots of alpha, kappa and beta values
plotfinals <- function(kappafinal,betafinal,a_list){
  kappafinal$Year<- as.numeric(kappafinal$Year)
  betafinal$name <- as.numeric(betafinal$name)
  a_list$Age <- as.numeric(a_list$Age)
  g1 <-ggplot(kappafinal,aes(x = Year, y = kappa_est))+geom_point()+geom_line()+
    ggtitle(paste("Kappa values for Belgium",gender,"1919-2018"))
  g2 <-ggplot(betafinal,aes(x = name, y = beta_est))+geom_point()+geom_line()+
    ggtitle(paste("Beta values for Belgium",gender,"1919-2018"))+xlab("Age")
  g3 <- ggplot(a_list,aes(x = Age,y = alpha))+geom_point()+geom_line()+
    ggtitle(paste("Kappa values for Belgium",gender,"1919-2018"))
  grid.arrange(g1,g2,g3, ncol=2)
}
#for male
gender = "male"
male_new <- checkstatistics(male)
malpha_model <- find_alpha(new = male_new)
malpha_model$plot
mkappa_model <- find_kappa(new = male_new,a_list = malpha_model$alist)
mbeta_model <- find_beta(with_kappa = mkappa_model$with_kappa,z = mkappa_model$Z)
mconverged_est <- doconvergence(mkappa_model$kappa_list,
                               beta_list = mbeta_model$beta_list,
                               with_beta = mbeta_model$with_beta,
                               z=mkappa_model$Z)
mfinalest<- visualize_convergence(kappa_list = mkappa_model$kappa_list,
                                 beta_list = mbeta_model$beta_list,
                                 kappa_est = mconverged_est$kappa_est,
                                 beta_est = mconverged_est$beta_est)
male_ls <- find_ls(beta_est = finalest$betafinal,
                   kappa_est = finalest$kappafinal,
                   a_list = malpha_model$alist)
plotfinals(kappafinal = finalest$kappafinal,
           betafinal = finalest$betafinal,
           a_list = alpha_model$alist)
#for female
gender = "female"
female_new <- checkstatistics(female)
falpha_model <- find_alpha(new = female_new)
falpha_model$plot
fkappa_model <- find_kappa(new = female_new,a_list = falpha_model$alist)
fbeta_model <- find_beta(mwith_kappa = fkappa_model$male_kappa,
                        z = fkappa_model$Z)
fconverged_est <- doconvergence(fkappa_model$kappa_list,
                               beta_list = fbeta_model$beta_list,
                               with_beta = fbeta_model$with_beta,
                               z = fkappa_model$Z)
ffinalest<- visualize_convergence(kappa_list = fkappa_model$kappa_list,beta_list = beta_model$beta_list,
                                 kappa_est = fconverged_est$kappa_est,beta_est = converged_est$beta_est)
female_ls <- find_ls(beta_est = finalest$betafinal,
                     kappa_est = finalest$kappafinal,
                     a_list = falpha_model$alist)
plotfinals(kappafinal = finalest$kappafinal,betafinal = finalest$betafinal,a_list = alpha_model$alist)
#for all populations
gender = "all"
all_new <- checkstatistics(total)
lalpha_model <- find_alpha(new = all_new)
lalpha_model$plot
lkappa_model <- find_kappa(new = all_new,a_list = lalpha_model$alist)
lbeta_model <- find_beta(with_kappa = lkappa_model$with_kappa,
                        z = lkappa_model$Z)
lconverged_est <- doconvergence(lkappa_model$kappa_list,
                               beta_list = lbeta_model$beta_list,
                               with_beta = lbeta_model$with_beta,
                               z = lkappa_model$Z)
lfinalest<- visualize_convergence(kappa_list = lkappa_model$kappa_list,beta_list = lbeta_model$beta_list,
                                 kappa_est = lconverged_est$kappa_est,beta_est = lconverged_est$beta_est)
all_ls <- find_ls(beta_est = finalest$betafinal,kappa_est = finalest$kappafinal,
                  a_list = alpha_model$alist)
plotfinals(kappafinal = finalest$kappafinal,betafinal = finalest$betafinal,a_list = alpha_model$alist)
##model comparison
finalmodel <- function(with_kappa, kappafinal,betafinal){
  with_final <- left_join(with_kappa,kappafinal,by = "Year" )
  with_final <- left_join(with_final,betafinal,by = c("Age" = "name"))
  with_final_2 <- with_final %>% mutate(newmx =exp(alpha + kappa_est*beta_est) )
  return(with_final_2)
}
mdata <- finalmodel(mkappa_model$with_kappa,mfinalest$kappafinal,mfinalest$betafinal)
ggplot(mdata,aes(x = mx,y = newmx))+geom_point()+
  geom_abline(intercept = 0,slope=1,size=5,color = "red")+
  ggtitle("QQ-plot for male")
fdata <- finalmodel(fkappa_model$with_kappa,ffinalest$kappafinal,ffinalest$betafinal)
ggplot(fdata,aes(x = mx,y = newmx))+
  geom_point()+
  geom_abline(intercept = 0,slope=1,size=5,color = "red")+
  ggtitle("QQ-plot for female")
ldata <- finalmodel(lkappa_model$with_kappa,lfinalest$kappafinal,lfinalest$betafinal)
ggplot(ldata,aes(x = mx,y = newmx))+
  geom_point()+
  geom_abline(intercept = 0,slope=1,size=5,color = "red")+
  ggtitle("QQ-plot for male")
mean((mdata$mx-mdata$newmx)^2)
mean((fdata$mx-fdata$newmx)^2)