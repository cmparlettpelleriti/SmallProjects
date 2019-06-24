#libs--------------------------------------------------------------------
library(brms)
library(MASS)
library(lme4)
#data--------------------------------------------------------------------
set.seed(100)
cov <- matrix(c(1.0,0.5,0.7,0.6,
                0.5,1.0,0.3,0.2,
                0.7,0.3,1.0,0.3,
                0.6,0.2,0.3,1.0), ncol = 4)
mu <- c(10,20,15,12)

d <- mvrnorm(100,mu= mu, Sigma = cov)
df <- data.frame(d)

df$group <- factor(sample(rep(c(1,2,3,4),25)))
df[df$group == "1", "X1"] <- df[df$group == "1", "X1"] + 4
#frequentist-------------------------------------------------------------
anova(lm(cbind(df$X1,df$X2,df$X3)~df$group + df$X4))
summary(manova(cbind(df$X1,df$X2,df$X3)~df$group + df$X4))

#bayesian----------------------------------------------------------------

#---normal(0,10)---------------------------------------------------------
c <- brm(cbind(X1,X2,X3)~1, data = df, save_all_pars = T)
b <- brm(cbind(X1,X2,X3)~ X4, data = df, save_all_pars = T, prior = set_prior("normal(0,10)"))
a <- brm(cbind(X1,X2,X3)~group + X4, data = df, save_all_pars = T, prior = set_prior("normal(0,10)"))
e <- brm(cbind(X1,X2,X3)~group, data = df, save_all_pars = T, prior = set_prior("normal(0,10)"))


brms::bayes_factor(b,c) #BF_(b,c) = 1514.33737
brms::bayes_factor(a,b) #BF_(a,b) = 4995805590884194759929424601153536.00000
brms::bayes_factor(a,c) #BF_(a,c) = 7454852177624855203567160610028257280.00000
brms::bayes_factor(e,c) #BF_(e,c) = 581991166245196184900087671804657664.00000
brms::bayes_factor(a,e) #BF_(a,e) = 12.82710

#---normal(0,100)---------------------------------------------------------
c <- brm(cbind(X1,X2,X3)~1, data = df, save_all_pars = T)
b <- brm(cbind(X1,X2,X3)~ X4, data = df, save_all_pars = T, prior = set_prior("normal(0,100)"))
a <- brm(cbind(X1,X2,X3)~group + X4, data = df, save_all_pars = T, prior = set_prior("normal(0,100)"))
e <- brm(cbind(X1,X2,X3)~group, data = df, save_all_pars = T, prior = set_prior("normal(0,100)"))


brms::bayes_factor(b,c) #BF_(b,c) = 1.51511
brms::bayes_factor(a,b) #BF_(a,b) = 6123146550124626153832448.00000
brms::bayes_factor(a,c) #BF_(a,c) = 9278151924858376540913664.00000
brms::bayes_factor(e,c) #BF_(e,c) = 723745007326399801407307776.00000
brms::bayes_factor(a,e) #BF_(a,e) = 0.01277

#---normal(0,1)---------------------------------------------------------
c <- brm(cbind(X1,X2,X3)~1, data = df, save_all_pars = T)
b <- brm(cbind(X1,X2,X3)~ X4, data = df, save_all_pars = T, prior = set_prior("normal(0,1)"))
a <- brm(cbind(X1,X2,X3)~group + X4, data = df, save_all_pars = T, prior = set_prior("normal(0,1)"))
e <- brm(cbind(X1,X2,X3)~group, data = df, save_all_pars = T, prior = set_prior("normal(0,1)"))


brms::bayes_factor(b,c) #BF_(b,c) = 1082171.09614
brms::bayes_factor(a,b) #BF_(a,b) = 9137921216869377828177915104198656.00000
brms::bayes_factor(a,c) #BF_(a,c) = 9979672171136587402531419653901664649216.00000
brms::bayes_factor(e,c) #BF_(e,c) = 741835767084291808424136159182979072.00000
brms::bayes_factor(a,e) #BF_(a,e) = 13226.59785

