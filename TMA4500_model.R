source("TMA4500_load_and_format_data.R")

library(INLA)
library(brinla)


################################## model o2 ####################################

#### Linear regression ####

# O2

inla.model.o2 = inla(y ~ 1 + y_u + h, 
                       data = inla.data.o2, control.compute = list(dic = T,waic = T, cpo = T))

mean(summary(inla.model.o2)$cpo$pit)


#CO2

inla.model.co2 = inla(y ~ 1 + y_u, 
                        data = inla.data.co2, control.compute = list(dic = T,waic = T, cpo = T))

summary(inla.model.co2)



#### Model 0 ####

# O2

inla.model.o2.0 = inla(y ~ 1 + y_u + h + f(ID, model = "rw1", constr = F), 
                       data = inla.data.o2, control.compute = list(dic = T,waic = T, cpo = T))

mean(summary(inla.model.o2.0)$cpo$cpo)
inla.cpo(inla.model.co2.0)$cpo$cpo

#CO2

inla.model.co2.0 = inla(y ~ 1 + y_u + f(ID, model = "rw1", constr = F), 
                        data = inla.data.co2, control.compute = list(dic = T,waic = T, cpo = T))
 
summary(inla.model.co2.0)


# store posterior discrepancy plot in dataframe o2
data_mod.o2.0 = data.frame("Time" = data$Time, "mean" = 1e-6*4900000*inla.model.o2.0$summary.random$ID$mean,
                           "lower" = 1e-6*4900000*inla.model.o2.0$summary.random$ID$mean - 1.96*inla.model.o2.0$summary.random$ID$sd, 
                           "upper" = 1e-6*4900000*inla.model.o2.0$summary.random$ID$mean + 1.96*inla.model.o2.0$summary.random$ID$sd)

# store posterior discrepancy plot in dataframe ch2
data_mod.co2.0 = data.frame("Time" = data$Time, "mean" = 1e-6*4900000*inla.model.co2.0$summary.random$ID$mean,
                            "lower" = 1e-6*4900000*inla.model.co2.0$summary.random$ID$mean - 1.96*inla.model.co2.0$summary.random$ID$sd, 
                            "upper" = 1e-6*4900000*inla.model.co2.0$summary.random$ID$mean + 1.96*inla.model.co2.0$summary.random$ID$sd)



difference.mod.0 = data.frame("Time" = data$Time, 
                              "mean" = 1e-6*4900000*inla.model.o2.0$summary.random$ID$mean + 1e-6*4900000*inla.model.co2.0$summary.random$ID$mean,
                              "var" = inla.model.o2.0$summary.random$ID$sd^2 + inla.model.co2.0$summary.random$ID$sd^2)

difference.mod.0$lower = difference.mod.0$mean - 1.96*sqrt(difference.mod.0$var)
difference.mod.0$upper = difference.mod.0$mean + 1.96*sqrt(difference.mod.0$var)


#### Model 1: gaussian priors ####

prior.fixed.o2 = list(mean.intercept = constants$b0_o2, 
                   prec.intercept = constants$b0_o2_prec,
                   mean = list(y_u = constants$b1_o2, h = constants$b2_o2),
                   prec = list(y_u = constants$b1_o2_prec, h = constants$b2_o2_prec))

inla.model.o2.1 = inla(y ~ 1 + y_u + h + f(ID, model = "rw1", constr = F), 
                       data = inla.data.o2, control.fixed = prior.fixed.o2, 
                       family = "gaussian", control.compute = list(dic = T,waic = T, cpo = T),
                       control.predictor(compute = T))

mean(summary(inla.model.o2.1)$cpo$cpo)

data_mod.o2.1 = data.frame("Time" = data$Time, "mean" = 1e-6*4900000*inla.model.o2.1$summary.random$ID$mean,
                           "lower" = 1e-6*4900000*inla.model.o2.1$summary.random$ID$mean - 1.96*inla.model.o2.1$summary.random$ID$sd, 
                           "upper" = 1e-6*4900000*inla.model.o2.1$summary.random$ID$mean + 1.96*inla.model.o2.1$summary.random$ID$sd)

inla.model.o2.2$summary.fitted.values

prior.fixed.co2 = list(mean.intercept = constants$b0_co2, 
                   prec.intercept = constants$b0_co2_prec,
                   mean = list(y_u = constants$b1_co2),
                   prec = list(y_u = constants$b1_co2_prec))



inla.model.co2.1 = inla(y ~ 1 + y_u + f(ID, model = "rw1", constr = F), 
                       data = inla.data.co2, control.fixed = prior.fixed.co2, 
                       family = "gaussian", control.compute = list(dic = T,waic = T, cpo = T),
                       control.predictor(compute = T))

summary(inla.model.co2.1)

data_mod.co2.1 = data.frame("Time" = data$Time, "mean" = 1e-6*4900000*inla.model.co2.1$summary.random$ID$mean,
                           "lower" = 1e-6*4900000*inla.model.co2.1$summary.random$ID$mean - 1.96*inla.model.co2.1$summary.random$ID$sd, 
                           "upper" = 1e-6*4900000*inla.model.co2.1$summary.random$ID$mean + 1.96*inla.model.co2.1$summary.random$ID$sd)

difference.mod.1 = data.frame("Time" = data$Time, 
                              "mean" = 1e-6*4900000*inla.model.o2.1$summary.random$ID$mean + 1e-6*4900000*inla.model.co2.1$summary.random$ID$mean,
                              "var" = inla.model.o2.1$summary.random$ID$sd^2 + inla.model.co2.1$summary.random$ID$sd^2)

difference.mod.1$lower = difference.mod.1$mean - 1.96*sqrt(difference.mod.1$var)
difference.mod.1$upper = difference.mod.1$mean + 1.96*sqrt(difference.mod.1$var)


#### Model 2: PC hyperpriors ###

prior.l.o2 = list(prec = list(prior = "pc.prec", param = c(1, 0.99)))

prior.f.o2 = list(prec = list(prior = "pc.prec", param = c(1, 0.01)))

inla.model.o2.2 = inla(y ~ 1 + y_u + h + f(ID, model = "rw1", hyper = prior.f.o2, constr = F),
                       data = inla.data.o2, control.fixed = prior.fixed.o2,
                       family = "gaussian", control.family = list(hyper = prior.l.o2),
                       control.predictor(compute = T), control.compute = list(dic = T,waic = T, cpo = T))

mean(summary(inla.model.o2.2)$cpo$cpo)

data_mod.o2.2 = data.frame("Time" = data$Time, "mean" = 1e-6*4900000*inla.model.o2.2$summary.random$ID$mean,
                           "lower" = 1e-6*4900000*inla.model.o2.2$summary.random$ID$mean - 1.96*inla.model.o2.2$summary.random$ID$sd, 
                           "upper" = 1e-6*4900000*inla.model.o2.2$summary.random$ID$mean + 1.96*inla.model.o2.2$summary.random$ID$sd)

prior.l.co2 = list(prec = list(prior = "pc.prec", param = c(1, 0.99)))

prior.f.co2 = list(prec = list(prior = "pc.prec", param = c(1, 0.01)))


inla.model.co2.2 = inla(y ~ 1 + y_u + f(ID, model = "rw1", hyper = prior.f.co2, constr = F), 
                        data = inla.data.co2, control.fixed = prior.fixed.co2, 
                        family = "gaussian", control.family = list(hyper = prior.l.co2),
                        control.predictor(compute = T), control.compute = list(dic = T,waic = T, cpo = T))

summary(inla.model.co2.2)

data_mod.co2.2 = data.frame("Time" = data$Time, "mean" = 1e-6*4900000*inla.model.co2.2$summary.random$ID$mean,
                            "lower" = 1e-6*4900000*inla.model.co2.2$summary.random$ID$mean - 1.96*inla.model.co2.2$summary.random$ID$sd, 
                            "upper" = 1e-6*4900000*inla.model.co2.2$summary.random$ID$mean + 1.96*inla.model.co2.2$summary.random$ID$sd)


difference.mod.2 = data.frame("Time" = data$Time, 
                              "mean" = 1e-6*4900000*inla.model.o2.2$summary.random$ID$mean + 1e-6*4900000*inla.model.co2.2$summary.random$ID$mean,
                              "var" = inla.model.o2.2$summary.random$ID$sd^2 + inla.model.co2.2$summary.random$ID$sd^2)

difference.mod.2$lower = difference.mod.2$mean - 1.96*sqrt(difference.mod.2$var)
difference.mod.2$upper = difference.mod.2$mean + 1.96*sqrt(difference.mod.2$var)


#### Model 3: looser gaussian priors ####

prior.l.o2.3 = list(prec = list(prior = "pc.prec", param = c(1, 0.99)))

prior.f.o2.3 = list(prec = list(prior = "pc.prec", param = c(1, 0.01)))

prior.fixed.o2.3 = list(mean.intercept = constants$b0_o2, 
                      prec.intercept = constants$b0_o2_prec_1,
                      mean = list(y_u = constants$b1_o2, h = constants$b2_o2),
                      prec = list(y_u = constants$b1_o2_prec_1, h = constants$b2_o2_prec))

prior.fixed.co2.3 = list(mean.intercept = constants$b0_co2, 
                       prec.intercept = constants$b0_co2_prec_1,
                       mean = list(y_u = constants$b1_co2),
                       prec = list(y_u = constants$b1_co2_prec_1))


prior.l.co2.3 = list(prec = list(prior = "pc.prec", param = c(1, 0.99)))

prior.f.co2.3 = list(prec = list(prior = "pc.prec", param = c(1, 0.01)))

inla.model.o2.3 = inla(y ~ 1 + y_u + h + f(ID, model = "rw1", hyper = prior.f.o2.3, constr = F),
                       data = inla.data.o2, control.fixed = prior.fixed.o2.3,
                       family = "gaussian", control.family = list(hyper = prior.l.o2.3),
                       control.predictor(compute = T), control.compute = list(dic = T,waic = T, cpo = T))

mean(summary(inla.model.o2.3)$cpo$cpo)

data_mod.o2.3 = data.frame("Time" = data$Time, "mean" = 1e-6*4900000*inla.model.o2.3$summary.random$ID$mean,
                           "lower" = 1e-6*4900000*inla.model.o2.3$summary.random$ID$mean - 1.96*inla.model.o2.3$summary.random$ID$sd, 
                           "upper" = 1e-6*4900000*inla.model.o2.3$summary.random$ID$mean + 1.96*inla.model.o2.3$summary.random$ID$sd)


inla.model.co2.3 = inla(y ~ 1 + y_u + f(ID, model = "rw1", hyper = prior.f.co2.3, constr = F), 
                        data = inla.data.co2, control.fixed = prior.fixed.co2.3, 
                        family = "gaussian", control.family = list(hyper = prior.l.co2.3),
                        control.predictor(compute = T), control.compute = list(dic = T,waic = T, cpo = T))

summary(inla.model.co2.3)

data_mod.co2.3 = data.frame("Time" = data$Time, "mean" = 1e-6*4900000*inla.model.co2.3$summary.random$ID$mean,
                            "lower" = 1e-6*4900000*inla.model.co2.3$summary.random$ID$mean - 1.96*inla.model.co2.3$summary.random$ID$sd, 
                            "upper" = 1e-6*4900000*inla.model.co2.3$summary.random$ID$mean + 1.96*inla.model.co2.3$summary.random$ID$sd)

difference.mod.3 = data.frame("Time" = data$Time, 
                              "mean" = 1e-6*4900000*inla.model.o2.3$summary.random$ID$mean + 1e-6*4900000*inla.model.co2.3$summary.random$ID$mean,
                              "var" = inla.model.o2.3$summary.random$ID$sd^2 + inla.model.co2.3$summary.random$ID$sd^2)

difference.mod.3$lower = difference.mod.3$mean - 1.96*sqrt(difference.mod.3$var)
difference.mod.3$upper = difference.mod.3$mean + 1.96*sqrt(difference.mod.3$var)

#### priors to fixed effects and discrepancy ####

prior.prec = list(prec = list(prior = "loggamma", param = c(10, 0.045)))

inla.model.o2.3 = inla(y ~ 1 + y_u + h + f(ID, model = "rw1", hyper = prior.prec), 
                       data = inla.data.o2, control.fixed = prior.fixed)

summary(inla.model.o2.3)

plot(inla.model.o2.3$marginals.fixed[[1]], type = "l", col = "red", lwd = 1.5)

bri.hyperpar.plot(inla.model.o2.3)

############################## baseline model co2 ##############################
inla.model.co2.1 = inla(y ~ 1 + y_u + f(ID, model = "rw1"), data = inla.data.co2) 

# summary output
summary(inla.model.co2.1)

# plot posterior likelihood
plot(inla.model.co2.1$marginals.fixed[[1]], type = "l", col = "red", lwd = 1.5)

# plot posterior SD of error and random effect
bri.hyperpar.plot(inla.model.co2.1)


#### priors to fixed effects ####

prior.fixed = list(mean.intercept = constants$b0_co2, prec.intercept = 2500,
                   mean = constants$b1_co2, prec = 2500)

inla.model.co2.2 = inla(y ~ 1 + y_u + f(ID, model = "rw1"), data = inla.data.co2,
                        control.fixed = prior.fixed)

summary(inla.model.co2.2)

plot(inla.model.co2.2$marginals.fixed[[1]], type = "l", col = "red", lwd = 1.5)

bri.hyperpar.plot(inla.model.co2.2)


#### priors to fixed effects and discrepancy ####

prior.prec = list(prec = list(prior = "loggamma", param = c(10, 0.045)))

inla.model.co2.3 = inla(y ~ 1 + y_u + f(ID, model = "rw1", hyper = prior.prec), 
                        data = inla.data.co2, control.fixed = prior.fixed)

summary(inla.model.co2.3)

#plot(inla.model.o2.2$summary.random$ID$mean, type = "l")

#plot(inla.model.co2.3$marginals.fixed[[1]], type = "l", col = "red", lwd = 1.5)

#bri.hyperpar.plot(inla.model.co2.3)

#constants$b1_o2
#constants$b2_o2


#,
#mean = c(constants$b1_o2, constants$b2_o2), prec = c(2500,2500))
# fitting a model with gaussian priors for discrepancy function
prec.prior.1 = list(prec = list(prior = "ar1", param = c(mean(data$delta_oxygen)), 0.01), 
                    initial = mean(data$delta_oxygen))

inla.formula.1 = y ~ constants$b0_o2 + constants$b1_o2*y_u + constants$b2_o2*h + 
  f(constants$b2_o2*delta_o2, model = "iid")


#plot(inla.model.o2.1$marginals.fixed[[1]], type = "l", col = "red", lwd = 1.5)

#names(inla.models()$latent)

#names(inla.models()$likelihood)

#inla.doc("gaussian")
#names(inla.models()$prior)
#inla.set.control.fixed.default()[c("mean.intercept", "prec.intercept", "mean", "prec")]
#inla.doc("control.fixed")
#inla.doc("pc.prec")


#plot(inla.model.o2.2$summary.random$ID$mean, type = "l")