source("TMA4500_load_and_format_data.R")
source("TMA4500_model.R")


library(ggplot2)
library(ggpubr)

########################### Preliminary raw-data plots #########################


h1_plot = ggplot(data = data, aes(x=Time)) + 
  geom_line(aes(y = o2_added_1), lwd = 1.5) + 
  scale_y_continuous(bquote(h[1](t) ~ " [kg/min]"), limits = c(0.165,0.20))  +
  theme_bw() +
  theme(text = element_text(size = 35))


h2_plot = ggplot(data = data, aes(x=Time)) + 
  geom_line(aes(y = o2_added_2), lwd = 1.5) + 
  scale_y_continuous(bquote(h[2](t) ~ " [kg/min]"), limits = c(0.165,0.20)) +
  theme_bw() +
  theme(text = element_text(size = 35)) 

ggarrange(h1_plot, h2_plot)

h_plot = ggplot(data = data, aes(x=Time)) + 
  geom_line(aes(y = o2_added_2 + o2_added_1), lwd = 1.5) + 
  scale_y_continuous(bquote(h(t) ~ " [kg/min]"), limits = c(0.33,0.39)) +
  theme_bw() +
  theme(text = element_text(size = 35)) 

h_plot

c_o2_c_plot = ggplot(data = data, aes(x=Time)) + 
  geom_line(aes(y = oxygen_mgl_c), lwd = 1.5) + 
  scale_y_continuous(bquote({y[O[2]]^C} (t)~"[mg/l]"), limits = c(7.8,8.63)) +
  theme_bw() +
  theme(text = element_text(size = 35))

c_o2_e_plot = ggplot(data = data, aes(x=Time)) + 
  geom_line(aes(y = oxygen_mgl_e), lwd = 1.5) + 
  scale_y_continuous(bquote({y[O[2]]^E} (t)~"[mg/l]"), limits = c(7.8,8.63)) +
  theme_bw() +
  theme(text = element_text(size = 35))

ggarrange(c_o2_c_plot, c_o2_e_plot)

avg_c_o2_plot = ggplot(data = data, aes(x=Time)) + 
  geom_line(aes(y = (oxygen_mgl_c + oxygen_mgl_e)*0.5), lwd = 1.5) + 
  scale_y_continuous(bquote({y[O[2]]} (t)~"[mg/l]"), limits = c(7.25,8.5)) +
  theme_bw() +
  theme(text = element_text(size = 35))

avg_c_o2_plot

qf_plot = ggplot(data = data, aes(x=Time)) + 
  geom_line(aes(y = fresh_flow), lwd = 1.5) + 
  scale_y_continuous(bquote({q[f]} (t)~"[l/min]"), limits = c(24500, 27500)) +
  theme_bw() +
  theme(text = element_text(size = 35))

qr_plot = ggplot(data = data, aes(x=Time)) + 
  geom_line(aes(y = recycled_flow), lwd = 1.5) + 
  scale_y_continuous(bquote({q[r]} (t)~"[l/min]"), limits = c(24500, 27500)) +
  theme_bw() +
  theme(text = element_text(size = 35))

ggarrange(qf_plot, qr_plot)

q_plot = ggplot(data = data, aes(x=Time)) + 
  geom_line(aes(y = recycled_flow + fresh_flow), lwd = 1.5) + 
  scale_y_continuous(bquote({q} (t)~"[l/min]"), limits = c(51500, 52500)) +
  theme_bw() +
  theme(text = element_text(size = 35))

q_plot

c_co2_plot = ggplot(data = data, aes(x = Time)) +
  geom_line(aes(y = c_co2_tank), lwd = 1.5) + 
  scale_y_continuous(bquote({y[CO[2]]} (t) ~ "[mg/l]"), limits = c(7.25, 8.5)) +
  theme_bw() +
  theme(text = element_text(size = 35))

c_co2_plot

ggarrange(avg_c_o2_plot, c_co2_plot)

TC_plot = ggplot(data = data, aes(x=Time)) + 
  geom_line(aes(y = temp_center), lwd = 1.5) + 
  scale_y_continuous(bquote({T^C} (t) ~"[C]"), limits = c(11.875, 11.95)) +
  theme_bw() +
  theme(text = element_text(size = 35))


TE_plot = ggplot(data = data, aes(x=Time)) + 
  geom_line(aes(y = temp_edge), lwd = 1.5) + 
  scale_y_continuous(bquote({T^E} (t) ~"[C]"), limits = c(11.875, 11.95)) +
  theme_bw() +
  theme(text = element_text(size = 35))


ggarrange(TC_plot, TE_plot)

###############################

temperature_plot = ggplot(data = data, aes(x=Time)) + 
  geom_line(aes(y = temp_center, colour = "Center sensor"), lwd = 1.5) + 
  geom_line(aes(y = mean(temp_center), colour = "Center sensor"), lwd = .5) +
  geom_ribbon(aes(ymin = mean(temp_center) - 1.96*sd(temp_center), 
                  ymax = mean(temp_center) + 1.96*sd(temp_center)), alpha = 0.05, fill = "red") +
  geom_line(aes(y = temp_edge, colour = "Edge sensor"), lwd = 1.5) + 
  geom_line(aes(y = mean(temp_edge), colour = "Edge sensor"), lwd = .5) +
  geom_ribbon(aes(ymin = mean(temp_edge) - 1.96*sd(temp_edge), 
                  ymax = mean(temp_edge) + 1.96*sd(temp_edge)), alpha = 0.05, fill = "blue") +
  scale_colour_manual("", breaks = c("Center sensor", "Edge sensor"),
                      values = c("Center sensor" = "red", "Edge sensor" = "blue")) + 
  labs(x = "Time", y = expression(Temperature~"("~degree~C~")")) +
  theme(text = element_text(size = 20))

temperature_plot

mean_temperature_plot = ggplot(data = data, aes(x=Time)) + 
  geom_line(aes(y = (temp_center + temp_edge)/2, colour = "Edge sensor"), lwd = 1.5) + 
  geom_line(aes(y = mean((temp_center + temp_edge)/2), colour = "Edge sensor"), lwd = .5) +
  geom_ribbon(aes(ymin = mean((temp_center + temp_edge)/2) - 1.96*sd((temp_center + temp_edge)/2), 
                  ymax = mean((temp_center + temp_edge)/2) + 1.96*sd((temp_center + temp_edge)/2)), 
              alpha = 0.05, fill = "purple") +
  scale_colour_manual("", breaks = c("Edge sensor"),
                      values = c("Edge sensor" = "purple")) + 
  labs(x = "Time", y = bquote("Temperature ("~degree~"C)")) +
  theme(text = element_text(size = 20))

mean_temperature_plot

h1_plot = ggplot(data = data, aes(x=Time)) + 
  geom_line(aes(y = o2_added_1, colour = "Injector 1"), lwd = 1.5) + 
  geom_line(aes(y = o2_added_2, colour = "Injector 2"), lwd = 1.5) + 
  scale_colour_manual("", breaks = c("Injector 1", "Injector 2"),
                      values = c("Injector 1" = "red", "Injector 2" = "blue")) + 
  scale_y_continuous(bquote(O[2] ~ "added (kg/min)"), limits = c(0.140,0.22))  +
  theme(text = element_text(size = 20))

h1_plot

sum_added_o2_plot = ggplot(data = data, aes(x=Time)) + 
  geom_line(aes(y = o2_added_1 + o2_added_2, colour = "Injector 1"), lwd = 1.5) +
  scale_colour_manual("", breaks = c("Injector 1"),
                      values = c("Injector 1" = "purple")) + 
  scale_y_continuous(bquote(O[2] ~ "added (kg/min)"))  +
  theme(text = element_text(size = 20))

sum_added_o2_plot



c_o2_plot = ggplot(data = data, aes(x=Time)) + 
  geom_line(aes(y = oxygen_mgl_e, colour = "Edge sensor"), lwd = 1.5) + 
  geom_line(aes(y = oxygen_mgl_c, colour = "Center sensor"), lwd = 1.5) + 
  scale_colour_manual("", breaks = c("Edge sensor", "Center sensor"),
                      values = c("Edge sensor" = "red", "Center sensor" = "blue")) + 
  scale_y_continuous(bquote(O[2] ~ "concentration (mg/l)"), limits = c(7.5, 8.5)) +
  theme(text = element_text(size = 20))

c_o2_plot


average_c_o2_plot = ggplot(data = data, aes(x=Time)) + 
  geom_line(aes(y = (oxygen_mgl_e + oxygen_mgl_c)/2, colour = "Edge"), lwd = 1.5) +
  scale_colour_manual("", breaks = c("Edge"),
                      values = c("Edge" = "purple")) + 
  scale_y_continuous(bquote(O[2] ~ "concentration (mg/l)")) +
  theme(text = element_text(size = 20))

average_c_o2_plot

c_co2_plot = ggplot(data = data, aes(x = Time)) +
  geom_line(aes(y = c_co2_tank, color = "Concentration"), lwd = 1.5) + 
  scale_colour_manual("", breaks = "Concentration", values = c("Concentration" = "red")) + 
  scale_y_continuous(bquote(CO[2] ~ "concentration (mg/l)")) +
  theme(text = element_text(size = 20))

c_co2_plot

flow_plot = ggplot(data = data, aes(x=Time)) + 
  geom_line(aes(y = fresh_flow, colour = "Fresh water"), lwd = 1.5) + 
  geom_line(aes(y = mean(fresh_flow), colour = "Fresh water"), lwd = .5) +
  geom_ribbon(aes(ymin = mean(fresh_flow) - 1.96*sd(fresh_flow), 
                  ymax = mean(fresh_flow) + 1.96*sd(fresh_flow)), alpha = 0.05, fill = "blue") +
  geom_line(aes(y = recycled_flow, colour = "Recycled water"), lwd = 1.5) +
  geom_line(aes(y = mean(recycled_flow), colour = "Recycled water"), lwd = .5) + 
  geom_ribbon(aes(ymin = mean(recycled_flow) - 1.96*sd(recycled_flow), 
                  ymax = mean(recycled_flow) + 1.96*sd(recycled_flow)), alpha = 0.05, fill = "red") +
  scale_colour_manual("", breaks = c("Fresh water", "Recycled water"),
                      values = c("Fresh water" = "blue", "Recycled water" = "red")) + 
  scale_y_continuous("Water flow (l/min)") +
  theme(text = element_text(size = 20))

flow_plot

total_flow_plot = ggplot(data = data, aes(x = Time)) +
  geom_line(aes(y = total_flow, color = "Total flow"), lwd = 1.5) +
  geom_line(aes(y = mean(total_flow), color = "Total flow"), lwd = .5) +
  geom_ribbon(aes(ymin = mean(total_flow) - 1.96*sd(total_flow), 
                  ymax = mean(total_flow) + 1.96*sd(total_flow)), alpha = 0.05, fill = "purple") +
  scale_colour_manual("", breaks = "Total flow", values = c("Total flow" = "purple")) + 
  scale_y_continuous("Water flow (l/min)") +
  theme(text = element_text(size = 20))

total_flow_plot

o2_discrepancy_plot = ggplot(data = data, aes(x=Time)) + 
  geom_line(aes(y = delta_o2*1e-6), lwd = 1.5) +  
  scale_y_continuous(bquote(g[O[2]](t)~"[kg/min]"), lim = c(-2,2)) + 
  theme_bw() +
  theme(text = element_text(size = 30))


co2_discrepancy_plot = ggplot(data = data, aes(x=Time)) +  
  geom_line(aes(y = delta_co2*1e-6), lwd = 1.5) + 
  scale_y_continuous(bquote(g[CO[2]](t)~"[kg/min]"), lim = c(-2,2)) +
  theme_bw() +
  theme(text = element_text(size = 30))

ggarrange(o2_discrepancy_plot, co2_discrepancy_plot, legend = F)

difference_discrepancy_plot = ggplot(data = data, aes(x=Time)) + 
  geom_line(aes(y = delta_co2*1e-6 + delta_o2*1e-6), lwd = 1.5) +
  geom_hline(aes(yintercept = mean(delta_co2*1e-6 + delta_o2*1e-6), color = "darkgrey"), lwd = 1.5) +
  scale_colour_manual(values = c("darkgrey" = "darkgrey"), name = " ", 
                      labels = c(bquote("E["~g[O[2]](t) + g[CO[2]](t)~"]"))) +
   
  scale_y_continuous(bquote((g[O[2]] + g[CO[2]]) (t) ~ "[kg/min]"), lim = c(-3,3)) +
  theme_bw() +
  theme(text = element_text(size = 35))

(histogram_difference_discrepancy = ggplot(data = data) +
  geom_histogram(aes(x = delta_co2*1e-6 + delta_o2*1e-6, after_stat(density)), fill = "lightgrey", bins = 15) + 
  geom_vline(aes(xintercept = mean(delta_co2*1e-6 + delta_o2*1e-6)), color = "darkgrey", lwd = 1.5) +
  stat_function(fun = dnorm, args = list(mean = mean(delta_co2*1e-6 + delta_o2*1e-6), 
                                         sd = sd(delta_co2*1e-6 + delta_o2*1e-6)), lwd = 1.5) +
  
  theme_bw() +
  scale_x_continuous(lim = c(-3.5, 3.5)) +
  xlab(bquote((g[O[2]] + g[CO[2]])(t) ~ "[kg/min]")) +
  theme(text = element_text(size = 35))
)

ggarrange(difference_discrepancy_plot, histogram_difference_discrepancy, common.legend = T)

mean(delta_co2*1e-6 + delta_o2*1e-6)

mean(data$delta_co2)/mean(data$delta_o2) # 1.66, diskuter usikkerheter

0.48/0.45
############################### Posterior plots ################################

#### Model 0: No priors fitted ####

## Discrepancy

(posterior_discrepancy_o2_mod_0 = ggplot(data = data_mod.o2.0, aes(x = Time)) +
  geom_line(aes(y = mean), lwd = 1.5) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.05, fill = "black") +
  geom_line(aes(y = lower), color = "black", lwd = .5)+
  geom_line(aes(y =  upper), color = "black", lwd = .5) +
  scale_y_continuous(bquote(delta[O[2]] (t) ~"|"~ y[O[2]](t)~"[kg/min]"), lim = c(-12000, 12000)) +
  theme_bw() +
  theme(text = element_text(size = 30)))  

(posterior_discrepancy_co2_mod_0 = ggplot(data = data_mod.co2.0, aes(x = Time)) +
  geom_line(aes(y = mean), lwd = 1.5) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.05, fill = "black") +
  geom_line(aes(y = lower), color = "black", lwd = .5)+
  geom_line(aes(y =  upper), color = "black", lwd = .5) +
  scale_y_continuous(bquote(delta[CO[2]] (t) ~"|"~ y[CO[2]](t)~"[kg/min]"), lim = c(-12000, 12000)) +
  theme_bw() +
  theme(text = element_text(size = 30)))


ggarrange(posterior_discrepancy_o2_mod_0, posterior_discrepancy_co2_mod_0)

(posterior_discrepancy_compared_o2_mod_0 = ggplot(data = data_mod.o2.0, aes(x = Time)) +
  geom_line(aes(y = mean), lwd = 1.5, color = "black") + 
  geom_line(aes(y = data$delta_o2*1e-6, colour = "g"), lwd = 1.5) +
  scale_y_continuous(bquote("E["~delta[O[2]] (t) ~"|"~ y[O[2]](t) ~"]"~"[kg/min]"), lim = c(-5,90)) +
  scale_color_manual(name = "", values = c("g" = "grey"), labels = c("g" = bquote(g[O[2]] (t)))) +
  theme_bw() +
  theme(text = element_text(size = 40), legend.position = "top"))

(posterior_discrepancy_compared_co2_mod_0 = ggplot(data = data_mod.co2.0, aes(x = Time)) +
    geom_line(aes(y = mean), lwd = 1.5, color = "black") + 
    geom_line(aes(y = data$delta_co2*1e-6, color = "g"), lwd = 1.5) +
    scale_y_continuous(bquote("E["~delta[CO[2]] (t) ~"|"~ y[CO[2]](t) ~"]"~"[kg/min]"), lim = c(-5,90)) +
    theme_bw() + 
    scale_color_manual(name = "", values = c("g" = "grey"), labels = c("g" = bquote(g[CO[2]] (t)))) +
    theme(text = element_text(size = 40), legend.position = "top"))

ggarrange(posterior_discrepancy_compared_o2_mod_0, posterior_discrepancy_compared_co2_mod_0)

(posterior_discrepancy_compared_co2_mod_0 = ggplot(data = data_mod.co2.0, aes(x = Time)) +
    geom_line(aes(y = mean), lwd = 1.5, color = "black") + 
    geom_line(aes(y = data$delta_co2*1e-6, color = "g(t)"), lwd = 1.5) +
    scale_y_continuous(bquote("E["~delta[CO[2]] (t) ~"|"~ y[CO[2]](t) ~"]"), lim = c(-5,90)) +
    theme_bw() + 
    scale_color_manual(name = "", values = c("g(t)" = "darkgrey")) +
    theme(text = element_text(size = 30)))


(posterior_discrepancy_diff_mod_0 = ggplot(data = difference.mod.0, aes(x = Time)) +
  geom_line(aes(y = mean), lwd = 1.5) + 
  #geom_line(aes(y = (delta_o2 + delta_co2)*1e-6), lwd = 1.5, color = "grey") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.05, fill = "black") +
  geom_line(aes(y = lower), color = "black", lwd = .5)+
  geom_line(aes(y = upper), color = "black", lwd = .5) +
  scale_y_continuous(bquote((delta[O[2]] + delta[CO[2]])(t)~"|"~y(t)~"[kg/min]")) +
  theme_bw() +
  theme(text = element_text(size = 40)))


(histogram_difference_discrepancy_0 = ggplot(data = data) +
    #geom_histogram(aes(x = delta_co2*1e-6 + delta_o2*1e-6, after_stat(density), color = "g(t)"), fill = "lightgrey", bins = 10) + 
    #scale_color_manual(name = "", values = c("g(t)" = "lightgrey")) +
    stat_function(fun = dnorm, args = list(mean = mean(difference.mod.0$mean), sd = sqrt(mean(difference.mod.0$var))), lwd = 1.5) +
    #geom_vline(aes(xintercept = mean(difference.mod.0$mean), colour = "delta"), lwd = 1.5) + 
    geom_vline(aes(xintercept = mean(delta_o2 + delta_co2)*1e-6, colour = "g"), lwd = 1.5) + 
    scale_colour_manual(values = c("g" = "grey"), name = " ", 
                        labels = c("g" = bquote("E["~g[O[2]](t) + g[CO[2]](t)~"|"~y(t)~"]"))) +
    theme_bw() +
    scale_x_continuous(limits = c(-25000,25000)) +
    xlab(bquote((delta[O[2]] + delta[CO[2]])(t)~"|"~y(t)~"[kg/min]")) +
    ylab("density") +
    theme(text = element_text(size = 40), legend.position = "top"))

ggarrange(posterior_discrepancy_diff_mod_0, histogram_difference_discrepancy_0, common.legend = T)

(posterior_discrepancy_compared_co2_mod_0 = ggplot(data = difference.mod.0, aes(x = Time)) +
    geom_line(aes(y = mean), lwd = 1.5, color = "black") + 
    geom_line(aes(y = data$delta_co2*1e-6 + data$delta_o2*1e-6, color = "g"), lwd = 1.5) +
    scale_y_continuous(bquote("E["~delta[O[2]] (t) + delta[CO[2]] (t) ~"|"~ y[CO[2]](t) ~"]")) +
    theme_bw() + 
    scale_color_manual(values = c("g" = "grey"), name = "", 
                       labels = c("g" = bquote("E["~g[O[2]](t) + g[CO[2]](t)~"|"~y(t)~"]"))) +
    theme(text = element_text(size = 40), legend.position = "top"))


#### Model 1: only gaussian priors ####



(posterior_discrepancy_o2 = ggplot(data = data_mod.o2.1, aes(x = Time)) +
    geom_line(aes(y = mean, color = "1"), lwd = 1.5) + 
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.05, fill = "red") +
    geom_line(aes(y = lower), color = "red", lwd = .5)+
    geom_line(aes(y =  upper), color = "red", lwd = .5) +
    geom_line(aes(y = data_mod.o2.2$mean, color = "2"), lwd = 1.5) + 
    geom_ribbon(aes(ymin = data_mod.o2.2$lower, ymax = data_mod.o2.2$upper), alpha = 0.05, fill = "blue") +
    geom_line(aes(y = data_mod.o2.2$lower), color = "blue", lwd = .5)+
    geom_line(aes(y = data_mod.o2.2$upper), color = "blue", lwd = .5) +
    geom_line(aes(y = data_mod.o2.3$mean, color = "3"), lwd = 1.5) + 
    geom_ribbon(aes(ymin = data_mod.o2.3$lower, ymax = data_mod.o2.3$upper), alpha = 0.05, fill = "green") +
    geom_line(aes(y = data_mod.o2.3$lower), color = "green", lwd = .5)+
    geom_line(aes(y = data_mod.o2.3$upper), color = "green", lwd = .5) +
    scale_y_continuous(bquote(delta[O[2]] (t) ~"|"~ y[O[2]](t))) +
    scale_color_manual(values = c("1" = "red", "2" = "blue", "3" = "green"), name = "", labels = c("1" = "Model 1", "2" = "Model 2", "3" = "Model 3")) +
    theme_bw() +
    theme(text = element_text(size = 30)))

(posterior_discrepancy_o2_1 = ggplot(data = data_mod.o2.1, aes(x = Time)) +
    geom_line(aes(y = mean, color = "1"), lwd = 1.5) + 
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.05, fill = "red") +
    geom_line(aes(y = lower), color = "red", lwd = .5)+
    geom_line(aes(y =  upper), color = "red", lwd = .5) +
    scale_y_continuous(bquote(delta[O[2]] (t) ~"|"~ y[O[2]](t)~"[kg/min]"), lim = c(-0.43, -0.34)) +
    scale_color_manual(values = c("1" = "red"), name = "", labels = c("1" = "Model 1")) +
    theme_bw() +
    theme(text = element_text(size = 40), legend.position = "top"))

(posterior_discrepancy_o2_2 = ggplot(data = data_mod.o2.2, aes(x = Time)) +
    geom_line(aes(y = mean, color = "1"), lwd = 1.5) + 
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.05, fill = "blue") +
    geom_line(aes(y = lower), color = "blue", lwd = .5)+
    geom_line(aes(y =  upper), color = "blue", lwd = .5) +
    scale_y_continuous(bquote(delta[O[2]] (t) ~"|"~ y[O[2]](t)~"[kg/min]"), lim = c(-0.43, -0.34)) +
    scale_color_manual(values = c("1" = "blue"), name = "", labels = c("1" = "Model 2")) +
    theme_bw() +
    theme(text = element_text(size = 40), legend.position = "top"))

(posterior_discrepancy_o2_3 = ggplot(data = data_mod.o2.3, aes(x = Time)) +
    geom_line(aes(y = mean, color = "1"), lwd = 1.5) + 
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.05, fill = "green") +
    geom_line(aes(y = lower), color = "green", lwd = .5)+
    geom_line(aes(y =  upper), color = "green", lwd = .5) +
    scale_y_continuous(bquote(delta[O[2]] (t) ~"|"~ y[O[2]](t)~"[kg/min]"), lim = c(-0.43, -0.34)) +
    scale_color_manual(values = c("1" = "green"), name = "", labels = c("1" = "Model 3")) +
    theme_bw() +
    theme(text = element_text(size = 40), legend.position = "top"))

ggarrange(posterior_discrepancy_o2_1, posterior_discrepancy_o2_2, posterior_discrepancy_o2_3, ncol = 3)


(posterior_discrepancy_co2_1 = ggplot(data = data_mod.co2.1, aes(x = Time)) +
    geom_line(aes(y = mean, color = "1"), lwd = 1.5) + 
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.05, fill = "red") +
    geom_line(aes(y = lower), color = "red", lwd = .5)+
    geom_line(aes(y =  upper), color = "red", lwd = .5) +
    scale_y_continuous(bquote(delta[CO[2]] (t) ~"|"~ y[CO[2]](t)~"[kg/min]"), lim = c(0.10,0.32)) +
    scale_color_manual(values = c("1" = "red"), name = "", labels = c("1" = "Model 1")) +
    theme_bw() +
    theme(text = element_text(size = 40), legend.position = "top"))

(posterior_discrepancy_co2_2 = ggplot(data = data_mod.co2.2, aes(x = Time)) +
    geom_line(aes(y = mean, color = "1"), lwd = 1.5) + 
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.05, fill = "blue") +
    geom_line(aes(y = lower), color = "blue", lwd = .5)+
    geom_line(aes(y =  upper), color = "blue", lwd = .5) +
    scale_y_continuous(bquote(delta[CO[2]] (t) ~"|"~ y[CO[2]](t)~"[kg/min]"), lim = c(0.10,0.32)) +
    scale_color_manual(values = c("1" = "blue"), name = "", labels = c("1" = "Model 2")) +
    theme_bw() +
    theme(text = element_text(size = 40), legend.position = "top"))

(posterior_discrepancy_co2_3 = ggplot(data = data_mod.co2.3, aes(x = Time)) +
    geom_line(aes(y = mean, color = "1"), lwd = 1.5) + 
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.05, fill = "green") +
    geom_line(aes(y = lower), color = "green", lwd = .5)+
    geom_line(aes(y =  upper), color = "green", lwd = .5) +
    geom_ribbon(aes(ymin = data_mod.co2.2$lower, ymax = data_mod.co2.2$upper), alpha = 0.05, fill = "green") +
    geom_line(aes(y = data_mod.co2.2$lower), color = "green", lwd = .5)+
    geom_line(aes(y =  data_mod.co2.2$upper), color = "green", lwd = .5) +
    scale_y_continuous(bquote(delta[CO[2]] (t) ~"|"~ y[CO[2]](t)~"[kg/min]"), lim = c(0.10,0.32)) +
    scale_color_manual(values = c("1" = "green"), name = "", labels = c("1" = "Model 3")) +
    theme_bw() +
    theme(text = element_text(size = 40), legend.position = "top"))

ggarrange(posterior_discrepancy_co2_1, posterior_discrepancy_co2_2, posterior_discrepancy_co2_3, ncol = 3)



(posterior_discrepancy_o2_mod_1 = ggplot(data = data_mod.o2.1, aes(x = Time)) +
    geom_line(aes(y =data$delta_o2*1e-6, color = "g"), lwd = 1.5) +
    geom_line(aes(y = mean, color = "1"), lwd = 1.5) +  
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.05, fill = "red") +
    geom_line(aes(y = lower), color = "red", lwd = .5)+
    geom_line(aes(y =  upper), color = "red", lwd = .5) +
    scale_y_continuous(bquote(delta[O[2]] (t) ~"|"~ y[O[2]](t)~"[kg/min]"), lim = c(-2.2, 2)) +
    scale_color_manual(values = c("1" = "red", "g" = "grey"), name = "", labels = c("1" = "Model 1", "g" = bquote(g[O[2]](t)))) +
    theme_bw() +
    theme(text = element_text(size = 40), legend.position = "top"))


(posterior_discrepancy_co2_mod_1 = ggplot(data = data_mod.co2.1, aes(x = Time)) +
    geom_line(aes(y =data$delta_co2*1e-6, color = "g"), lwd = 1.5) +
    geom_line(aes(y = mean, color = "1"), lwd = 1.5) +  
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.05, fill = "red") +
    geom_line(aes(y = lower), color = "red", lwd = .5)+
    geom_line(aes(y =  upper), color = "red", lwd = .5) +
    scale_y_continuous(bquote(delta[CO[2]] (t) ~"|"~ y[O[2]](t)~"[kg/min]"), lim = c(-2.2, 2)) +
    scale_color_manual(values = c("1" = "red", "g" = "grey"), name = "", labels = c("1" = "Model 1", "g" = bquote(g[CO[2]](t)))) +
    theme_bw() +
    theme(text = element_text(size = 40), legend.position = "top"))


ggarrange(posterior_discrepancy_o2_mod_1, posterior_discrepancy_co2_mod_1)


(posterior_discrepancy_o2_mod_2 = ggplot(data = data_mod.o2.2, aes(x = Time)) +
    geom_line(aes(y =data$delta_o2*1e-6, color = "g"), lwd = 1.5) +
    geom_line(aes(y = mean, color = "1"), lwd = 1.5) +  
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.05, fill = "blue") +
    geom_line(aes(y = lower), color = "blue", lwd = .5)+
    geom_line(aes(y =  upper), color = "blue", lwd = .5) +
    scale_y_continuous(bquote(delta[O[2]] (t) ~"|"~ y[O[2]](t)~"[kg/min]"), lim = c(-2.2, 2)) +
    scale_color_manual(values = c("1" = "blue", "g" = "grey"), name = "", labels = c("1" = "Model 2", "g" = bquote(g[O[2]](t)))) +
    theme_bw() +
    theme(text = element_text(size = 40), legend.position = "top"))


(posterior_discrepancy_co2_mod_2 = ggplot(data = data_mod.co2.2, aes(x = Time)) +
    geom_line(aes(y =data$delta_co2*1e-6, color = "g"), lwd = 1.5) +
    geom_line(aes(y = mean, color = "1"), lwd = 1.5) +  
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.05, fill = "blue") +
    geom_line(aes(y = lower), color = "blue", lwd = .5)+
    geom_line(aes(y =  upper), color = "blue", lwd = .5) +
    scale_y_continuous(bquote(delta[CO[2]] (t) ~"|"~ y[O[2]](t)~"[kg/min]"), lim = c(-2.2, 2)) +
    scale_color_manual(values = c("1" = "blue", "g" = "grey"), name = "", labels = c("1" = "Model 2", "g" = bquote(g[CO[2]](t)))) +
    theme_bw() +
    theme(text = element_text(size = 40), legend.position = "top"))


ggarrange(posterior_discrepancy_o2_mod_2, posterior_discrepancy_co2_mod_2)

(posterior_discrepancy_o2_mod_3 = ggplot(data = data_mod.o2.3, aes(x = Time)) +
    geom_line(aes(y =data$delta_o2*1e-6, color = "g"), lwd = 1.5) +
    geom_line(aes(y = mean, color = "1"), lwd = 1.5) +  
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.05, fill = "green") +
    geom_line(aes(y = lower), color = "green", lwd = .5)+
    geom_line(aes(y =  upper), color = "green", lwd = .5) +
    scale_y_continuous(bquote(delta[O[2]] (t) ~"|"~ y[O[2]](t)~"[kg/min]"), lim = c(-2.2, 2)) +
    scale_color_manual(values = c("1" = "green", "g" = "grey"), name = "", labels = c("1" = "Model 3", "g" = bquote(g[O[2]](t)))) +
    theme_bw() +
    theme(text = element_text(size = 40), legend.position = "top"))


(posterior_discrepancy_co2_mod_3 = ggplot(data = data_mod.co2.3, aes(x = Time)) +
    geom_line(aes(y =data$delta_co2*1e-6, color = "g"), lwd = 1.5) +
    geom_line(aes(y = mean, color = "1"), lwd = 1.5) +  
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.05, fill = "green") +
    geom_line(aes(y = lower), color = "green", lwd = .5)+
    geom_line(aes(y =  upper), color = "green", lwd = .5) +
    scale_y_continuous(bquote(delta[CO[2]] (t) ~"|"~ y[O[2]](t)~"[kg/min]"), lim = c(-2.2, 2)) +
    scale_color_manual(values = c("1" = "green", "g" = "grey"), name = "", labels = c("1" = "Model 3", "g" = bquote(g[CO[2]](t)))) +
    theme_bw() +
    theme(text = element_text(size = 40), legend.position = "top"))


ggarrange(posterior_discrepancy_o2_mod_3, posterior_discrepancy_co2_mod_3)



(posterior_discrepancy_diff_mod_1 = ggplot(data = difference.mod.1, aes(x = Time)) +
    geom_line(aes(y = mean, col = "1"), lwd = 1.5) + 
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.05, fill = "red") +
    geom_line(aes(y = lower), color = "red", lwd = .5)+
    geom_line(aes(y = upper), color = "red", lwd = .5) +
    scale_y_continuous(bquote((delta[O[2]] + delta[CO[2]])(t)~"|"~y(t)~"[kg/min]"), lim = c(-0.28, -0.07)) +
    scale_color_manual(values = c("1" = "red"), name = "", labels = c("1" = "Model 1")) +
    theme_bw() +
    theme(text = element_text(size = 40), legend.position = "top"))

(posterior_discrepancy_diff_mod_2 = ggplot(data = difference.mod.2, aes(x = Time)) +
    geom_line(aes(y = mean, col = "1"), lwd = 1.5) + 
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.05, fill = "blue") +
    geom_line(aes(y = lower), color = "blue", lwd = .5)+
    geom_line(aes(y = upper), color = "blue", lwd = .5) +
    scale_y_continuous(bquote((delta[O[2]] + delta[CO[2]])(t)~"|"~y(t)~"[kg/min]"), lim = c(-0.28, -0.07)) +
    scale_color_manual(values = c("1" = "blue"), name = "", labels = c("1" = "Model 2")) +
    theme_bw() +
    theme(text = element_text(size = 40), legend.position = "top"))

(posterior_discrepancy_diff_mod_3 = ggplot(data = difference.mod.3, aes(x = Time)) +
    geom_line(aes(y = mean, col = "1"), lwd = 1.5) + 
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.05, fill = "green") +
    geom_line(aes(y = lower), color = "green", lwd = .5)+
    geom_line(aes(y = upper), color = "green", lwd = .5) +
    scale_y_continuous(bquote((delta[O[2]] + delta[CO[2]])(t)~"|"~y(t)~"[kg/min]"), lim = c(-0.28, -0.07)) +
    scale_color_manual(values = c("1" = "green"), name = "", labels = c("1" = "Model 3")) +
    theme_bw() +
    theme(text = element_text(size = 40), legend.position = "top"))

ggarrange(posterior_discrepancy_diff_mod_1, posterior_discrepancy_diff_mod_2, posterior_discrepancy_diff_mod_3, ncol = 3)




(histogram_difference_discrepancy_1 = ggplot(data = data) +
    stat_function(fun = dnorm, args = list(mean = mean(difference.mod.1$mean), sd = sqrt(mean(difference.mod.1$var))), aes(col = "1"), lwd = 1.5) +
    stat_function(fun = dnorm, args = list(mean = mean(difference.mod.2$mean), sd = sqrt(mean(difference.mod.2$var))), aes(col = "2"), lwd = 1.5) +
    stat_function(fun = dnorm, args = list(mean = mean(difference.mod.3$mean), sd = sqrt(mean(difference.mod.3$var))), aes(col = "3"), lwd = 1.5) +
    geom_vline(aes(xintercept = mean(delta_o2*1e-6 + delta_co2*1e-6), col = "g"), lwd = 1.5) +
    theme_bw() +
    scale_x_continuous(bquote((delta[CO[2]] + delta[O[2]])(t)~"|"~y(t)~"[kg/min]"), lim = c(-0.25, -0.05)) + 
    scale_color_manual(values = c("1" = "red", "2" = "blue", "3" = "green", "g" = "grey"),
                       labels = c("1" = "Model 1", "2" = "Model 2", "3" = "Model 3", "g" = bquote("E["~g[O[2]](t)+g[CO[2]](t)~"]")),
                       name = "") +
    ylab("Density") +
    theme(text = element_text(size = 40), legend.position = "top"))

ggarrange(posterior_discrepancy_diff_mod_1, histogram_difference_discrepancy_1)

(posterior_discrepancy_compared_co2_mod_0 = ggplot(data = difference.mod.0, aes(x = Time)) +
    geom_line(aes(y = mean*1e-6), lwd = 1.5, color = "black") + 
    geom_line(aes(y = data$delta_co2*1e-6 + data$delta_o2*1e-6, color = "g(t)"), lwd = 1.5) +
    scale_y_continuous(bquote("E["~delta[O[2]] (t) + delta[CO[2]] (t) ~"|"~ y[CO[2]](t) ~"]")) +
    theme_bw() + 
    scale_color_manual(name = "", values = c("g(t)" = "darkgrey")) +
    theme(text = element_text(size = 30), legend.position = "top"))


#### Model 2: PC Priors ###
(posterior_discrepancy_o2_mod_2 = ggplot(data = data_mod.o2.2, aes(x = Time)) +
    geom_line(aes(y = data$delta_o2*1e-6, color = "g(t)"), lwd = 1.5) +
    scale_color_manual(name = "", values = c("g(t)" = "grey")) +
    geom_line(aes(y = mean*1e-6), lwd = 1.5) + 
    geom_ribbon(aes(ymin = lower*1e-6, ymax = upper*1e-6), alpha = 0.05, fill = "black") +
    geom_line(aes(y = lower*1e-6), color = "black", lwd = .5)+
    geom_line(aes(y =  upper*1e-6), color = "black", lwd = .5) +
    scale_y_continuous(bquote(delta[O[2]] (t) ~"|"~ y[O[2]](t)), lim = c(-2,2)) +
    theme_bw() +
    theme(text = element_text(size = 30)))

(posterior_discrepancy_co2_mod_2 = ggplot(data = data_mod.co2.2, aes(x = Time)) +
    geom_line(aes(y = data$delta_co2*1e-6), color = "grey", lwd = 1.5) +
    geom_line(aes(y = mean*1e-6), lwd = 1.5) + 
    geom_ribbon(aes(ymin = lower*1e-6, ymax = upper*1e-6), alpha = 0.05, fill = "black") +
    geom_line(aes(y = lower*1e-6), color = "black", lwd = .5)+
    geom_line(aes(y =  upper*1e-6), color = "black", lwd = .5) +
    scale_y_continuous(bquote(delta[CO[2]] (t) ~"|"~ y[CO[2]](t)), lim = c(-2,2)) +
    theme_bw() +
    theme(text = element_text(size = 30)))

ggarrange(posterior_discrepancy_o2_mod_2, posterior_discrepancy_co2_mod_2, common.legend = T)

#### Model 3: Updated priors ####
(posterior_discrepancy_o2_mod_3 = ggplot(data = data_mod.o2.3, aes(x = Time)) +
   geom_line(aes(y = data$delta_o2*1e-6, color = "g(t)"), lwd = 1.5) +
   scale_color_manual(name = "", values = c("g(t)" = "grey")) +
   geom_line(aes(y = mean*1e-6), lwd = 1.5) + 
   geom_ribbon(aes(ymin = lower*1e-6, ymax = upper*1e-6), alpha = 0.05, fill = "black") +
   geom_line(aes(y = lower*1e-6), color = "black", lwd = .5)+
   geom_line(aes(y =  upper*1e-6), color = "black", lwd = .5) +
   scale_y_continuous(bquote(delta[O[2]] (t) ~"|"~ y[O[2]](t)), lim = c(-2,2)) +
   theme_bw() +
   theme(text = element_text(size = 30)))

(posterior_discrepancy_co2_mod_3 = ggplot(data = data_mod.co2.3, aes(x = Time)) +
    geom_line(aes(y = data$delta_co2*1e-6), color = "grey", lwd = 1.5) +
    geom_line(aes(y = mean*1e-6), lwd = 1.5) + 
    geom_ribbon(aes(ymin = lower*1e-6, ymax = upper*1e-6), alpha = 0.05, fill = "black") +
    geom_line(aes(y = lower*1e-6), color = "black", lwd = .5)+
    geom_line(aes(y =  upper*1e-6), color = "black", lwd = .5) +
    scale_y_continuous(bquote(delta[CO[2]] (t) ~"|"~ y[CO[2]](t)), lim = c(-2,2)) +
    theme_bw() +
    theme(text = element_text(size = 30)))

ggarrange(posterior_discrepancy_o2_mod_3, posterior_discrepancy_co2_mod_3, common.legend = T)




posterior_discrepancy_co2_mod_0 = ggplot(data = data_mod.co2.0, aes(x = Time)) +
  geom_line(aes(y = mean*1e-6), lwd = 1.5) + 
  geom_ribbon(aes(ymin = lower*1e-6, ymax = upper*1e-6), alpha = 0.05, fill = "black") +
  geom_line(aes(y = lower*1e-6), color = "black", lwd = .5)+
  geom_line(aes(y =  upper*1e-6), color = "black", lwd = .5) +
  scale_y_continuous(bquote(delta[CO[2]] (t) ~"|"~ y[CO[2]](t)), lim = c(-60000, 60000)) +
  theme_bw() +
  theme(text = element_text(size = 30))

ggarrange(posterior_discrepancy_o2_mod_0, posterior_discrepancy_co2_mod_0)


posterior_discrepancy_co2_mod_0 = ggplot(data = data_mod.co2.0, aes(x = Time)) +
  geom_line(aes(y = mean*1e-6)) + 
  geom_line(aes(y=data$delta_co2*1e-6)) +
  geom_ribbon(aes(ymin = lower*1e-6, ymax = upper*1e-6), alpha = 0.05) +
  ylab("Value")

posterior_discrepancy_co2_mod_0


posterior_discrepancy_o2_mod_2 = ggplot(data = data_mod.o2.2, aes(x = Time)) +
  geom_line(aes(y = mean), colour = "red") + 
  geom_line(aes(y = data$delta_o2)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.05, fill = "red") +
  ylab("Value")

posterior_discrepancy_o2_mod_2

posterior_discrepancy_co2_mod_2 = ggplot(data = data_mod.co2.2, aes(x = Time)) +
  geom_line(aes(y = mean), colour = "red") + 
  geom_line(aes(y = data$delta_co2)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.05, fill = "red") +
  ylab("Value")

posterior_discrepancy_co2_mod_2



################################################################################

# Plotting posterior fixed effect coefficients #

################################################################################

# linear regression

(posterior_b0_o2 = ggplot() +
  geom_line(aes(x = c(0,inla.model.o2$marginals.fixed[[1]][,1]), 
                y = c(0,inla.model.o2$marginals.fixed[[1]][,2])), lwd = 1.5) +
  #geom_line(aes(x = c(0,inla.model.o2.0$marginals.fixed[[1]][,1]), 
  #              y = c(0,inla.model.o2.0$marginals.fixed[[1]][,2])), lwd = 1.5) +
  geom_vline(aes(xintercept = constants$b0_o2, colour = "grey"), lwd = 1.5) +
  scale_colour_manual(values = c("grey" = "grey"), name = " ", 
                      labels = c(bquote("E["~beta[0]^{O[2]}~"]"))) +
  scale_x_continuous(bquote(pi(beta[0]^{O[2]}~"|"~y(t)))) +
  ylab("density") + 
  theme_bw() +
  theme(legend.position = "top", text = element_text(size = 40)))


posterior_b1_o2 = ggplot() +
  geom_line(aes(x = c(inla.model.o2$marginals.fixed[[2]][,1],1), 
                y = c(inla.model.o2$marginals.fixed[[2]][,2],0)), lwd = 1.5) +
  geom_vline(aes(xintercept = constants$b1_o2, colour = "grey"), lwd = 1.5) +
  scale_colour_manual(values = c("grey" = "grey"), name = " ", 
                      labels = c(bquote("E["~beta[1]^{O[2]}~"]"))) +
  scale_x_continuous(bquote(pi(beta[1]^{O[2]}~"|"~y(t)))) +
  ylab("density") + 
  theme_bw() +
  theme(legend.position = "top", text = element_text(size = 40))

(posterior_b2_o2 = ggplot() +
  geom_line(aes(x = c(inla.model.o2$marginals.fixed[[3]][,1]), 
                y = c(inla.model.o2$marginals.fixed[[3]][,2])), lwd = 1.5) +
  geom_vline(aes(xintercept = constants$b2_o2, colour = "grey"), lwd = 1.5) +
  scale_colour_manual(values = c("grey" = "grey"), name = " ", 
                      labels = c(bquote("E["~beta[2]^{O[2]}~"]"))) +
  scale_x_continuous(bquote(pi(beta[2]^{O[2]}~"|"~y(t)))) +
  ylab("density") + 
  theme_bw() +
  theme(axis.text.x = element_text(size=15),
        legend.position = "top", text = element_text(size = 40)))

ggarrange(posterior_b0_o2, posterior_b1_o2, posterior_b2_o2, ncol = 3)


(posterior_b0_co2 = ggplot() +
  geom_line(aes(x = c(-0.2,inla.model.co2$marginals.fixed[[1]][,1]), 
                y = c(0,inla.model.co2$marginals.fixed[[1]][,2])), lwd = 1.5) +
  geom_vline(aes(xintercept = constants$b0_co2, colour = "grey"), lwd = 1.5) +
  scale_colour_manual(values = c("grey" = "grey"), name = " ", 
                      labels = c(bquote("E["~beta[0]^{CO[2]}~"]"))) +
  scale_x_continuous(bquote(pi(beta[0]^{CO[2]}~"|"~y(t)))) +
  ylab("density") + 
  theme_bw() +
  theme(legend.position = "top", text = element_text(size = 40)))


(posterior_b1_co2 = ggplot() +
  geom_line(aes(x = c(inla.model.co2$marginals.fixed[[2]][,1],1), 
                y = c(inla.model.co2$marginals.fixed[[2]][,2],0)), lwd = 1.5) +
  geom_vline(aes(xintercept = constants$b1_co2, colour = "grey"), lwd = 1.5) +
  scale_colour_manual(values = c("grey" = "grey"), name = " ", 
                      labels = c(bquote("E["~beta[1]^{CO[2]}~"]"))) +
  scale_x_continuous(bquote(pi(beta[1]^{CO[2]}~"|"~y(t)))) +
  ylab("density") + 
  theme_bw() +
  theme(legend.position = "top", text = element_text(size = 40)))


ggarrange(posterior_b0_co2, posterior_b1_co2)

### Model 0

(posterior_b0_o2_0 = ggplot() +
    geom_line(aes(x = c(inla.model.o2.0$marginals.fixed[[1]][,1]), 
                  y = c(inla.model.o2.0$marginals.fixed[[1]][,2])), lwd = 1.5) +

    geom_vline(aes(xintercept = constants$b0_o2, colour = "grey"), lwd = 1.5) +
    scale_colour_manual(values = c("grey" = "grey"), name = " ", 
                        labels = c(bquote("E["~beta[0]^{O[2]}~"]"))) +
    scale_x_continuous(bquote(pi(beta[0]^{O[2]}~"|"~y(t)))) +
    ylab("density") + 
    theme_bw() +
    theme(legend.position = "top", text = element_text(size = 40)))


(posterior_b1_o2_0 = ggplot() +
  geom_line(aes(x = c(inla.model.o2.0$marginals.fixed[[2]][,1]), 
                y = c(inla.model.o2.0$marginals.fixed[[2]][,2])), lwd = 1.5) +
  geom_vline(aes(xintercept = constants$b1_o2, colour = "grey"), lwd = 1.5) +
  scale_colour_manual(values = c("grey" = "grey"), name = " ", 
                      labels = c(bquote("E["~beta[1]^{O[2]}~"]"))) +
  scale_x_continuous(bquote(pi(beta[1]^{O[2]}~"|"~y(t)))) +
  ylab("density") + 
  theme_bw() +
  theme(legend.position = "top", text = element_text(size = 40)))

(posterior_b2_o2_0 = ggplot() +
    geom_line(aes(x = c(inla.model.o2.0$marginals.fixed[[3]][,1]), 
                  y = c(inla.model.o2.0$marginals.fixed[[3]][,2])), lwd = 1.5) +
    geom_vline(aes(xintercept = constants$b2_o2, colour = "grey"), lwd = 1.5) +
    scale_colour_manual(values = c("grey" = "grey"), name = " ", 
                        labels = c(bquote("E["~beta[2]^{O[2]}~"]"))) +
    scale_x_continuous(bquote(pi(beta[2]^{O[2]}~"|"~y(t)))) +
    ylab("density") + 
    theme_bw() +
    theme(axis.text.x = element_text(size=20),
          legend.position = "top", text = element_text(size = 40)))

ggarrange(posterior_b0_o2_0, posterior_b1_o2_0, posterior_b2_o2_0, ncol = 3)


(posterior_b0_co2_0 = ggplot() +
    geom_line(aes(x = c(inla.model.co2.0$marginals.fixed[[1]][,1]), 
                  y = c(inla.model.co2.0$marginals.fixed[[1]][,2])), lwd = 1.5) +
    geom_vline(aes(xintercept = constants$b0_co2, colour = "grey"), lwd = 1.5) +
    scale_colour_manual(values = c("grey" = "grey"), name = " ", 
                        labels = c(bquote("E["~beta[0]^{CO[2]}~"]"))) +
    scale_x_continuous(bquote(pi(beta[0]^{CO[2]}~"|"~y(t)))) +
    ylab("density") + 
    theme_bw() +
    theme(legend.position = "top", text = element_text(size = 40)))


(posterior_b1_co2_0 = ggplot() +
    geom_line(aes(x = c(inla.model.co2.0$marginals.fixed[[2]][,1],1), 
                  y = c(inla.model.co2.0$marginals.fixed[[2]][,2],0)), lwd = 1.5) +
    geom_vline(aes(xintercept = constants$b1_co2, colour = "grey"), lwd = 1.5) +
    scale_colour_manual(values = c("grey" = "grey"), name = " ", 
                        labels = c(bquote("E["~beta[1]^{CO[2]}~"]"))) +
    scale_x_continuous(bquote(pi(beta[1]^{CO[2]}~"|"~y(t)))) +
    ylab("density") + 
    theme_bw() +
    theme(legend.position = "top", text = element_text(size = 40)))

ggarrange(posterior_b0_co2_0, posterior_b1_co2_0)


# b0, model 1,2,3

(posterior_b0_o2 = ggplot() +
  #geom_line(aes(x = c(0,inla.model.o2.0$marginals.fixed[[1]][,1]), 
  #              y = c(0,inla.model.o2.0$marginals.fixed[[1]][,2])), lwd = 1.5) +
  geom_vline(aes(xintercept = constants$b0_o2, colour = "mu"), lwd = 1.5) +
  geom_line(aes(x = c(inla.model.o2.1$marginals.fixed[[1]][,1]), 
                y = c(inla.model.o2.1$marginals.fixed[[1]][,2]), colour = "1"), lwd = 1.5) +
  geom_line(aes(x = c(inla.model.o2.2$marginals.fixed[[1]][,1]), 
                y = c(inla.model.o2.2$marginals.fixed[[1]][,2]), colour = "2"),linetype = "dashed", lwd = 1.5) +
  geom_line(aes(x = c(inla.model.o2.3$marginals.fixed[[1]][,1]), 
                y = c(inla.model.o2.3$marginals.fixed[[1]][,2]), colour = "3"), lwd = 1.5) +
  scale_colour_manual(values = c("mu" = "black", "1" = "red", "2" = "blue", "3" = "green"), name = " ", 
                      labels = c("mu" = bquote("E["~beta[0]^{O[2]}~"]"), "1" = "Model 1", "2" = "Model 2", "3" = "Model 3")) +
  scale_x_continuous(bquote(pi(beta[0]^{O[2]}~"|"~y(t)))) +
  ylab("density") + 
  theme_bw() +
  theme(legend.position = "top", text = element_text(size = 40)))


(posterior_b1_o2 = ggplot() +
    #geom_line(aes(x = c(0,inla.model.o2.0$marginals.fixed[[1]][,1]), 
    #              y = c(0,inla.model.o2.0$marginals.fixed[[1]][,2])), lwd = 1.5) +
    geom_vline(aes(xintercept = constants$b1_o2, colour = "mu"), lwd = 1.5) +
    geom_line(aes(x = c(inla.model.o2.1$marginals.fixed[[2]][,1]), 
                  y = c(inla.model.o2.1$marginals.fixed[[2]][,2]), colour = "1"), lwd = 1.5) +
    geom_line(aes(x = c(inla.model.o2.2$marginals.fixed[[2]][,1]), 
                  y = c(inla.model.o2.2$marginals.fixed[[2]][,2]), colour = "2"),linetype = "dashed", lwd = 1.5) +
    geom_line(aes(x = c(inla.model.o2.3$marginals.fixed[[2]][,1]), 
                  y = c(inla.model.o2.3$marginals.fixed[[2]][,2]), colour = "3"), lwd = 1.5) +
    scale_colour_manual(values = c("mu" = "black", "1" = "red", "2" = "blue", "3" = "green"), name = " ", 
                        labels = c("mu" = bquote("E["~beta[1]^{O[2]}~"]"), "1" = "Model 1", "2" = "Model 2", "3" = "Model 3")) +
    scale_x_continuous(bquote(pi(beta[1]^{O[2]}~"|"~y(t)))) +
    ylab("density") + 
    theme_bw() +
    theme(legend.position = "top", text = element_text(size = 40)))


(posterior_b2_o2 = ggplot() +
    #geom_line(aes(x = c(0,inla.model.o2.0$marginals.fixed[[1]][,1]), 
    #              y = c(0,inla.model.o2.0$marginals.fixed[[1]][,2])), lwd = 1.5) +
    geom_vline(aes(xintercept = constants$b2_o2, colour = "mu"), lwd = 1.5) +
    geom_line(aes(x = c(inla.model.o2.1$marginals.fixed[[3]][,1]), 
                  y = c(inla.model.o2.1$marginals.fixed[[3]][,2]), colour = "1"), lwd = 1.5) +
    geom_line(aes(x = c(inla.model.o2.2$marginals.fixed[[3]][,1]), 
                  y = c(inla.model.o2.2$marginals.fixed[[3]][,2]), colour = "2"),linetype = "longdash", lwd = 1.5) +
    geom_line(aes(x = c(inla.model.o2.3$marginals.fixed[[3]][,1]), 
                  y = c(inla.model.o2.3$marginals.fixed[[3]][,2]), colour = "3"),linetype = "dashed", lwd = 1.5) +
    scale_colour_manual(values = c("mu" = "black", "1" = "red", "2" = "blue", "3" = "green"), name = " ", 
                        labels = c("mu" = bquote("E["~beta[2]^{O[2]}~"]"), "1" = "Model 1", "2" = "Model 2", "3" = "Model 3")) +
    scale_x_continuous(bquote(pi(beta[2]^{O[2]}~"|"~y(t)))) +
    ylab("density") + 
    theme_bw() +
    theme(legend.position = "top", text = element_text(size = 40)))


(posterior_b0_co2 = ggplot() +
    #geom_line(aes(x = c(0,inla.model.o2.0$marginals.fixed[[1]][,1]), 
    #              y = c(0,inla.model.o2.0$marginals.fixed[[1]][,2])), lwd = 1.5) +
    geom_vline(aes(xintercept = constants$b0_co2, colour = "mu"), lwd = 1.5) +
    geom_line(aes(x = c(inla.model.co2.1$marginals.fixed[[1]][,1]), 
                  y = c(inla.model.co2.1$marginals.fixed[[1]][,2]), colour = "1"), lwd = 1.5) +
    geom_line(aes(x = c(inla.model.co2.2$marginals.fixed[[1]][,1]), 
                  y = c(inla.model.co2.2$marginals.fixed[[1]][,2]), colour = "2"),linetype = "dashed", lwd = 1.5) +
    geom_line(aes(x = c(inla.model.co2.3$marginals.fixed[[1]][,1]), 
                  y = c(inla.model.co2.3$marginals.fixed[[1]][,2]), colour = "3"), lwd = 1.5) +
    scale_colour_manual(values = c("mu" = "black", "1" = "red", "2" = "blue", "3" = "green"), name = " ", 
                        labels = c("mu" = bquote("E["~beta[0]^{CO[2]}~"]"), "1" = "Model 1", "2" = "Model 2", "3" = "Model 3")) +
    scale_x_continuous(bquote(pi(beta[0]^{CO[2]}~"|"~y(t)))) +
    ylab("density") + 
    theme_bw() +
    theme(legend.position = "top", text = element_text(size = 40)))


(posterior_b1_co2 = ggplot() +
    #geom_line(aes(x = c(0,inla.model.o2.0$marginals.fixed[[1]][,1]), 
    #              y = c(0,inla.model.o2.0$marginals.fixed[[1]][,2])), lwd = 1.5) +
    geom_vline(aes(xintercept = constants$b1_co2, colour = "mu"), lwd = 1.5) +
    geom_line(aes(x = c(inla.model.co2.1$marginals.fixed[[2]][,1]), 
                  y = c(inla.model.co2.1$marginals.fixed[[2]][,2]), colour = "1"), lwd = 1.5) +
    geom_line(aes(x = c(inla.model.co2.2$marginals.fixed[[2]][,1]), 
                  y = c(inla.model.co2.2$marginals.fixed[[2]][,2]), colour = "2"),linetype = "dashed", lwd = 1.5) +
    geom_line(aes(x = c(inla.model.co2.3$marginals.fixed[[2]][,1]), 
                  y = c(inla.model.co2.3$marginals.fixed[[2]][,2]), colour = "3"), lwd = 1.5) +
    scale_colour_manual(values = c("mu" = "black", "1" = "red", "2" = "blue", "3" = "green"), name = " ", 
                        labels = c("mu" = bquote("E["~beta[1]^{CO[2]}~"]"), "1" = "Model 1", "2" = "Model 2", "3" = "Model 3")) +
    scale_x_continuous(bquote(pi(beta[1]^{CO[2]}~"|"~y(t)))) +
    ylab("density") + 
    theme_bw() +
    theme(legend.position = "top", text = element_text(size = 40)))


posterior_b1_o2 = ggplot() +
  geom_line(aes(x = c(inla.model.o2$marginals.fixed[[2]][,1],1), 
                y = c(inla.model.o2$marginals.fixed[[2]][,2],0)), lwd = 1.5) +
  geom_vline(aes(xintercept = constants$b1_o2, colour = "grey"), lwd = 1.5) +
  scale_colour_manual(values = c("grey" = "grey"), name = " ", 
                      labels = c(bquote("E["~beta[1]^{O[2]}~"]"))) +
  scale_x_continuous(bquote(pi(beta[1]^{O[2]}~"|"~y(t)))) +
  ylab("density") + 
  theme_bw() +
  theme(legend.position = "top", text = element_text(size = 40))

posterior_b2_o2 = ggplot() +
  geom_line(aes(x = c(inla.model.o2$marginals.fixed[[3]][,1]), 
                y = c(inla.model.o2$marginals.fixed[[3]][,2])), lwd = 1.5) +
  geom_vline(aes(xintercept = constants$b2_o2, colour = "grey"), lwd = 1.5) +
  scale_colour_manual(values = c("grey" = "grey"), name = " ", 
                      labels = c(bquote("E["~beta[2]^{O[2]}~"]"))) +
  scale_x_continuous(bquote(pi(beta[2]^{O[2]}~"|"~y(t)))) +
  ylab("density") + 
  theme_bw() +
  theme(axis.text.x = element_text(size=15),
        legend.position = "top", text = element_text(size = 40))

ggarrange(posterior_b0_o2, posterior_b1_o2, posterior_b2_o2, ncol = 3)


(posterior_b0_co2 = ggplot() +
    geom_line(aes(x = c(-0.2,inla.model.co2$marginals.fixed[[1]][,1]), 
                  y = c(0,inla.model.co2$marginals.fixed[[1]][,2])), lwd = 1.5) +
    geom_vline(aes(xintercept = constants$b0_co2, colour = "grey"), lwd = 1.5) +
    scale_colour_manual(values = c("grey" = "grey"), name = " ", 
                        labels = c(bquote("E["~beta[0]^{CO[2]}~"]"))) +
    scale_x_continuous(bquote(pi(beta[0]^{CO[2]}~"|"~y(t)))) +
    ylab("density") + 
    theme_bw() +
    theme(legend.position = "top", text = element_text(size = 40)))


(posterior_b1_co2 = ggplot() +
    geom_line(aes(x = c(inla.model.co2$marginals.fixed[[2]][,1],1), 
                  y = c(inla.model.co2$marginals.fixed[[2]][,2],0)), lwd = 1.5) +
    geom_vline(aes(xintercept = constants$b1_co2, colour = "grey"), lwd = 1.5) +
    scale_colour_manual(values = c("grey" = "grey"), name = " ", 
                        labels = c(bquote("E["~beta[1]^{CO[2]}~"]"))) +
    scale_x_continuous(bquote(pi(beta[1]^{CO[2]}~"|"~y(t)))) +
    ylab("density") + 
    theme_bw() +
    theme(legend.position = "top", text = element_text(size = 40)))


ggarrange(posterior_b0_co2, posterior_b1_co2)



###############################################################################

# Plot posterior sigma^2 for y

(posterior_sigma2_o2_lm = ggplot() + 
   geom_line(aes(x = inla.tmarginal(function(x) 1/x, inla.model.o2$marginals.hyperpar$`Precision for the Gaussian observations`)[,1],
                 y = inla.tmarginal(function(x) 1/x, inla.model.o2$marginals.hyperpar$`Precision for the Gaussian observations`)[,2],
                 color = "lm"), lwd = 1.5) + 
   scale_colour_manual(values = c("lm" = "black"), name = "",
                       labels = c("lm" = "Linear Model")) + 
   scale_x_continuous(bquote(pi(sigma[y[O[2]]]^2 ~ "|"~ y[O[2]](t)))) +
   scale_y_continuous("Density") +
   theme_bw() + theme(text = element_text(size = 40), axis.text = element_text(size = 20), legend.position = "top"))

(posterior_sigma2_co2_lm = ggplot() + 
    geom_line(aes(x = inla.tmarginal(function(x) 1/x, inla.model.co2$marginals.hyperpar$`Precision for the Gaussian observations`)[,1],
                  y = inla.tmarginal(function(x) 1/x, inla.model.co2$marginals.hyperpar$`Precision for the Gaussian observations`)[,2],
                  color = "lm"), lwd = 1.5) + 
    scale_colour_manual(values = c("lm" = "black"), name = "",
                        labels = c("lm" = "Linear Model")) + 
    scale_x_continuous(bquote(pi(sigma[y[CO[2]]]^2 ~ "|"~ y[CO[2]](t)))) +
    scale_y_continuous("Density") +
    theme_bw() + theme(text = element_text(size = 40), axis.text = element_text(size = 20), legend.position = "top"))

ggarrange(posterior_sigma2_o2_lm, posterior_sigma2_co2_lm, common.legend = T)

(posterior_sigma2_o2_0 = ggplot() + 
    geom_line(aes(x = inla.tmarginal(function(x) 1/x, inla.model.o2.0$marginals.hyperpar$`Precision for the Gaussian observations`)[,1],
                  y = inla.tmarginal(function(x) 1/x, inla.model.o2.0$marginals.hyperpar$`Precision for the Gaussian observations`)[,2],
                  color = "lm"), lwd = 1.5) + 
    scale_colour_manual(values = c("lm" = "black"), name = "",
                        labels = c("lm" = "Model 0")) + 
    scale_x_continuous(bquote(pi(sigma[y[O[2]]]^2 ~ "|"~ y[O[2]](t)))) +
    scale_y_continuous("Density") +
    theme_bw() + theme(text = element_text(size = 40), axis.text = element_text(size = 20), legend.position = "top"))

(posterior_sigma2_co2_0 = ggplot() + 
    geom_line(aes(x = inla.tmarginal(function(x) 1/x, inla.model.co2.0$marginals.hyperpar$`Precision for the Gaussian observations`)[,1],
                  y = inla.tmarginal(function(x) 1/x, inla.model.co2.0$marginals.hyperpar$`Precision for the Gaussian observations`)[,2],
                  color = "lm"), lwd = 1.5) + 
    scale_colour_manual(values = c("lm" = "black"), name = "",
                        labels = c("lm" = "Model 0")) + 
    scale_x_continuous(bquote(pi(sigma[y[CO[2]]]^2 ~ "|"~ y[CO[2]](t)))) +
    scale_y_continuous("Density") +
    theme_bw() + theme(text = element_text(size = 40), axis.text = element_text(size = 20), legend.position = "top"))

ggarrange(posterior_sigma2_o2_0, posterior_sigma2_co2_0, common.legend = T)


(posterior_sigma2_o2_123 = ggplot() + 
  geom_line(aes(x = inla.tmarginal(function(x) 1/x, inla.model.o2.1$marginals.hyperpar$`Precision for the Gaussian observations`)[,1],
                y = inla.tmarginal(function(x) 1/x, inla.model.o2.1$marginals.hyperpar$`Precision for the Gaussian observations`)[,2],
                color = "1"), lwd = 1.5) +
  geom_line(aes(x = inla.tmarginal(function(x) 1/x, inla.model.o2.2$marginals.hyperpar$`Precision for the Gaussian observations`)[,1],
                y = inla.tmarginal(function(x) 1/x, inla.model.o2.2$marginals.hyperpar$`Precision for the Gaussian observations`)[,2],
                color = "2"), lwd = 1.5) +
  geom_line(aes(x = inla.tmarginal(function(x) 1/x, inla.model.o2.3$marginals.hyperpar$`Precision for the Gaussian observations`)[,1],
                y = inla.tmarginal(function(x) 1/x, inla.model.o2.3$marginals.hyperpar$`Precision for the Gaussian observations`)[,2],
                color = "3"), lwd = 1.5, linetype ="dashed") + 
  scale_colour_manual(values = c("1" = "red", "2" = "blue", "3" = "green"), name = "",
                                labels = c("1" = "Model 1", "2" = "Model 2", "3" = "Model 3")) + 
  scale_x_continuous(bquote(pi(sigma[y[O[2]]]^2 ~ "|"~ y[O[2]](t)))) +
  scale_y_continuous("Density") +
  theme_bw() + theme(text = element_text(size = 40), axis.text = element_text(size = 20), legend.position = "top"))
  

(posterior_sigma2_co2_123 = ggplot() + 
    geom_line(aes(x = inla.tmarginal(function(x) 1/x, inla.model.co2.1$marginals.hyperpar$`Precision for the Gaussian observations`)[,1],
                  y = inla.tmarginal(function(x) 1/x, inla.model.co2.1$marginals.hyperpar$`Precision for the Gaussian observations`)[,2],
                  color = "1"), lwd = 1.5) +
    geom_line(aes(x = inla.tmarginal(function(x) 1/x, inla.model.co2.2$marginals.hyperpar$`Precision for the Gaussian observations`)[,1],
                  y = inla.tmarginal(function(x) 1/x, inla.model.co2.2$marginals.hyperpar$`Precision for the Gaussian observations`)[,2],
                  color = "2"), lwd = 1.5) +
    geom_line(aes(x = inla.tmarginal(function(x) 1/x, inla.model.co2.3$marginals.hyperpar$`Precision for the Gaussian observations`)[,1],
                  y = inla.tmarginal(function(x) 1/x, inla.model.co2.3$marginals.hyperpar$`Precision for the Gaussian observations`)[,2],
                  color = "3"), lwd = 1.5, linetype ="dashed") + 
    scale_colour_manual(values = c("1" = "red", "2" = "blue", "3" = "green"), name = "",
                        labels = c("1" = "Model 1", "2" = "Model 2", "3" = "Model 3")) + 
    scale_x_continuous(bquote(pi(sigma[y[CO[2]]]^2 ~ "|"~ y[CO[2]](t)))) +
    scale_y_continuous("Density") +
    theme_bw() + theme(text = element_text(size = 40), axis.text = element_text(size = 20), legend.position = "top"))



inla.tmarginal(function(x) 1/x, inla.model.o2$marginals.hyperpar$`Precision for the Gaussian observations`)[,1]

inla.model.co2$summary.linear.predictor[,1]

# how to plot posterior fixed parameters
plot(inla.model.o2.1$marginals.fixed[[1]], type = "l", col = "red", lwd = 1.5)

# how to plot posterior hyperparameters
plot(inla.model.o2.1$marginals.hyperpar$`Precision for the Gaussian observations`)

plot(inla.model.o2.2$marginals.fixed[[1]], type = "l", col = "red", lwd = 1.5)














