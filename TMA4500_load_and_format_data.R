rm(list = ls())

# This script is made to import and transform csv data from clarify into a suitable dataframe format

################################################################################
# TO11_TKJ_51_00_o2Avg_PV_Value – Momentanverdi tilsatt O2 kjegle 1 (kg/t)     #
# TO11_TKJ_52_00_o2Avg_PV_Value – Momentanverdi tilsatt O2 kjegle 2 (kg/t)     #
# TO11_CT_80_51_PV_Value – O2 konsentrasjon målepunkt nært senter (%)          #
# TO11_CT_80_52_PV_Value – O2 konsentrasjon målepunkt nært kant (%)            #
# TO11_FIT_10_41_PV_Value – Mengde friskt sjøvann inn i fiskekar (l/min)       #
# TO11_FIT_82_41_PV_Value – Mengde gjenbruksvann (l/min)                       #
# TO11_SoluBlu_CO2_tmp_CO2_SoluBlu – CO2 innhold i fiskekaret (mg/l)           #
# TO11_TT_80_11_PV_Value – Temperaturmåling nært senter (degC)                 #
# TO11_TT_80_12_PV_Value – Temperaturmåling nært kant (degC)                   #
################################################################################

# packages needed
library(readxl)


# defining constants dataframe
constants = data.frame(V = 4900000, pc_mgl = 0.0873) # 34%0 salinitet endre


# loading clarify data
#data = read_excel("Export_2022-08-11T05-00-00Z_2022-08-11T08-00-00Z.xlsx") #11. August
#data = read_excel("Export_2022-09-24T05-00-00Z_2022-09-24T08-00-00Z.xlsx") #24. September
data = read_excel("Export_2022-11-15T03-00-00Z_2022-11-15T06-00-00Z.xlsx") #15. November
# - foringspause fra 2 til halv 7



#head(data)

########################## Functions for data formatting ########################

# count na values
countna = function(vector){
  nacount = 0
  for(i in 1:length(vector)){
    if(is.na(vector[i])){
      nacount = nacount + 1
    }
  }
  return(nacount)
}

# get data on wanted format
clean_data_frame <- function(data){
  data = data[,c(2,7,12,17,22,27,32,37,42)]
  colnames(data) = data[1,]
  data = data[-c(1,2),]
  data = as.data.frame(sapply(data, as.numeric))
  n = length(colnames(data))
  for(i in 1:n){
    if(colnames(data)[i] == "TO11_TKJ_51_00_o2Avg_PV_Value"){colnames(data)[i] = "o2_added_1"}
    if(colnames(data)[i] == "TO11_TKJ_52_00_o2Avg_PV_Value"){colnames(data)[i] = "o2_added_2"}
    if(colnames(data)[i] == "TO11_CT_80_51_PV_Value"){colnames(data)[i] = "c_o2_center"}
    if(colnames(data)[i] == "TO11_CT_80_52_PV_Value"){colnames(data)[i] = "c_o2_rim"}
    if(colnames(data)[i] == "TO11_FIT_10_41_PV_Value"){colnames(data)[i] = "fresh_flow"}
    if(colnames(data)[i] == "TO11_FIT_83_41_PV_Value"){colnames(data)[i] = "recycled_flow"}
    if(colnames(data)[i] == "TO11_SoluBlu_CO2_tmp_CO2_SoluBlu_3"){colnames(data)[i] = "c_co2_tank"}
    if(colnames(data)[i] == "TO11_TT_80_11_PV_Value"){colnames(data)[i] = "temp_center"}
    if(colnames(data)[i] == "TO11_TT_80_12_PV_Value"){colnames(data)[i] = "temp_edge"}
    
    nacount = countna(data[,i])
    if(nacount>length(data[,i])/2){
      data[,i] = rep(0, length(data[,i]))
    }
  }
  
  data$o2_added_1 = data$o2_added_1/60 #kg/min
  data$o2_added_2 = data$o2_added_2/60 #kg/min
  
  datamean_temperature = (mean(data$temp_center) + mean(data$temp_edge))*0.5
  
  q_f = mean(data$fresh_flow)
  q_r = mean(data$recycled_flow)
  q = q_f + q_r
  
  
  data["total_flow"] = data$fresh_flow + data$recycled_flow #l/min
  data["oxygen_added"] = data$o2_added_1 + data$o2_added_2 #kg/min
  data$oxygen_added_mgmin = data$oxygen_added*1e6
  
  
  
  #data["oxygen_in_freshflow"] = constants$pc_mgl*q_f*1e-4 #kg/min 
  #data["oxygen_in_reflow"] = 0.97*constants$pc_mgl*q_r*1e-4 #kg/min
  #data["oxygen_in"] = data$oxygen_added + data$oxygen_in_freshflow + data$oxygen_in_reflow #kg/min
  data["oxygen_mgl_e"] = data$c_o2_rim*constants$pc_mgl
  data["oxygen_mgl_c"] = data$c_o2_center*constants$pc_mgl
  #data["oxygen_out"] = data$oxygen_mgl_c*q*1e-6 #kg/min
  #data["delta_oxygen"] = data$oxygen_out - data$oxygen_in #kg/min
  #data["co2_in"] = data$fresh_flow*1e-6 + data$recycled_flow*5*1e-6 #kg/min 
  #data["co2_out"] = data$c_co2_tank*data$total_flow #kg/min
  #data["delta_co2"] = data$co2_out - data$co2_in #kg/min
  
  # observe rapidly increasing trend in plot for flow, slice before step up
  data = data[1:150,]
  
  return(data)
}

mean(data$o2_added_1)
var(data$o2_added_1)

mean(data$o2_added_2)
var(data$o2_added_2)

mean(data$oxygen_mgl_c)
var(data$oxygen_mgl_c)

mean(data$oxygen_mgl_e)
var(data$oxygen_mgl_e)

mean(data$fresh_flow)
var(data$fresh_flow)

mean(data$recycled_flow)
var(data$recycled_flow)

mean(data$c_co2_tank)
var(data$c_co2_tank)

mean(data$temp_center)
var(data$temp_center)

mean(data$temp_edge)
var(data$temp_edge)

# run cleaned data
data = clean_data_frame(data)
head(data)

# add time and index vector
time <- seq(ISOdatetime(2022,11,15,3,0,0), ISOdatetime(2022,11,15,5,30,0), by=(60*1))[1:length(data[,1])]
data["Time"] = time
data["index"] = as.numeric(rownames(data))

# define dataframe of wanted constants, aligned with report
constants$q_f = mean(data$fresh_flow)
constants$q_r = mean(data$recycled_flow)
constants$q = constants$q_r + constants$q_f
constants$c_o2_f = 100*constants$pc_mgl
constants$c_o2_r = 97*constants$pc_mgl
constants$xi = 0.67
constants$b0_o2 = (constants$q_f*constants$c_o2_f + constants$q_r*constants$c_o2_r)/constants$V
constants$b1_o2 = 1 - constants$q/constants$V
constants$b2_o2 = 1/constants$V
constants$c_co2_f = 1.00
constants$b0_co2 = constants$c_co2_f*constants$q_f/constants$V
constants$b1_co2 = 1 - constants$q/constants$V + constants$xi*(constants$q_r/constants$V)
constants$b2_co2 = 1/constants$V

constants$b0_o2_prec = 1/(var((data$fresh_flow*constants$c_o2_f + 
                                 data$recycled_flow*constants$c_o2_r)
                              /constants$V))
constants$b1_o2_prec = 1/var(1 - (data$fresh_flow + data$recycled_flow)/constants$V)
constants$b2_o2_prec = 1e25
constants$b0_co2_prec = 1/var(constants$c_co2_f*data$fresh_flow/constants$V)
constants$b1_co2_prec = 1/var(1 - (data$fresh_flow + data$recycled_flow)/constants$V + 
                                constants$xi*(data$recycled_flow/constants$V))

constants$c_o2_f_sd = 0.02*constants$c_o2_f
constants$c_o2_r_sd = 0.02*constants$c_o2_r
constants$c_co2_f_sd = 0.02*constants$c_co2_f
constants$q_r_sd_1 = constants$q_r*0.02
constants$q_f_sd_1 = constants$q_f*0.02
constants$q_sd_1 = constants$q*0.02

constants$xi_sd = 0.10*constants$xi

constants$b0_o2_var_1 = ((1/constants$V)^2) * ((constants$q_r^2)*(constants$c_o2_r_sd^2) +
                                               (constants$c_o2_r^2)*(constants$q_r_sd_1^2) + 
                                                 (constants$q_f^2)*(constants$c_o2_f_sd^2) +
                                                 (constants$c_o2_f^2)*(constants$q_f_sd_1^2))
constants$b0_o2_prec_1 = 1/constants$b0_o2_var_1
constants$b1_o2_var_1 = (1/constants$V^2)*(constants$q_sd_1^2)
constants$b1_o2_prec_1 = 1/constants$b1_o2_var_1
constants$b0_co2_var_1 = (1/constants$V^2) * ((constants$q_f^2)*(constants$c_co2_f_sd^2) +
                                                (constants$c_co2_f^2)*(constants$q_f_sd_1^2))
constants$b0_co2_prec_1 = 1/constants$b0_co2_var_1
constants$b1_co2_var_1 = (1/constants$V^2) * ((constants$q_r^2)*(constants$xi_sd^2) +
                                                (constants$xi^2)*(constants$q_r_sd_1^2) +
                                                (constants$q_f_sd_1^2))
constants$b1_co2_prec_1 = 1/constants$b1_co2_var_1


h_o2 = data$oxygen_added_mgmin
y_o2 = (data$oxygen_mgl_c + data$oxygen_mgl_e)*0.5



### sjekk på om man kan simulere senter og kantmålinger for co2
y_o2_1 = data$oxygen_mgl_e
y_u_o2_1 = data$oxygen_mgl_c

mean(y_o2_1)
mean(y_u_o2_1)

relative_delta = (mean(y_o2_1) - mean(y_u_o2_1))/mean(y_o2_1)



y_co2 = data$c_co2_tank

y_u_co2_1 = y_co2 - y_co2*relative_delta
y_u_co2_1 = y_co2 - y_co2*relative_delta
mean(y_u_co2_1)
# går dette an ¨argumentere for?

mean(y_co2)
# hvor står co2 måleren???

#### vet ikke, bruker forrige tidssteg foreløbig.

# y_u(t) = y(t-1)
y_u_o2 = c(y_o2[1], y_o2)[-(length(y_o2))]
y_u_co2 = c(y_co2[1], y_co2)[-(length(y_co2))]


n = length(y_o2)
delta_o2 = (y_o2 - (constants$b0_o2) - (constants$b1_o2*y_u_o2) - (constants$b2_o2*h_o2))/constants$b2_o2
delta_co2 = (y_co2 - (constants$b0_co2) - (constants$b1_co2*y_u_co2))/constants$b2_co2

data["delta_o2"] = delta_o2
data["delta_co2"] = delta_co2

ID = data$index 


############################# defining inla data ###############################
inla.data.o2 = data.frame(y = y_o2, y_u = y_u_o2, h = h_o2, delta = delta_o2, ID = ID)
inla.data.co2 = data.frame(y = y_co2, y_u = y_u_co2, delta = delta_co2, ID = ID)


mean(inla.data.o2$h)
var(inla.data.o2$h*1e-6)

