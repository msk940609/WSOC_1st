f3rd_bj_ca=fread("Datafile/FRIEND_3rd_online/Beijing/3rd campagin_CA_Beijing_Ewha.csv")
f3rd_bj_ca

f3rd_bj_os=fread("Datafile/FRIEND_3rd_online/Beijing/Beijing_3rd_organic speciation_ewha_upload.csv")
f3rd_bj_os

f3rd_bj_gas=fread("Datafile/FRIEND_3rd_online/Beijing/Beijing_gas_final.csv")
f3rd_bj_gas

f3rd_bj_chp=fread("Datafile/FRIEND_3rd_online/Beijing/chemical_Beijing_final.csv")
f3rd_bj_chp

f3rd_bj_ca
f3rd_bj_os
f3rd_bj_gas
f3rd_bj_chp

###======
f3rd_bj_chp$date2 <- as.POSIXct(f3rd_bj_chp$time)-36000
f3rd_bj_chp2=f3rd_bj_chp %>% separate(date2, c("Date","Time"), sep = " ")
f3rd_bj_chp2=f3rd_bj_chp2 %>% separate(Time, c("Hour","Minute","Second"),sep = ":")
f3rd_bj_chp2=subset(f3rd_bj_chp2,f3rd_bj_chp2$Hour!="09")
f3rd_bj_chp_m=melt(f3rd_bj_chp2[,c("Date","NO3","SO4","NH4","Cl","OC","EC")], id.vars = c("Date"), na.rm = T) %>% 
  dcast(Date~variable, mean)
f3rd_bj_chp_m

f3rd_bj_gas$date2 <- as.POSIXct(f3rd_bj_gas$time)-36000
f3rd_bj_gas2=f3rd_bj_gas %>% separate(date2, c("Date","Time"), sep = " ")
f3rd_bj_gas2=f3rd_bj_gas2 %>% separate(Time, c("Hour","Minute","Second"),sep = ":")
f3rd_bj_gas2=subset(f3rd_bj_gas2,f3rd_bj_gas2$Hour!="09")
f3rd_bj_gas_m=melt(f3rd_bj_gas2[,c("Date","SO2","NO","NOX","NO2","O3","CO","PM10","PM2.5")], id.vars = c("Date"), na.rm = T) %>% 
  dcast(Date~variable, mean)

f3rd_bj_gas$date2 <- as.POSIXct(f3rd_bj_gas$time)-36000
f3rd_bj_gas2=f3rd_bj_gas %>% separate(date2, c("Date","Time"), sep = " ")
f3rd_bj_gas2=f3rd_bj_gas2 %>% separate(Time, c("Hour","Minute","Second"),sep = ":")
f3rd_bj_gas2=subset(f3rd_bj_gas2,f3rd_bj_gas2$Hour!="09")
f3rd_bj_gas_m=melt(f3rd_bj_gas2[,c("Date","SO2","NO","NOX","NO2","O3","CO","PM10","PM2.5")], id.vars = c("Date"), na.rm = T) %>% 
  dcast(Date~variable, mean)


pm_2nd_merge=pm_2nd_merge %>% 
  mutate("Date2"=as.POSIXct(pm_2nd_merge$Date, formats="%d-%m-%yyyy %H:%M"))



f3rd_bj_ca=f3rd_bj_ca[,1:5]

f3rd_bj_ca=drop_na(f3rd_bj_ca)


