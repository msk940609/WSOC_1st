ss_2nd_pm=read.xlsx2("Datafile/FRINED_2nd_online/SS_2nd_BAM1020_Total.csv", sheetIndex = 1, startRow = 2)
ss_2nd_pm=fread("Datafile/FRINED_2nd_online/SS_2nd_BAM1020_Total.csv")
head(ss_2nd_pm)

ss_2nd_pm
ss_2nd_pm=ss_2nd_pm %>% mutate(date2=as.POSIXct(Date, format = '%Y-%m-%d %H:%M')-36000)
ss_2nd_pm2=ss_2nd_pm %>% separate(date2, c("Date","Time"), sep = " ")
ss_2nd_pm2=ss_2nd_pm2 %>% separate(Time, c("Hour","Minute","Second"),sep = ":")
ss_2nd_pm2=subset(ss_2nd_pm2,ss_2nd_pm2$Hour!="09")
ss_2nd_pm2_m=melt(ss_2nd_pm2[,c("Date","PM10","PM2.5")], id.vars = c("Date"), na.rm = T) %>% 
  dcast(Date~variable, mean)

fwrite(ss_2nd_pm2_m, file="Datafile/FRINED_2nd_online/SS_PM_avg.csv")

ss_2nd_ion=fread("Datafile/FRINED_2nd_online/SS_2nd_AIM_Total.csv")
head(ss_2nd_ion)

ss_2nd_ion
ss_2nd_ion=ss_2nd_ion %>% mutate(date2=as.POSIXct(Date, format = '%Y-%m-%d %H:%M')-36000)
ss_2nd_ion2=ss_2nd_ion %>% separate(date2, c("Date","Time"), sep = " ")
ss_2nd_ion2=ss_2nd_ion2 %>% separate(Time, c("Hour","Minute","Second"),sep = ":")
ss_2nd_ion2=subset(ss_2nd_ion2,ss_2nd_ion2$Hour!="09")
ss_2nd_ion2_m=melt(ss_2nd_ion2[,!(colnames(ss_2nd_ion2) %in% c("Hour","Minute","Second"))], id.vars = c("Date"), na.rm = T) %>% 
  dcast(Date~variable, mean)

ss_2nd_ion2_m

fwrite(ss_2nd_ion2_m, file="Datafile/FRINED_2nd_online/SS_ion_avg.csv")


ss_2nd_gas=fread("Datafile/FRINED_2nd_online/SS_2nd_GAS_Total.csv")
head(ss_2nd_gas)

ss_2nd_gas
ss_2nd_gas=ss_2nd_gas %>% mutate(date2=as.POSIXct(Date, format = '%Y-%m-%d %H:%M')-36000)
ss_2nd_gas2=ss_2nd_gas %>% separate(date2, c("Date","Time"), sep = " ")
ss_2nd_gas2=ss_2nd_gas2 %>% separate(Time, c("Hour","Minute","Second"),sep = ":")
ss_2nd_gas2=subset(ss_2nd_gas2,ss_2nd_gas2$Hour!="09")
ss_2nd_gas2_m=melt(ss_2nd_gas2[,!(colnames(ss_2nd_gas2) %in% c("Hour","Minute","Second"))], id.vars = c("Date"), na.rm = T) %>% 
  dcast(Date~variable, mean)

ss_2nd_gas2_m

fwrite(ss_2nd_gas2_m, file="Datafile/FRINED_2nd_online/SS_gas_avg.csv")

ss_2nd_pm2_m=ss_2nd_pm2_m %>% left_join(ss_2nd_ion2_m) %>% left_join(ss_2nd_gas2_m)

fwrite(ss_2nd_pm2_m, file="Datafile/FRINED_2nd_online/SS_avg_all.csv")

