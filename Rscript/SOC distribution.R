
envi_1st=fread("Datafile/FRIEND_1st_envi_re.csv")
envi_2nd=fread("Datafile/FRIEND_2nd_envi_re.csv")
envi_3rd=fread("Datafile/FRIEND_3rd_envi.csv")

envi_1st
envi_1st$WISOC=envi_1st$OC-envi_1st$WSOC
envi_1st$WSOCbb=2.94*envi_1st$Levoglucosan
envi_1st$WSOCnbb=envi_1st$WSOC-envi_1st$WSOCbb
envi_1st$SOC=envi_1st$WSOCnbb
envi_1st$POC=envi_1st$WSOCbb+envi_1st$WISOC
envi_1st$Season="Winter"


envi_2nd
envi_2nd$WISOC=envi_2nd$OC-envi_2nd$WSOC
envi_2nd$WSOCbb=2.94*envi_2nd$levo/1000
envi_2nd$WSOCnbb=envi_2nd$WSOC-envi_2nd$WSOCbb
envi_2nd$SOC=envi_2nd$WSOCnbb
envi_2nd$POC=envi_2nd$WSOCbb+envi_2nd$WISOC
envi_2nd$Season="Summer"


envi_3rd
envi_3rd$WISOC=envi_3rd$OC-envi_3rd$WSOC
envi_3rd$WSOCbb=2.94*envi_3rd$Levo/1000
envi_3rd$WSOCnbb=envi_3rd$WSOC-envi_3rd$WSOCbb
envi_3rd$SOC=envi_3rd$WSOCnbb
envi_3rd$POC=envi_3rd$WSOCbb+envi_3rd$WISOC
envi_3rd$Season="Spring"

envi_1st_sel=envi_1st[,c("Group","No","Season","Event","Date","PM2.5","OC","POC","SOC")]
envi_2nd_sel=envi_2nd[,c("Group","No","Season","Event","Date","PM2.5","OC","POC","SOC")]
envi_3rd_sel=envi_3rd[,c("Group","No","Season","Event","Date","PM2.5","OC","POC","SOC")]
max(envi_1st_sel$No)
max(envi_2nd_sel$No)

envi_1st_sel$Nn=envi_1st_sel$No
envi_2nd_sel$Nn=envi_2nd_sel$No+31+6+5
envi_3rd_sel$Nn=envi_3rd_sel$No+31+6+5+30+9+5

envi_all_sel=rbind(envi_1st_sel,envi_2nd_sel,envi_3rd_sel)


FT_envi_trend=melt(envi_all_sel[,c("Group","Season","Nn","No","Event","Date","PM2.5","OC","POC","SOC")],
                   id.vars = c("Group","Season","Nn","No","Event","Date","PM2.5","OC"),variable.name = "type",value.name = "conc")

FT_envi_trend2=melt(FT_envi_trend,
                    id.vars = c("Group","Season","Nn","No","Event","Date","type","conc"), na.rm = F)

FT_envi_trend2

FT_envi_trend=FT_envi_trend %>% 
  mutate(date2 = as.POSIXct(FT_envi_trend$Date, format = '%Y-%m-%d %H:%M'))

FT_envi_trend

FT_envi_trend2=FT_envi_trend2 %>% 
  mutate(date2 = as.POSIXct(FT_envi_trend2$Date, format = '%Y-%m-%d %H:%M'))

FT_envi_trend2

FT_envi_trend2$val2=ifelse(FT_envi_trend2$variable=="PM2.5",FT_envi_trend2$value*0.2,FT_envi_trend2$value)
#lims <- as.POSIXct(strptime(c("2021-05-31 23:00","2021-06-30 24:00"), format = "%Y-%m-%d %H:%M"))

FT_envi_trend$type=factor(FT_envi_trend$type, levels = c("POC","SOC"))
FT_envi_trend$Group=factor(FT_envi_trend$Group,levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"))
FT_envi_trend2$Group=factor(FT_envi_trend2$Group,levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"))


#trend=====
##Ul====
FT_envi_trend
table(FT_envi_trend2$variable)

FT_envi_trend_ul=subset(FT_envi_trend,FT_envi_trend$Group=="Ulaanbaatar")
FT_envi_trend2_ul=subset(FT_envi_trend2,FT_envi_trend2$Group=="Ulaanbaatar")

FT_envi_trend2_ul$val2=ifelse(FT_envi_trend2_ul$variable=="OC",FT_envi_trend2_ul$value*1,FT_envi_trend2_ul$value*1)

#FT_envi_trend_ul$conc2=ifelse(FT_envi_trend_ul$Nn>52,FT_envi_trend_ul$conc*0.8*1.3,FT_envi_trend_ul$conc*1*1.3)
FT_envi_trend_ul$type=factor(FT_envi_trend_ul$type,levels = c("POC","SOC"))
FT_envi_trend2_ul_pm=subset(FT_envi_trend2_ul,FT_envi_trend2_ul$variable=="PM2.5")

FT_envi_trend2_ul_pm
FT_envi_trend_ul

tt_ul=na.omit(FT_envi_trend2_ul_pm,cols="value" )

pmtrend_ul=ggplot()+
  geom_rect(aes(xmin=1,xmax=34, ymax=Inf, ymin=-Inf),alpha=0.4,fill="grey50")+
  geom_rect(aes(xmin=42.5,xmax=73.5, ymax=Inf, ymin=-Inf),alpha=0.4,fill="grey50")+
  geom_rect(aes(xmin=86.5,xmax=118, ymax=Inf, ymin=-Inf),alpha=0.4,fill="grey50")+
  geom_area(data = FT_envi_trend_ul ,aes(x=Nn , y=conc,fill=type,col=type),lty=1,position = position_stack(reverse = T),size=1.2,na.rm = T)+
  geom_area(data = FT_envi_trend_ul ,aes(x=Nn , y=conc,fill=type),position = position_stack(reverse = T),size=0.75,na.rm = T)+
  #geom_line(data = FT_envi_trend2_ul_pm ,aes(x=Nn, y=value ),col="black",size=0.75, na.rm = T)+
  geom_line(data = tt_ul ,aes(x=Nn, y=value ),col="black",size=0.75, na.rm = T)+
  geom_rect(aes(xmin=31.5,xmax=42.5, ymax=Inf, ymin=-Inf),alpha=1,fill="white")+
  geom_rect(aes(xmin=72.5,xmax=86.5, ymax=Inf, ymin=-Inf),alpha=1,fill="white")+
  geom_point(data = FT_envi_trend2_ul_pm ,aes(x=Nn, y=value ),col="black", na.rm = F, size=1.5)+
  geom_line(data = FT_envi_trend2_ul_pm[c(31,33),] ,aes(x=Nn, y=value),col="black",size=0.75, na.rm = T, lty=2)+
  geom_line(data = FT_envi_trend2_ul_pm[c(153,154),] ,aes(x=Nn, y=value),col="black",size=0.75, na.rm = T, lty=2)+
  annotate(geom = "text",x=16,y=164,label="Winter (1st)",size=10)+
  annotate(geom = "text",x=58,y=164,label="Summer (2nd)",size=10)+
  annotate(geom = "text",x=103,y=164,label="Spring (3rd)",size=10)+
  scale_color_manual(values = c("#9DBCD4","blue"))+
  scale_fill_manual(values =c("#9DBCD4","#CB7723"))+
  scale_x_continuous(breaks = c(1,32,43,73,87,118),
                     labels = c(expression(atop(paste("Dec 15"^"th"),"(2020)")),
                                expression(atop(paste("Jan 15"^"th"),"(2021)")),
                                expression(atop(paste("June 1"^"st"),"(2021)")),
                                expression(atop(paste("July 1"^"st"),"(2021)")),
                                expression(atop(paste("Mar 14"^"th"),"(2022)")),
                                expression(atop(paste("Apr 14"^"th"),"(2022)"))
                     ),
                     limits = c(1,108+5+5),
                     expand = c(0.01,0.01))+
  #scale_x_datetime('',
  #                 date_breaks = '1 month',
  #                 date_labels = "%m %d", expand = c(0.008,0.008))+
  scale_y_continuous(name = expression(bold("PM"["2.5"]~"("*"\u03bcg/"*m^"3"*")")),expand = c(0.02,0.02),
                     sec.axis = sec_axis(~.*1/1.3, name = expression(bold("OC"~"("*"\u03bcg/"*m^"3"*")")),breaks = seq(0,140,30)),
                     limits = c(0,180),breaks = seq(0,160,40))+
  ggtitle("UB")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.2,0.4,0.2,0.2),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.line.y.right = element_line(colour="blue", size=1.),  
        axis.text.x = element_text(size = 20,angle = 0,vjust = 0.5, hjust = 0.5,
                                   colour = "black", face = 1,family = "Arial"),
        axis.ticks.length = unit(0.2,"cm"),
        axis.ticks.x = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.left = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.right = element_line(size = 1.5, colour = "blue"),
        axis.title.x = element_text(size = 0.1),
        axis.text.y.left = element_text(size = 20, colour = "black" , face =1,family = "Arial"),
        axis.text.y.right = element_text(size = 20, colour = "blue" , face = 1,family = "Arial"),
        axis.title.y.left = element_text(size = 24, colour = "black",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial"),
        axis.title.y.right = element_text(size = 24, colour = "blue",margin = unit(c(0.1,0.1,0.1,0.2),"cm"),family = "Arial"),
        legend.text = element_text(size = 14, colour = "black",family = "Arial",margin = unit(c(0.1,0.3,0.1,0.1),"cm"), hjust = 0.5,vjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = c(0.92,1.07))+
  xlab("")+
  guides(col="none",shape="none", fill=guide_legend(title = "", reverse = F))+
  ggsave(filename("Ulan_pm_trend_all"),height = 12, width = 32*1.2, units = "cm", dpi = 300)


##bj====
FT_envi_trend
table(FT_envi_trend2$variable)

FT_envi_trend_bj=subset(FT_envi_trend,FT_envi_trend$Group=="Beijing")
FT_envi_trend2_bj=subset(FT_envi_trend2,FT_envi_trend2$Group=="Beijing")

FT_envi_trend_bj$conc2=FT_envi_trend_bj$conc*2

#FT_envi_trend_bj$conc2=ifelse(FT_envi_trend_bj$Nn>52,FT_envi_trend_bj$conc*0.8*1.3,FT_envi_trend_bj$conc*1*1.3)
FT_envi_trend_bj$type=factor(FT_envi_trend_bj$type,levels = c("POC","SOC"))
FT_envi_trend2_bj_pm=subset(FT_envi_trend2_bj,FT_envi_trend2_bj$variable=="PM2.5")

FT_envi_trend2_bj_pm
FT_envi_trend_bj

tt_bj=na.omit(FT_envi_trend2_bj_pm,cols="value" )

max(FT_envi_trend_bj$OC, na.rm = T)

pmtrend_bj=ggplot()+
  geom_rect(aes(xmin=1,xmax=34, ymax=Inf, ymin=-Inf),alpha=0.4,fill="grey50")+
  geom_rect(aes(xmin=42.5,xmax=73.5, ymax=Inf, ymin=-Inf),alpha=0.4,fill="grey50")+
  geom_rect(aes(xmin=86.5,xmax=118, ymax=Inf, ymin=-Inf),alpha=0.4,fill="grey50")+
  geom_area(data = FT_envi_trend_bj ,aes(x=Nn , y=conc2,fill=type,col=type),lty=1,position = position_stack(reverse = T),size=1.2,na.rm = T)+
  geom_area(data = FT_envi_trend_bj ,aes(x=Nn , y=conc2,fill=type),position = position_stack(reverse = T),size=0.75,na.rm = T)+
  #geom_line(data = FT_envi_trend2_bj_pm ,aes(x=Nn, y=value ),col="black",size=0.75, na.rm = T)+
  geom_line(data = tt_bj ,aes(x=Nn, y=value ),col="black",size=0.75, na.rm = T)+
  geom_rect(aes(xmin=31.5,xmax=42.5, ymax=Inf, ymin=-Inf),alpha=1,fill="white")+
  geom_rect(aes(xmin=72.5,xmax=86.5, ymax=Inf, ymin=-Inf),alpha=1,fill="white")+
  geom_point(data = FT_envi_trend2_bj_pm ,aes(x=Nn, y=value ),col="black", na.rm = F, size=1.5)+
  geom_line(data = FT_envi_trend2_bj_pm[c(31,32),] ,aes(x=Nn, y=value),col="black",size=0.75, na.rm = T, lty=2)+
  geom_line(data = FT_envi_trend2_bj_pm[c(61,62),] ,aes(x=Nn, y=value),col="black",size=0.75, na.rm = T, lty=2)+
  annotate(geom = "text",x=16,y=117,label="Winter (1st)",size=10)+
  annotate(geom = "text",x=58,y=117,label="Summer (2nd)",size=10)+
  annotate(geom = "text",x=103,y=117,label="Spring (3rd)",size=10)+
  scale_color_manual(values = c("#9DBCD4","blue"))+
  scale_fill_manual(values =c("#9DBCD4","#CB7723"))+
  scale_x_continuous(breaks = c(1,32,43,73,87,118),
                     labels = c(expression(atop(paste("Dec 15"^"th"),"(2020)")),
                                expression(atop(paste("Jan 15"^"th"),"(2021)")),
                                expression(atop(paste("June 1"^"st"),"(2021)")),
                                expression(atop(paste("July 1"^"st"),"(2021)")),
                                expression(atop(paste("Mar 14"^"th"),"(2022)")),
                                expression(atop(paste("Apr 14"^"th"),"(2022)"))
                     ),
                     limits = c(1,108+5+5),
                     expand = c(0.01,0.01))+
  #scale_x_datetime('',
  #                 date_breaks = '1 month',
  #                 date_labels = "%m %d", expand = c(0.008,0.008))+
  scale_y_continuous(name = expression(bold("PM"["2.5"]~"("*"\u03bcg/"*m^"3"*")")),expand = c(0.02,0.02),
                     sec.axis = sec_axis(~.*1/2, name = expression(bold("OC"~"("*"\u03bcg/"*m^"3"*")")),breaks = seq(0,140,20)),
                     limits = c(0,125),breaks = seq(0,160,30))+
  ggtitle("BJ")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.2,0.4,0.2,0.2),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.line.y.right = element_line(colour="blue", size=1.),  
        axis.text.x = element_text(size = 20,angle = 0,vjust = 0.5, hjust = 0.5,
                                   colour = "black", face = 1,family = "Arial"),
        axis.ticks.length = unit(0.2,"cm"),
        axis.ticks.x = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.left = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.right = element_line(size = 1.5, colour = "blue"),
        axis.title.x = element_text(size = 0.1),
        axis.text.y.left = element_text(size = 20, colour = "black" , face =1,family = "Arial"),
        axis.text.y.right = element_text(size = 20, colour = "blue" , face = 1,family = "Arial"),
        axis.title.y.left = element_text(size = 24, colour = "black",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial"),
        axis.title.y.right = element_text(size = 24, colour = "blue",margin = unit(c(0.1,0.1,0.1,0.2),"cm"),family = "Arial"),
        legend.text = element_text(size = 14, colour = "black",family = "Arial",margin = unit(c(0.1,0.1,0.1,0.1),"cm"), hjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = c(0.92,1.07))+
  xlab("")+
  guides(col="none",shape="none", fill=guide_legend(title = "", reverse = F))+
  ggsave(filename("bj_pm_trend_all"),height = 12, width = 32*1.2, units = "cm", dpi = 300)


##ss====
FT_envi_trend
table(FT_envi_trend2$variable)

FT_envi_trend_ss=subset(FT_envi_trend,FT_envi_trend$Group=="Seosan")
FT_envi_trend2_ss=subset(FT_envi_trend2,FT_envi_trend2$Group=="Seosan")

FT_envi_trend_ss$conc2=FT_envi_trend_ss$conc*3

FT_envi_trend_ss$type=factor(FT_envi_trend_ss$type,levels = c("POC","SOC"))
FT_envi_trend2_ss_pm=subset(FT_envi_trend2_ss,FT_envi_trend2_ss$variable=="PM2.5")
FT_envi_trend2_ss_pm
FT_envi_trend_ss

tt_ss=na.omit(FT_envi_trend2_ss_pm,cols="value" )
max(FT_envi_trend_ss$OC, na.rm = T)

pmtrend_ss=ggplot()+
  geom_rect(aes(xmin=1,xmax=34, ymax=Inf, ymin=-Inf),alpha=0.4,fill="grey50")+
  geom_rect(aes(xmin=42.5,xmax=73.5, ymax=Inf, ymin=-Inf),alpha=0.4,fill="grey50")+
  geom_rect(aes(xmin=86.5,xmax=118, ymax=Inf, ymin=-Inf),alpha=0.4,fill="grey50")+
  geom_area(data = FT_envi_trend_ss ,aes(x=Nn , y=conc2,fill=type,col=type),lty=1,position = position_stack(reverse = T),size=1.2,na.rm = T)+
  geom_area(data = FT_envi_trend_ss ,aes(x=Nn , y=conc2,fill=type),position = position_stack(reverse = T),size=0.75,na.rm = T)+
  geom_line(data = tt_ss ,aes(x=Nn, y=value ),col="black",size=0.75, na.rm = T)+
  geom_rect(aes(xmin=31.5,xmax=42.5, ymax=Inf, ymin=-Inf),alpha=1,fill="white")+
  geom_rect(aes(xmin=72.5,xmax=86.5, ymax=Inf, ymin=-Inf),alpha=1,fill="white")+
  geom_point(data = FT_envi_trend2_ss_pm ,aes(x=Nn, y=value ),col="black", na.rm = F, size=1.5)+
  geom_line(data = FT_envi_trend2_ss_pm[c(31,32),] ,aes(x=Nn, y=value),col="black",size=0.75, na.rm = T, lty=2)+
  geom_line(data = FT_envi_trend2_ss_pm[c(61,62),] ,aes(x=Nn, y=value),col="black",size=0.75, na.rm = T, lty=2)+
  annotate(geom = "text",x=16,y=75,label="Winter (1st)",size=10)+
  annotate(geom = "text",x=58,y=75,label="Summer (2nd)",size=10)+
  annotate(geom = "text",x=103,y=75,label="Spring (3rd)",size=10)+
  scale_color_manual(values = c("#9DBCD4","blue"))+
  scale_fill_manual(values =c("#9DBCD4","#CB7723"))+
  scale_x_continuous(breaks = c(1,32,43,73,87,118),
                     labels = c(expression(atop(paste("Dec 15"^"th"),"(2020)")),
                                expression(atop(paste("Jan 15"^"th"),"(2021)")),
                                expression(atop(paste("June 1"^"st"),"(2021)")),
                                expression(atop(paste("July 1"^"st"),"(2021)")),
                                expression(atop(paste("Mar 14"^"th"),"(2022)")),
                                expression(atop(paste("Apr 14"^"th"),"(2022)"))
                     ),
                     limits = c(1,108+5+5),
                     expand = c(0.01,0.01))+
  #scale_x_datetime('',
  #                 date_breaks = '1 month',
  #                 date_labels = "%m %d", expand = c(0.008,0.008))+
  scale_y_continuous(name = expression(bold("PM"["2.5"]~"("*"\u03bcg/"*m^"3"*")")),expand = c(0.02,0.02),
                     sec.axis = sec_axis(~.*1/3, name = expression(bold("OC"~"("*"\u03bcg/"*m^"3"*")")),breaks = seq(0,140,5)),
                     limits = c(0,80),breaks = seq(0,160,20))+
  ggtitle("SS")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.2,0.4,0.2,0.2),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.line.y.right = element_line(colour="blue", size=1.),  
        axis.text.x = element_text(size = 20,angle = 0,vjust = 0.5, hjust = 0.5,
                                   colour = "black", face = 1,family = "Arial"),
        axis.ticks.length = unit(0.2,"cm"),
        axis.ticks.x = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.left = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.right = element_line(size = 1.5, colour = "blue"),
        axis.title.x = element_text(size = 0.1),
        axis.text.y.left = element_text(size = 20, colour = "black" , face =1,family = "Arial"),
        axis.text.y.right = element_text(size = 20, colour = "blue" , face = 1,family = "Arial"),
        axis.title.y.left = element_text(size = 24, colour = "black",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial"),
        axis.title.y.right = element_text(size = 24, colour = "blue",margin = unit(c(0.1,0.1,0.1,0.2),"cm"),family = "Arial"),
        legend.text = element_text(size = 14, colour = "black",family = "Arial",margin = unit(c(0.1,0.1,0.1,0.1),"cm"), hjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = c(0.92,1.07))+
  xlab("")+
  guides(col="none",shape="none", fill=guide_legend(title = "", reverse = F))+
  ggsave(filename("ss_pm_trend_all"),height = 12, width = 32*1.2, units = "cm", dpi = 300)


##sul====
FT_envi_trend
table(FT_envi_trend2$variable)

FT_envi_trend_sul=subset(FT_envi_trend,FT_envi_trend$Group=="Seoul")
FT_envi_trend2_sul=subset(FT_envi_trend2,FT_envi_trend2$Group=="Seoul")


FT_envi_trend2_sul$val2=ifelse(FT_envi_trend2_sul$variable=="OC",FT_envi_trend2_sul$value*1,FT_envi_trend2_sul$value*1)
FT_envi_trend_sul$conc2=FT_envi_trend_sul$conc*3


#FT_envi_trend_sul$conc2=ifelse(FT_envi_trend_sul$Nn>52,FT_envi_trend_sul$conc*0.8*1.3,FT_envi_trend_sul$conc*1*1.3)
FT_envi_trend_sul$type=factor(FT_envi_trend_sul$type,levels = c("POC","SOC"))
FT_envi_trend2_sul_pm=subset(FT_envi_trend2_sul,FT_envi_trend2_sul$variable=="PM2.5")
FT_envi_trend2_sul_pm
FT_envi_trend_sul

tt_sul=na.omit(FT_envi_trend2_sul_pm,cols="value" )
max(FT_envi_trend_sul$OC, na.rm = T)

pmtrend_sul=ggplot()+
  geom_rect(aes(xmin=1,xmax=34, ymax=Inf, ymin=-Inf),alpha=0.4,fill="grey50")+
  geom_rect(aes(xmin=42.5,xmax=73.5, ymax=Inf, ymin=-Inf),alpha=0.4,fill="grey50")+
  geom_rect(aes(xmin=86.5,xmax=118, ymax=Inf, ymin=-Inf),alpha=0.4,fill="grey50")+
  geom_area(data = FT_envi_trend_sul ,aes(x=Nn , y=conc2,fill=type,col=type),lty=1,position = position_stack(reverse = T),size=1.2,na.rm = T)+
  geom_area(data = FT_envi_trend_sul ,aes(x=Nn , y=conc2,fill=type),position = position_stack(reverse = T),size=0.75,na.rm = T)+
  #geom_line(data = FT_envi_trend2_sul_pm ,aes(x=Nn, y=value ),col="black",size=0.75, na.rm = T)+
  geom_line(data = tt_sul ,aes(x=Nn, y=value ),col="black",size=0.75, na.rm = T)+
  geom_rect(aes(xmin=31.5,xmax=42.5, ymax=Inf, ymin=-Inf),alpha=1,fill="white")+
  geom_rect(aes(xmin=72.5,xmax=86.5, ymax=Inf, ymin=-Inf),alpha=1,fill="white")+
  geom_point(data = FT_envi_trend2_sul_pm ,aes(x=Nn, y=value ),col="black", na.rm = F, size=1.5)+
  geom_line(data = FT_envi_trend2_sul_pm[c(31,32),] ,aes(x=Nn, y=value),col="black",size=0.75, na.rm = T, lty=2)+
  geom_line(data = FT_envi_trend2_sul_pm[c(61,62),] ,aes(x=Nn, y=value),col="black",size=0.75, na.rm = T, lty=2)+
  annotate(geom = "text",x=16,y=75,label="Winter (1st)",size=10)+
  annotate(geom = "text",x=58,y=75,label="Summer (2nd)",size=10)+
  annotate(geom = "text",x=103,y=75,label="Spring (3rd)",size=10)+
  scale_color_manual(values = c("#9DBCD4","blue"))+
  scale_fill_manual(values =c("#9DBCD4","#CB7723"))+
  scale_x_continuous(breaks = c(1,32,43,73,87,118),
                     labels = c(expression(atop(paste("Dec 15"^"th"),"(2020)")),
                                expression(atop(paste("Jan 15"^"th"),"(2021)")),
                                expression(atop(paste("June 1"^"st"),"(2021)")),
                                expression(atop(paste("July 1"^"st"),"(2021)")),
                                expression(atop(paste("Mar 14"^"th"),"(2022)")),
                                expression(atop(paste("Apr 14"^"th"),"(2022)"))
                     ),
                     limits = c(1,108+5+5),
                     expand = c(0.01,0.01))+
  scale_y_continuous(name = expression(bold("PM"["2.5"]~"("*"\u03bcg/"*m^"3"*")")),expand = c(0.02,0.02),
                     sec.axis = sec_axis(~.*1/3, name = expression(bold("OC"~"("*"\u03bcg/"*m^"3"*")")),breaks = seq(0,140,5)),
                     limits = c(0,80),breaks = seq(0,160,20))+
  ggtitle("SE")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.2,0.4,0.2,0.2),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.line.y.right = element_line(colour="blue", size=1.),  
        axis.text.x = element_text(size = 20,angle = 0,vjust = 0.5, hjust = 0.5,
                                   colour = "black", face = 1,family = "Arial"),
        axis.ticks.length = unit(0.2,"cm"),
        axis.ticks.x = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.left = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.right = element_line(size = 1.5, colour = "blue"),
        axis.title.x = element_text(size = 0.1),
        axis.text.y.left = element_text(size = 20, colour = "black" , face =1,family = "Arial"),
        axis.text.y.right = element_text(size = 20, colour = "blue" , face = 1,family = "Arial"),
        axis.title.y.left = element_text(size = 24, colour = "black",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial"),
        axis.title.y.right = element_text(size = 24, colour = "blue",margin = unit(c(0.1,0.1,0.1,0.2),"cm"),family = "Arial"),
        legend.text = element_text(size = 14, colour = "black",family = "Arial",margin = unit(c(0.1,0.1,0.1,0.1),"cm"), hjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = c(0.92,1.07))+
  xlab("")+
  guides(col="none",shape="none", fill=guide_legend(title = "", reverse = F))+
  ggsave(filename("sul_pm_trend_all"),height = 12, width = 32*1.2, units = "cm", dpi = 300)



##nt====
FT_envi_trend
table(FT_envi_trend2$variable)

FT_envi_trend_nt=subset(FT_envi_trend,FT_envi_trend$Group=="Noto")
FT_envi_trend2_nt=subset(FT_envi_trend2,FT_envi_trend2$Group=="Noto")

FT_envi_trend_nt$conc2=FT_envi_trend_nt$conc*3.5


#FT_envi_trend_nt$conc2=ifelse(FT_envi_trend_nt$Nn>52,FT_envi_trend_nt$conc*0.8*1.3,FT_envi_trend_nt$conc*1*1.3)
FT_envi_trend_nt$type=factor(FT_envi_trend_nt$type,levels = c("POC","SOC"))
FT_envi_trend2_nt_pm=subset(FT_envi_trend2_nt,FT_envi_trend2_nt$variable=="PM2.5")
FT_envi_trend2_nt_pm
FT_envi_trend_nt

tt_nt=na.omit(FT_envi_trend2_nt_pm,cols="value" )
max(FT_envi_trend_nt$PM2.5, na.rm = T)

pmtrend_nt=ggplot()+
  geom_rect(aes(xmin=1,xmax=34, ymax=Inf, ymin=-Inf),alpha=0.4,fill="grey50")+
  geom_rect(aes(xmin=42.5,xmax=73.5, ymax=Inf, ymin=-Inf),alpha=0.4,fill="grey50")+
  geom_rect(aes(xmin=86.5,xmax=118, ymax=Inf, ymin=-Inf),alpha=0.4,fill="grey50")+
  geom_area(data = FT_envi_trend_nt ,aes(x=Nn , y=conc2,fill=type,col=type),lty=1,position = position_stack(reverse = T),size=1.2,na.rm = T)+
  geom_area(data = FT_envi_trend_nt ,aes(x=Nn , y=conc2,fill=type),position = position_stack(reverse = T),size=0.75,na.rm = T)+
  #geom_line(data = FT_envi_trend2_nt_pm ,aes(x=Nn, y=value ),col="black",size=0.75, na.rm = T)+
  geom_line(data = tt_nt ,aes(x=Nn, y=value ),col="black",size=0.75, na.rm = T)+
  geom_rect(aes(xmin=28.5,xmax=42.5, ymax=Inf, ymin=-Inf),alpha=1,fill="white")+
  geom_rect(aes(xmin=72.5,xmax=86.5, ymax=Inf, ymin=-Inf),alpha=1,fill="white")+
  geom_point(data = FT_envi_trend2_nt_pm ,aes(x=Nn, y=value ),col="black", na.rm = F, size=1.5)+
  geom_line(data = FT_envi_trend2_nt_pm[c(28,30),] ,aes(x=Nn, y=value),col="black",size=0.75, na.rm = T, lty=2)+
  geom_line(data = FT_envi_trend2_nt_pm[c(58,59),] ,aes(x=Nn, y=value),col="black",size=0.75, na.rm = T, lty=2)+
  annotate(geom = "text",x=16,y=51,label="Winter (1st)",size=10)+
  annotate(geom = "text",x=58,y=51,label="Summer (2nd)",size=10)+
  annotate(geom = "text",x=103,y=51,label="Spring (3rd)",size=10)+
  scale_color_manual(values = c("#9DBCD4","blue"))+
  scale_fill_manual(values =c("#9DBCD4","#CB7723"))+
  scale_x_continuous(breaks = c(1,32,43,73,87,118),
                     labels = c(expression(atop(paste("Dec 15"^"th"),"(2020)")),
                                expression(atop(paste("Jan 15"^"th"),"(2021)")),
                                expression(atop(paste("June 1"^"st"),"(2021)")),
                                expression(atop(paste("July 1"^"st"),"(2021)")),
                                expression(atop(paste("Mar 14"^"th"),"(2022)")),
                                expression(atop(paste("Apr 14"^"th"),"(2022)"))
                     ),
                     limits = c(1,108+5+5),
                     expand = c(0.01,0.01))+
  scale_y_continuous(name = expression(bold("PM"["2.5"]~"("*"\u03bcg/"*m^"3"*")")),expand = c(0.02,0.02),
                     sec.axis = sec_axis(~.*1/3.5, name = expression(bold("OC"~"("*"\u03bcg/"*m^"3"*")")),breaks = seq(0,140,5)),
                     limits = c(0,55),breaks = seq(0,160,10))+
  ggtitle("NT")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.2,0.4,0.2,0.2),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.line.y.right = element_line(colour="blue", size=1.),  
        axis.text.x = element_text(size = 20,angle = 0,vjust = 0.5, hjust = 0.5,
                                   colour = "black", face = 1,family = "Arial"),
        axis.ticks.length = unit(0.2,"cm"),
        axis.ticks.x = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.left = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.right = element_line(size = 1.5, colour = "blue"),
        axis.title.x = element_text(size = 0.1),
        axis.text.y.left = element_text(size = 20, colour = "black" , face =1,family = "Arial"),
        axis.text.y.right = element_text(size = 20, colour = "blue" , face = 1,family = "Arial"),
        axis.title.y.left = element_text(size = 24, colour = "black",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial"),
        axis.title.y.right = element_text(size = 24, colour = "blue",margin = unit(c(0.1,0.1,0.1,0.2),"cm"),family = "Arial"),
        legend.text = element_text(size = 14, colour = "black",family = "Arial",margin = unit(c(0.1,0.1,0.1,0.1),"cm"), hjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = c(0.92,1.07))+
  xlab("")+
  guides(col="none",shape="none", fill=guide_legend(title = "", reverse = F))+
  ggsave(filename("nt_pm_trend_all"),height = 12, width = 32*1.2, units = "cm", dpi = 300)


##SOC distribution=====
envi_all_sel
soc_all=envi_all_sel
soc_all$SOCp=soc_all$SOC/soc_all$OC*100
soc_all$POCp=soc_all$POC/soc_all$OC*100

soc_all$Group=factor(soc_all$Group, levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"))

soc_all$Grouplab=factor(soc_all$Group, levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"),
                         labels = c("UB","BJ","SS","SE","NT"))

soc_all$sslab=factor(soc_all$Season, levels = c("Winter","Summer","Spring"),
                     labels = c("Winter (1st)","Summer (2nd)","Spring (3rd)"))

soc_all$sslab2=factor(soc_all$Season, levels = c("Winter","Summer","Spring"),
                         labels = c(expression(bold("1"^"st"~"(Winter)")),
                                    expression(bold("2"^"nd"~"(Summer)")),
                                    expression(bold("3"^"rd"~"(Spring)"))
                         ))

ggplot()+
  stat_boxplot(data=soc_all, aes(x=Grouplab, y=SOCp/100, fill=sslab2),geom='errorbar', linetype=1, width=0.25,
               position = position_dodge(width = 0.8,preserve = "single"))+
  geom_boxplot(data=soc_all, aes(x=Grouplab, y=SOCp/100, fill=sslab2),alpha=1, outlier.color = NA,
               position = position_dodge(width = 0.8,preserve = "single") )+
  scale_y_continuous(name=" SOC (%)",breaks = seq(0,1,0.25),labels = scales::percent_format(accuracy = 1), limits = c(0,1.1))+
  scale_fill_manual(values = c("#BAD7E9","#F99417","#3CCF4E"), labels = c(expression(bold("1"^"st"~"(Winter)")),
                                                               expression(bold("2"^"nd"~"(Summer)")),
                                                               expression(bold("3"^"rd"~"(Spring)")))
                    )+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_blank(),
        plot.margin = unit(c(0.2,0.2,0.2,0.0),"cm"),
        panel.border = element_rect(size = 1.5,colour = "black"),
        axis.line.y.right = element_line(size = 1, color = "black"),
        axis.line.x.top =  element_line(colour = "black"),
        axis.text.x = element_text(size = 16,colour = "black", angle = 0, hjust = 0.5,vjust = 1.0),
        axis.ticks.x = element_blank(),
        axis.ticks.length.y = unit(0.15,"cm"),
        axis.text.y = element_text(size = 16, colour = "black" ),
        axis.title.x = element_text(size = 0.1, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        axis.title.y.left = element_text(size = 18, colour = "black",margin = unit(c(0.2,0.2,0.2,0.2),"cm")),
        axis.title.y.right = element_text(size = 18, colour = "black",angle = 90,margin = unit(c(0.2,0.2,0.2,0.5),"cm")),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text.x = element_text(size = 18, colour = "black",face = 2,margin = unit(c(0.3,0.4,0.3,0.2),"cm")),
        strip.text.y = element_blank(),
        strip.placement = "outside",
        #legend.margin = unit(c(0.0,0.0,0.0,0.0),"cm"),
        legend.title = element_text(size = 22, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        legend.text = element_text(size = 14,hjust=0, colour = "black",margin = unit(c(0.2,0.2,0.2,-0.2),"cm")),
        legend.key.width = unit(0.8,"cm"),
        legend.key.height = unit(0.8,"cm"),
        legend.background = element_blank(),
        legend.direction = "vertical",
        legend.justification=c(0.5, 0.5),
        legend.position = c(0.10,0.83))+
  guides(fill=guide_legend(title = NULL,title.hjust = 0.5))+
  ggsave(filename("FRIEND_all_SOC_region_compare"),height = 10, width = 25, units = "cm", dpi = 300)

grp=unique(soc_all$Group)
ss=unique(soc_all$Season)

stat_ss=data.frame()
for (i in 1:length(grp)) {
  
 # i=1
  temp=subset(soc_all,soc_all$Group==grp[i])
  
  aov_temp=aov(data=temp, SOCp~Season)
  
  #tuky=tukey.test2 <- LSD.test(aov_temp, trt = 'Season',p.adj = "none")
  tuky=tukey.test2 <- HSD.test(aov_temp, trt = 'Season')
  
  av_ph=tuky$groups
  av_ph$smp=rownames(av_ph)
  av_ph=av_ph %>% `colnames<-`(c("stat","Group","smp"))
  av_ph$Type="AOV"
  ph=av_ph
  ph$Region=grp[i]
  
  
  stat_ss=rbind(stat_ss,ph)
  
  }
stat_ss

soc_all
soc_all_ev=soc_all %>% filter(Event%in%c("Event","Non-event"))

ggplot()+
  stat_boxplot(data=soc_all_ev, aes(x=Grouplab, y=SOCp/100, fill=Event),geom='errorbar', linetype=1, width=0.25,
               position = position_dodge(width = 0.8,preserve = "single"))+
  geom_boxplot(data=soc_all_ev, aes(x=Grouplab, y=SOCp/100, fill=Event),alpha=1, outlier.color = NA,
               position = position_dodge(width = 0.8,preserve = "single") )+
  scale_y_continuous(name=" SOC (%)",breaks = seq(0,1,0.25),labels = scales::percent_format(accuracy = 1), limits = c(0,1.1))+
  scale_fill_manual(values = c("grey60", "#FFFFFF"))+
  facet_wrap(sslab~.)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.title = element_blank(),
        plot.margin = unit(c(0.2,0.2,0.2,0.0),"cm"),
        panel.border = element_rect(size = 1.5,colour = "black"),
        axis.line.y.right = element_line(size = 1, color = "black"),
        axis.line.x.top =  element_line(colour = "black"),
        axis.text.x = element_text(size = 16,colour = "black", angle = 0, hjust = 0.5,vjust = 1.0),
        axis.ticks.x = element_blank(),
        axis.ticks.length.y = unit(0.15,"cm"),
        axis.text.y = element_text(size = 16, colour = "black" ),
        axis.title.x = element_text(size = 0.1, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        axis.title.y.left = element_text(size = 18, colour = "black",margin = unit(c(0.2,0.2,0.2,0.2),"cm")),
        axis.title.y.right = element_text(size = 18, colour = "black",angle = 90,margin = unit(c(0.2,0.2,0.2,0.5),"cm")),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text.x = element_text(size = 18, colour = "black",face = 2,margin = unit(c(0.3,0.4,0.3,0.2),"cm")),
        strip.text.y = element_blank(),
        strip.placement = "outside",
        #legend.margin = unit(c(0.0,0.0,0.0,0.0),"cm"),
        legend.title = element_text(size = 22, colour = "black",margin = unit(c(0.0,0.2,0.0,0.2),"cm")),
        legend.text = element_text(size = 16, colour = "black",margin = unit(c(0.2,0.2,0.2,-0.2),"cm")),
        legend.key.width = unit(1.0,"cm"),
        legend.key.height = unit(1.0,"cm"),
        legend.background = element_blank(),
        legend.direction = "vertical",
        legend.justification=c(0.5, 0.5),
        legend.position = c(0.92,0.17))+
  guides(fill=guide_legend(title = NULL,title.hjust = 0.5))+
  ggsave(filename("FRIEND_2nd_SOC_event_compare"),height = 10, width = 30, units = "cm", dpi = 300)


ev=unique(soc_all_ev$Event)
grp=unique(soc_all_ev$Group)
ss

soc_all_ev_sel1=subset(soc_all_ev,soc_all_ev$Group!="Noto")
soc_all_ev_sel2=subset(soc_all_ev,soc_all_ev$Group=="Noto")
soc_all_ev_sel2=subset(soc_all_ev_sel2,soc_all_ev_sel2$Season!="Spring") %>% droplevels()

soc_all_ev_sel=rbind(soc_all_ev_sel1,soc_all_ev_sel2)

ev_stat=data.frame()
for (i in 1:length(grp)) {
  
  #i=1
  temp=subset(soc_all_ev_sel,soc_all_ev_sel$Group==grp[i])
  
  for (j in 1:length(ss)) {
    
    #j=1
    temp2=subset(temp,temp$Season==ss[j])
    
    temp_nev=subset(temp2,temp2$Event=="Non-event")
    temp_ev=subset(temp2,temp2$Event=="Event")
    
    s1=shapiro.test(temp_ev$SOCp)
    s2=shapiro.test(temp_nev$SOCp)
    
    ts=t.test(temp_ev$SOCp,temp_nev$SOCp,paired = F,var.equal = F)
    ws=wilcox.test(temp_ev$SOCp,temp_nev$SOCp,exact = F)
    
    new=data.frame(Group=grp[i],Season=ss[j] ,Norm_ev=round(s1$p.value,3),Norm_nev=round(s1$p.value,3), T.Test=round(ts$p.value,3),W.Test=round(ws$p.value,3))
    
    ev_stat=rbind(new,ev_stat)
    
  }
  
}

ev_stat


##Correlation between SOCp and PM2.5===========
soc_all

grp
ss

cor_stat=data.frame()
for (i in 1:length(grp)) {
  
 # i=1
  temp=subset(soc_all,soc_all$Group==grp[i])
  
  for (j in 1:length(ss)) {
    
    #j=1
    temp2=subset(temp,temp$Season==ss[j])
    temp2
    
    s1=shapiro.test(temp2$PM2.5)
    s2=shapiro.test(temp2$SOCp)
    
    linear=lm(data=temp2, PM2.5~SOCp)
    
    sl=summary(linear)
    
    cp=cor.test(temp2$PM2.5,temp2$SOCp, method = "pearson")
    cs=cor.test(temp2$PM2.5,temp2$SOCp, method = "spearman",exact = F)
    
    new=data.frame(Group=grp[i],Season=ss[j], norm1=round(s1$p.value,3),norm2=round(s2$p.value,3), R2=round(sl$r.squared,3),
                   Cor_p=round(cp$estimate,2),p_p=round(cp$p.value,2),Cor_s=round(cs$estimate,3),s_p=round(cs$p.value,3))
    
    cor_stat=rbind(new,cor_stat)
    
  }
}

cor_stat

soc_all
soc_all_sel=subset(soc_all,soc_all$SOCp<100)

soc_all_sel1=subset(soc_all_sel,soc_all_sel$Group!="Noto")
soc_all_sel2=subset(soc_all_sel,soc_all_sel$Group=="Noto") %>% filter(Season%in%c("Winter","Summer"))

soc_all_sel=rbind(soc_all_sel1,soc_all_sel2)

soc_all_sel$sslab=factor(soc_all_sel$Season, levels = c("Winter","Summer","Spring"),
                         labels = c(expression(bold("1"^"st"~"(Winter)")),
                                    expression(bold("2"^"nd"~"(Summer)")),
                                    expression(bold("3"^"rd"~"(Spring)"))
                                    ))

soc_all_sel$Grouplab2=factor(soc_all_sel$Grouplab, levels = c("UB","BJ","SS","SE","NT"),
                         labels = c(expression(bold("UB")),
                                    expression(bold("BJ")),
                                    expression(bold("SS")),
                                    expression(bold("SE")),
                                    expression(bold("NT"))
                         ))

ggplot(soc_all_sel,aes(x=PM2.5,y=SOCp, col=Grouplab2),shape=21)+
  geom_ribbon(data=soc_all_sel, aes(x=PM2.5,y=SOCp, group=Grouplab2),stat = "smooth",
              method = "lm",
              size=1.5,
              se = TRUE,
              fill="blue",
              alpha = 0.1, # or, use fill = NA
              colour = "black",
              linetype = "dotted")+
  geom_smooth(data=soc_all_sel, aes(x=PM2.5,y=SOCp, group=Grouplab2),col="red",method="lm",lty=1,se=F, size=1.5)+
  geom_point(data=soc_all_sel,aes(x=PM2.5,y=SOCp,fill=Grouplab2),col="black",size=8, shape=21)+
  facet_grid2(sslab~Grouplab2,independent = T,scales = "free", switch = "y",labeller = label_parsed )+
  scale_fill_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  scale_y_continuous(name = "SOC (%)",expand = c(0.1,0.1))+
  scale_x_continuous(name = expression(bold("PM"["2.5"]~"("*"\u03bcg/"*m^"3"*")")),expand = c(0.1,0.1))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text.x = element_text(colour = "black", size = 26, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        strip.text.y.left = element_text(colour = "black", size = 26, face = "bold",margin = unit(c(0.1,2.0,0.2,0.0),"cm")),
        strip.placement = "outside",
        plot.title= element_text(size = 50, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.2,0.4,0.2,0.2),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.line.y.right = element_line(colour="blue", size=1.2),  
        axis.text.x = element_text(size = 22,angle = 0,vjust = 0.5, hjust = 0.5,
                                   colour = "black",family = "Arial",margin = unit(c(0.1,0.1,0.2,0.1),"cm")),
        axis.ticks.length = unit(0.2,"cm"),
        axis.ticks.y.left = element_line(size = 1.5, colour = "black"),
        axis.ticks.x = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.right = element_line(size = 1.5, colour = "blue"),
        axis.title.x.bottom = element_text(size = 24, colour = "black",margin = unit(c(0.4,0.1,0.1,0.1),"cm"),family = "Arial"),
        axis.text.y.left = element_text(size = 22, colour = "black" ,family = "Arial"),
        axis.text.y.right = element_text(size = 22, colour = "blue" ,family = "Arial"),
        axis.title.y.left = element_text(size = 22, colour = "black",face = "bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial",vjust = -16),
        legend.text = element_text(size = 18, colour = "black",family = "Arial",margin = unit(c(0.1,0.1,0.2,0.1),"cm"), hjust = 0,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "vertical",
        legend.background = element_blank(),
        #legend.position = c(0.08,1.02) 
        legend.position = "NULL")+
  guides(fill=guide_legend(title = "", reverse = T, override.aes = list(size=8)))+
  ggsave(filename("pmvsSOC_all"),height = 45, width = 75, units = "cm", dpi = 300)



