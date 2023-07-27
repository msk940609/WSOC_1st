ev_1st=fread("Datafile/FRIEND_1st_envi_re.csv")
ft_1st=fread("Datafile/FRIEND_1st.csv")
cor_1st=fread("Datafile/molecular_cor_raw_1st.csv")

#1. map and pm trend=====

##1-1) map=====
samp=fread("Datafile/sampling_point.csv")
samp

##1-2) pm trend=====
envi_1st=ev_1st
envi_1st$WISOC=envi_1st$OC-envi_1st$WSOC
envi_1st$WSOCbb=2.94*envi_1st$Levoglucosan
envi_1st$WSOCnbb=envi_1st$WSOC-envi_1st$WSOCbb
envi_1st$SOC=envi_1st$WSOCnbb
envi_1st$POC=envi_1st$WSOCbb+envi_1st$WISOC
envi_1st$Season="Winter"

envi_1st$POCp=envi_1st$POC/envi_1st$OC*100
envi_1st$SOCp=envi_1st$SOC/envi_1st$OC*100

envi_1st_sel=envi_1st[,c("Group","No","Season","Event","Date","PM2.5","OC","POC","SOC")]
envi_1st_sel$Nn=envi_1st_sel$No


FT_envi_trend=melt(envi_1st_sel[,c("Group","Season","Nn","No","Event","Date","PM2.5","OC","POC","SOC")],
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

#lims <- as.POSIXct(strptime(c("2021-05-31 23:00","2021-06-30 24:00"), format = "%Y-%m-%d %H:%M"))

FT_envi_trend$type=factor(FT_envi_trend$type, levels = c("POC","SOC"))
FT_envi_trend$Group=factor(FT_envi_trend$Group,levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"))
FT_envi_trend2$Group=factor(FT_envi_trend2$Group,levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"))

lims <- as.POSIXct(strptime(c("2020-12-15 6:00","2021-01-14 12:00"), format = "%Y-%m-%d %H:%M"))

###1)Ul====
FT_envi_trend
table(FT_envi_trend2$variable)

FT_envi_trend_ul=subset(FT_envi_trend,FT_envi_trend$Group=="Ulaanbaatar")
FT_envi_trend2_ul=subset(FT_envi_trend2,FT_envi_trend2$Group=="Ulaanbaatar")

FT_envi_trend2_ul$val2=ifelse(FT_envi_trend2_ul$variable=="OC",FT_envi_trend2_ul$value*1,FT_envi_trend2_ul$value*1)
FT_envi_trend_ul$type=factor(FT_envi_trend_ul$type,levels = c("POC","SOC"))
FT_envi_trend2_ul_pm=subset(FT_envi_trend2_ul,FT_envi_trend2_ul$variable=="PM2.5")

FT_envi_trend_ul$conc2=FT_envi_trend_ul$conc*1

FT_envi_trend2_ul_pm
FT_envi_trend_ul
tt_ul=na.omit(FT_envi_trend2_ul_pm,cols="value" )

pmtrend_ul=ggplot()+
  #geom_rect(aes(xmin=1,xmax=34, ymax=Inf, ymin=-Inf),alpha=0.4,fill="grey50")+
  #geom_rect(aes(xmin=42.5,xmax=73.5, ymax=Inf, ymin=-Inf),alpha=0.4,fill="grey50")+
  #geom_rect(aes(xmin=86.5,xmax=118, ymax=Inf, ymin=-Inf),alpha=0.4,fill="grey50")+
  geom_area(data = FT_envi_trend_ul ,aes(x=date2 , y=conc2,fill=type,col=type),lty=1,position = position_stack(reverse = T),size=2,na.rm = T)+
  geom_area(data = FT_envi_trend_ul ,aes(x=date2 , y=conc2,fill=type),position = position_stack(reverse = T),size=1.5,na.rm = T)+
  geom_line(data = tt_ul ,aes(x=date2, y=value ),col="black",size=0.75, na.rm = T)+
  geom_point(data = FT_envi_trend2_ul_pm ,aes(x=date2, y=value ),col="black", na.rm = F, size=3)+
  #annotate(geom = "text",x=16,y=164,label="Winter (1st)",size=10)+
  scale_color_manual(values = c("#9DBCD4","blue"))+
  scale_fill_manual(values =c("#9DBCD4","#CB7723"))+
  scale_x_datetime('',
                   limits = lims,
                   date_breaks = '2 day',
                   date_labels = "%b-%d", expand = c(0.01,0.01))+
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
        axis.text.x = element_text(size = 20,angle = 315,vjust = 1.2, hjust = -0.1,
                                   colour = "black", face = 2,family = "Arial"),
        axis.ticks.length = unit(0.2,"cm"),
        axis.ticks.x = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.left = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.right = element_line(size = 1.5, colour = "blue"),
        axis.title.x = element_text(size = 0.1),
        axis.text.y.left = element_text(size = 20, colour = "black" , face =2,family = "Arial"),
        axis.text.y.right = element_text(size = 20, colour = "blue" , face = 2,family = "Arial"),
        axis.title.y.left = element_text(size = 24, colour = "black",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial"),
        axis.title.y.right = element_text(size = 24, colour = "blue",margin = unit(c(0.1,0.1,0.1,0.2),"cm"),family = "Arial"),
        legend.text = element_text(size = 18, colour = "black",family = "Arial",margin = unit(c(0.1,0.3,0.1,0.1),"cm"), hjust = 0.5,vjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = c(0.90,1.07))+
  xlab("")+
  guides(col="none",shape="none", fill=guide_legend(title = "", reverse = F))+
  ggsave(filename("Ulan_pm_trend_all"),height = 12, width = 35, units = "cm", dpi = 300)


###2) bj====
FT_envi_trend
table(FT_envi_trend2$variable)

FT_envi_trend_bj=subset(FT_envi_trend,FT_envi_trend$Group=="Beijing")
FT_envi_trend2_bj=subset(FT_envi_trend2,FT_envi_trend2$Group=="Beijing")
FT_envi_trend_bj$conc2=FT_envi_trend_bj$conc*3

#FT_envi_trend_bj$conc2=ifelse(FT_envi_trend_bj$Nn>52,FT_envi_trend_bj$conc*0.8*1.3,FT_envi_trend_bj$conc*1*1.3)
FT_envi_trend_bj$type=factor(FT_envi_trend_bj$type,levels = c("POC","SOC"))
FT_envi_trend2_bj_pm=subset(FT_envi_trend2_bj,FT_envi_trend2_bj$variable=="PM2.5")

FT_envi_trend2_bj_pm
FT_envi_trend_bj

tt_bj=na.omit(FT_envi_trend2_bj_pm,cols="value" )

max(FT_envi_trend_bj$OC, na.rm = T)

pmtrend_bj=ggplot()+
  #geom_rect(aes(xmin=1,xmax=34, ymax=Inf, ymin=-Inf),alpha=0.4,fill="grey50")+
  #geom_rect(aes(xmin=42.5,xmax=73.5, ymax=Inf, ymin=-Inf),alpha=0.4,fill="grey50")+
  #geom_rect(aes(xmin=86.5,xmax=118, ymax=Inf, ymin=-Inf),alpha=0.4,fill="grey50")+
  geom_area(data = FT_envi_trend_bj ,aes(x=date2 , y=conc2,fill=type,col=type),lty=1,position = position_stack(reverse = T),size=2,na.rm = T)+
  geom_area(data = FT_envi_trend_bj ,aes(x=date2 , y=conc2,fill=type),position = position_stack(reverse = T),size=1.5,na.rm = T)+
  geom_line(data = tt_bj ,aes(x=date2, y=value ),col="black",size=0.75, na.rm = T)+
  geom_point(data = FT_envi_trend2_bj_pm ,aes(x=date2, y=value ),col="black", na.rm = F, size=3)+
  #annotate(geom = "text",x=16,y=164,label="Winter (1st)",size=10)+
  scale_color_manual(values = c("#9DBCD4","blue"))+
  scale_fill_manual(values =c("#9DBCD4","#CB7723"))+
  scale_x_datetime('',
                   limits = lims,
                   date_breaks = '2 day',
                   date_labels = "%b-%d", expand = c(0.01,0.01))+
  scale_y_continuous(name = expression(bold("PM"["2.5"]~"("*"\u03bcg/"*m^"3"*")")),expand = c(0.02,0.02),
                     sec.axis = sec_axis(~.*1/3, name = expression(bold("OC"~"("*"\u03bcg/"*m^"3"*")")),breaks = seq(0,140,10)),
                     limits = c(0,90),breaks = seq(0,160,30))+
  ggtitle("BJ")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.2,0.4,0.2,0.2),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.line.y.right = element_line(colour="blue", size=1.),  
        axis.text.x = element_text(size = 20,angle = 315,vjust = 1.2, hjust = -0.1,
                                   colour = "black", face = 2,family = "Arial"),
        axis.ticks.length = unit(0.2,"cm"),
        axis.ticks.x = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.left = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.right = element_line(size = 1.5, colour = "blue"),
        axis.title.x = element_text(size = 0.1),
        axis.text.y.left = element_text(size = 20, colour = "black" , face =2,family = "Arial"),
        axis.text.y.right = element_text(size = 20, colour = "blue" , face = 2,family = "Arial"),
        axis.title.y.left = element_text(size = 24, colour = "black",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial"),
        axis.title.y.right = element_text(size = 24, colour = "blue",margin = unit(c(0.1,0.1,0.1,0.2),"cm"),family = "Arial"),
        legend.text = element_text(size = 18, colour = "black",family = "Arial",margin = unit(c(0.1,0.3,0.1,0.1),"cm"), hjust = 0.5,vjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = c(0.90,1.07))+
  xlab("")+
  guides(col="none",shape="none", fill=guide_legend(title = "", reverse = F))+
  ggsave(filename("bj_pm_trend_all"),height = 12, width = 35, units = "cm", dpi = 300)

### 3) ss====
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
  geom_area(data = FT_envi_trend_ss ,aes(x=date2 , y=conc2,fill=type,col=type),lty=1,position = position_stack(reverse = T),size=2,na.rm = T)+
  geom_area(data = FT_envi_trend_ss ,aes(x=date2 , y=conc2,fill=type),position = position_stack(reverse = T),size=1.5,na.rm = T)+
  geom_line(data = tt_ss ,aes(x=date2, y=value ),col="black",size=0.75, na.rm = T)+
  geom_point(data = FT_envi_trend2_ss_pm ,aes(x=date2, y=value ),col="black", na.rm = F, size=3)+
  scale_color_manual(values = c("#9DBCD4","blue"))+
  scale_fill_manual(values =c("#9DBCD4","#CB7723"))+
  scale_x_datetime('',
                   limits = lims,
                   date_breaks = '2 day',
                   date_labels = "%b-%d", expand = c(0.01,0.01))+
  scale_y_continuous(name = expression(bold("PM"["2.5"]~"("*"\u03bcg/"*m^"3"*")")),expand = c(0.02,0.02),
                     sec.axis = sec_axis(~.*1/3, name = expression(bold("OC"~"("*"\u03bcg/"*m^"3"*")")),breaks = seq(0,140,10)),
                     limits = c(0,70),breaks = seq(0,160,20))+
  ggtitle("SS")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.2,0.4,0.2,0.2),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.line.y.right = element_line(colour="blue", size=1.),  
        axis.text.x = element_text(size = 20,angle = 315,vjust = 1.2, hjust = -0.1,
                                   colour = "black", face = 2,family = "Arial"),
        axis.ticks.length = unit(0.2,"cm"),
        axis.ticks.x = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.left = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.right = element_line(size = 1.5, colour = "blue"),
        axis.title.x = element_text(size = 0.1),
        axis.text.y.left = element_text(size = 20, colour = "black" , face =2,family = "Arial"),
        axis.text.y.right = element_text(size = 20, colour = "blue" , face = 2,family = "Arial"),
        axis.title.y.left = element_text(size = 24, colour = "black",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial"),
        axis.title.y.right = element_text(size = 24, colour = "blue",margin = unit(c(0.1,0.1,0.1,0.2),"cm"),family = "Arial"),
        legend.text = element_text(size = 18, colour = "black",family = "Arial",margin = unit(c(0.1,0.3,0.1,0.1),"cm"), hjust = 0.5,vjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = c(0.90,1.07))+
  xlab("")+
  guides(col="none",shape="none", fill=guide_legend(title = "", reverse = F))+
  ggsave(filename("ss_pm_trend_all"),height = 12, width = 35, units = "cm", dpi = 300)

### 4)sul====
FT_envi_trend
table(FT_envi_trend2$variable)

FT_envi_trend_sul=subset(FT_envi_trend,FT_envi_trend$Group=="Seoul")
FT_envi_trend2_sul=subset(FT_envi_trend2,FT_envi_trend2$Group=="Seoul")


FT_envi_trend2_sul$val2=ifelse(FT_envi_trend2_sul$variable=="OC",FT_envi_trend2_sul$value*1,FT_envi_trend2_sul$value*1)
FT_envi_trend_sul$conc2=FT_envi_trend_sul$conc*3.5


#FT_envi_trend_sul$conc2=ifelse(FT_envi_trend_sul$Nn>52,FT_envi_trend_sul$conc*0.8*1.3,FT_envi_trend_sul$conc*1*1.3)
FT_envi_trend_sul$type=factor(FT_envi_trend_sul$type,levels = c("POC","SOC"))
FT_envi_trend2_sul_pm=subset(FT_envi_trend2_sul,FT_envi_trend2_sul$variable=="PM2.5")
FT_envi_trend2_sul_pm

tt_sul=na.omit(FT_envi_trend2_sul_pm,cols="value" )
max(FT_envi_trend_sul$OC, na.rm = T)

pmtrend_sul=ggplot()+
  geom_area(data = FT_envi_trend_sul ,aes(x=date2 , y=conc2,fill=type,col=type),lty=1,position = position_stack(reverse = T),size=2,na.rm = T)+
  geom_area(data = FT_envi_trend_sul ,aes(x=date2 , y=conc2,fill=type),position = position_stack(reverse = T),size=1.5,na.rm = T)+
  geom_line(data = tt_sul ,aes(x=date2, y=value ),col="black",size=0.75, na.rm = T)+
  geom_point(data = FT_envi_trend2_sul_pm ,aes(x=date2, y=value ),col="black", na.rm = F, size=3)+
  scale_color_manual(values = c("#9DBCD4","blue"))+
  scale_fill_manual(values =c("#9DBCD4","#CB7723"))+
  scale_x_datetime('',
                   limits = lims,
                   date_breaks = '2 day',
                   date_labels = "%b-%d", expand = c(0.01,0.01))+
  scale_y_continuous(name = expression(bold("PM"["2.5"]~"("*"\u03bcg/"*m^"3"*")")),expand = c(0.02,0.02),
                     sec.axis = sec_axis(~.*1/3, name = expression(bold("OC"~"("*"\u03bcg/"*m^"3"*")")),breaks = seq(0,140,10)),
                     limits = c(0,70),breaks = seq(0,160,20))+
  ggtitle("SE")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.2,0.4,0.2,0.2),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.line.y.right = element_line(colour="blue", size=1.),  
        axis.text.x = element_text(size = 20,angle = 315,vjust = 1.2, hjust = -0.1,
                                   colour = "black", face = 2,family = "Arial"),
        axis.ticks.length = unit(0.2,"cm"),
        axis.ticks.x = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.left = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.right = element_line(size = 1.5, colour = "blue"),
        axis.title.x = element_text(size = 0.1),
        axis.text.y.left = element_text(size = 20, colour = "black" , face =2,family = "Arial"),
        axis.text.y.right = element_text(size = 20, colour = "blue" , face = 2,family = "Arial"),
        axis.title.y.left = element_text(size = 24, colour = "black",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial"),
        axis.title.y.right = element_text(size = 24, colour = "blue",margin = unit(c(0.1,0.1,0.1,0.2),"cm"),family = "Arial"),
        legend.text = element_text(size = 18, colour = "black",family = "Arial",margin = unit(c(0.1,0.3,0.1,0.1),"cm"), hjust = 0.5,vjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = c(0.90,1.07))+
  xlab("")+
  guides(col="none",shape="none", fill=guide_legend(title = "", reverse = F))+
  ggsave(filename("sul_pm_trend_all"),height = 12, width = 35, units = "cm", dpi = 300)


###5)nt====
FT_envi_trend
table(FT_envi_trend2$variable)

FT_envi_trend_nt=subset(FT_envi_trend,FT_envi_trend$Group=="Noto")
FT_envi_trend2_nt=subset(FT_envi_trend2,FT_envi_trend2$Group=="Noto")

FT_envi_trend_nt$conc2=FT_envi_trend_nt$conc*3.0

FT_envi_trend_nt$type=factor(FT_envi_trend_nt$type,levels = c("POC","SOC"))
FT_envi_trend2_nt_pm=subset(FT_envi_trend2_nt,FT_envi_trend2_nt$variable=="PM2.5")
FT_envi_trend2_nt_pm
FT_envi_trend_nt

tt_nt=na.omit(FT_envi_trend2_nt_pm,cols="value" )
max(FT_envi_trend_nt$PM2.5, na.rm = T)

pmtrend_nt=ggplot()+
  geom_area(data = FT_envi_trend_nt ,aes(x=date2 , y=conc2,fill=type,col=type),lty=1,position = position_stack(reverse = T),size=2,na.rm = T)+
  geom_area(data = FT_envi_trend_nt ,aes(x=date2 , y=conc2,fill=type),position = position_stack(reverse = T),size=1.5,na.rm = T)+
  geom_line(data = tt_nt ,aes(x=date2, y=value ),col="black",size=0.75, na.rm = T)+
  geom_point(data = FT_envi_trend2_nt_pm ,aes(x=date2, y=value ),col="black", na.rm = F, size=3)+
  scale_color_manual(values = c("#9DBCD4","blue"))+
  scale_fill_manual(values =c("#9DBCD4","#CB7723"))+
  scale_x_datetime('',
                   limits = lims,
                   date_breaks = '2 day',
                   date_labels = "%b-%d", expand = c(0.01,0.01))+
  scale_y_continuous(name = expression(bold("PM"["2.5"]~"("*"\u03bcg/"*m^"3"*")")),expand = c(0.02,0.02),
                     sec.axis = sec_axis(~.*1/3, name = expression(bold("OC"~"("*"\u03bcg/"*m^"3"*")"))),
                     limits = c(0,10))+
  ggtitle("NT")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.2,0.4,0.2,0.2),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.line.y.right = element_line(colour="blue", size=1.),  
        axis.text.x = element_text(size = 20,angle = 315,vjust = 1.2, hjust = -0.1,
                                   colour = "black", face = 2,family = "Arial"),
        axis.ticks.length = unit(0.2,"cm"),
        axis.ticks.x = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.left = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.right = element_line(size = 1.5, colour = "blue"),
        axis.title.x = element_text(size = 0.1),
        axis.text.y.left = element_text(size = 20, colour = "black" , face =2,family = "Arial"),
        axis.text.y.right = element_text(size = 20, colour = "blue" , face = 2,family = "Arial"),
        axis.title.y.left = element_text(size = 24, colour = "black",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial"),
        axis.title.y.right = element_text(size = 24, colour = "blue",margin = unit(c(0.1,0.1,0.1,0.2),"cm"),family = "Arial"),
        legend.text = element_text(size = 18, colour = "black",family = "Arial",margin = unit(c(0.1,0.3,0.1,0.1),"cm"), hjust = 0.5,vjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = c(0.90,1.07))+
  xlab("")+
  guides(col="none",shape="none", fill=guide_legend(title = "", reverse = F))+
  ggsave(filename("nt_pm_trend_all"),height = 12, width = 35, units = "cm", dpi = 300)

###All=====
FT_envi_trend

FT_envi_trend_all=rbind(FT_envi_trend_ul,FT_envi_trend_bj,FT_envi_trend_ss,FT_envi_trend_sul,FT_envi_trend_nt)
FT_envi_trend2_all=rbind(FT_envi_trend2_ul,FT_envi_trend2_bj,FT_envi_trend2_ss,FT_envi_trend2_sul,FT_envi_trend2_nt)

FT_envi_trend2_all_pm=subset(FT_envi_trend2_all,FT_envi_trend2_all$variable=="PM2.5")

tt_all=na.omit(FT_envi_trend2_all_pm,cols="value" )
max(FT_envi_trend2_all_pm$PM2.5, na.rm = T)

FT_envi_trend_all$Grouplab=factor(FT_envi_trend_all$Group, levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"),
                                  labels = c("UT","BJ","SS","SE","NT"))

tt_all$Grouplab=factor(tt_all$Group, levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"),
                                  labels = c("UT","BJ","SS","SE","NT"))

FT_envi_trend2_all_pm$Grouplab=factor(FT_envi_trend2_all_pm$Group, levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"),
                                  labels = c("UT","BJ","SS","SE","NT"))

envi_na=subset(FT_envi_trend,is.na(FT_envi_trend$conc))
envi_na$Grouplab=factor(envi_na$Group, levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"),
                                      labels = c("UT","BJ","SS","SE","NT"))



ggplot()+
  geom_blank(data = FT_envi_trend2_all_pm ,aes(x=date2, y=value*1.1 ))+
  geom_area(data = FT_envi_trend_all ,aes(x=date2 , y=conc2,fill=type,col=type),lty=1,position = position_stack(reverse = T),size=2,na.rm = T)+
  geom_area(data = FT_envi_trend_all ,aes(x=date2 , y=conc2,fill=type),position = position_stack(reverse = T),size=1.5,na.rm = T)+
  geom_rect(data=envi_na,aes(xmin=date2-3600*12,xmax=date2+3600*12, ymin=-Inf, ymax=Inf), fill="white")+
  geom_line(data = tt_all ,aes(x=date2, y=value ),col="black",size=0.75, na.rm = T)+
  geom_point(data = FT_envi_trend2_all_pm ,aes(x=date2, y=value ),col="black", na.rm = F, size=3)+
  scale_color_manual(values = c("#9DBCD4","blue"))+
  scale_fill_manual(values =c("#9DBCD4","#CB7723"))+
  scale_x_datetime('',
                   limits = lims,
                   date_breaks = '2 day',
                   date_labels = "%b-%d", expand = c(0.01,0.01))+
  scale_y_continuous(name = expression(bold("PM"["2.5"]~"("*"\u03bcg/"*m^"3"*")")),expand = c(0.05,0.05),
                     sec.axis = sec_axis(~.*1/3, name = expression(bold("OC"~"("*"\u03bcg/"*m^"3"*")"))),
                     )+
  facet_wrap(Grouplab~., scales = "free_y", ncol = 1)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.2,0.4,0.2,0.2),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.line.y.right = element_line(colour="blue", size=1.),  
        axis.text.x = element_text(size = 20,angle = 325,vjust = 1.2, hjust = -0.1,
                                   colour = "black", face = 2,family = "Arial"),
        axis.ticks.length = unit(0.2,"cm"),
        axis.ticks.x = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.left = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.right = element_line(size = 1.5, colour = "blue"),
        axis.title.x = element_text(size = 0.1),
        axis.text.y.left = element_text(size = 20, colour = "black" , face =2,family = "Arial"),
        axis.text.y.right = element_text(size = 20, colour = "blue" , face = 2,family = "Arial"),
        axis.title.y.left = element_text(size = 24, colour = "black",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial"),
        axis.title.y.right = element_text(size = 24, colour = "blue",margin = unit(c(0.1,0.1,0.1,0.2),"cm"),family = "Arial"),
        legend.text = element_text(size = 18, colour = "black",family = "Arial",margin = unit(c(0.1,0.3,0.1,0.1),"cm"), hjust = 0.5,vjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = c(0.90,1.07))+
  xlab("")+
  guides(col="none",shape="none", fill=guide_legend(title = "", reverse = F))+
  ggsave(filename("pm_trend_all"),height = 35, width = 35, units = "cm", dpi = 300)


#2.vkplot======

##2-1) all=====

frd_merge ##merge data processsed in build_dataset.R
frd_merge_sel=subset(frd_merge,frd_merge$cnt>5)

fm_prop=unique(frd_merge_sel[,c("Formula","O.C","H.C","AI","DBE","Molecularclass","AIClass","Comp")])

vk_frd=melt(frd_merge_sel[,c("Group","pd","Formula","Bromo.Inty")], id.vars = c("Group","pd","Formula")) %>% 
  dcast(Group+pd~Formula, mean) %>% 
  melt(id.vars=c("Group","pd"), na.rm = T) %>% `colnames<-`(c("Group","pd","Formula","val"))

vk_frd=vk_frd %>% left_join(fm_prop)
vk_frd$Grouplab
vk_frd$Grouplab=factor(vk_frd$Group,
                       levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"),
                       labels = c("UB","BJ","SS","SE","NT"))
vk_frd$pdlab=factor(vk_frd$pd,
                    levels = c("1st","2nd","3rd"),
                    labels = c("1st (Winter)","2nd (Summer)","3rd (Spring)"))

vk_frd
vk_frd$ord=ifelse(vk_frd$Comp=="CHO",2,
                  ifelse(vk_frd$Comp=="CHON",3,
                         ifelse(vk_frd$Comp=="CHOS",4,
                                ifelse(vk_frd$Comp=="CHONS",5,6))))
vk_frd=vk_frd[order(vk_frd$ord,decreasing = T),]

frd_merge_sel

cnt=aggregate(frd_merge_sel$Freq, by=list(Group=frd_merge_sel$Group, pd=frd_merge_sel$pd, Formula=frd_merge_sel$Formula),sum)
cnt$freq=1

cnt2=aggregate(cnt$freq, by=list(Group=cnt$Group, pd=cnt$pd),sum)

cnt2$Grouplab=factor(cnt2$Group,
                     levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"),
                     labels = c("UB","BJ","SS","SE","NT"))

cnt2$pdlab=factor(cnt2$pd,
                  levels = c("1st","2nd","3rd"),
                  labels = c("1st (Winter)","2nd (Summer)","3rd (Spring)"))

ggplot()+
  geom_hline(yintercept = 1.5,lty=2,size=1)+
  geom_abline(slope = -1, intercept = 1.1,lty=2,size=1)+
  geom_abline(slope = -0.76, intercept = 0.75, lty=2,size=1)+
  geom_point(data=vk_frd, aes(x=O.C, y=H.C,fill=Comp,col=Comp),size=2, shape=21, alpha=0.4)+
  geom_text(data=cnt2, aes(x=0.15, y=0.15, label=paste0("italic('n')=","=",x)),size=9, parse = TRUE)+
  facet_rep_grid(pdlab~Grouplab, repeat.tick.labels = "all")+
  scale_x_continuous(name = "O/C",expand = c(0.01,0.01),limits = c(-0.01,1.0), breaks = round(seq(0,2.1,0.1),1),
                     labels =insert_minor_2(c("0","0.2","0.4","0.6","0.8","1.0","1.2","1.4","1.6","1.8","2.0","2.2"),2))+
  scale_y_continuous(name = "H/C",expand = c(0.01,0.01), limits = c(0.0,2.2),breaks = round(seq(0.0,2.5,0.1),1),
                     labels =insert_minor_1(c("0","0.5","1.0","1.5","2.0","2.5"),4))+
  scale_fill_manual(values = (c("#BC3C29FF","#EFC000FF","#008B45FF","#7876B1FF","grey50")),guide="none")+
  scale_colour_manual(values = (c("#BC3C29FF","#EFC000FF","#008B45FF","#7876B1FF","grey50")),guide="none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 2),
        plot.margin = unit(c(0.2,0.4,0.2,0.4),"cm"),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.text.x = element_text(size = 22, colour = "black",face = "bold", family = "Arial",margin = unit(c(0.3,0,0.3,0),"cm")),
        axis.text.y = element_text(size = 22, colour = "black",face = "bold", family = "Arial",margin = unit(c(0.0,0.3,0.0,0.3),"cm")),
        axis.title.x = element_text(size = 22, colour = "black",face = "bold", family = "Arial"),
        axis.title.y = element_text(size = 22, colour = "black",face = "bold", family = "Arial"),
        strip.text.x = element_text(size = 40, colour = "black",face = "bold",family = "Arial",margin = unit(c(0.6,0,0.4,0),"cm")),
        strip.text.y = element_text(size = 40, colour = "black",face = "bold",family = "Arial",margin = unit(c(0.2,0.6,0.4,0.2),"cm")),
        strip.background = element_blank(),
        legend.text = element_text(size = 22, colour = "black", family = "Arial",hjust = 0.5,margin = unit(c(0,0,0,0),"cm")),
        legend.spacing = unit(0.0,"cm"),
        legend.title = element_text(size = 26,hjust = 0.2 ,vjust = 0.75,colour = "black", family = "Arial",face = "bold"),
        legend.position = "right",
        legend.direction = "vertical",
        #legend.position = "bottom",
        #legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.background = element_blank(),
  )+
  ggsave(filename("vk_all"),height = 18, width = 80, units = "cm", dpi = 300, compression="lzw")

##2-2 vk_cor=====
cor_1st_sep_ul2_soc
cor_1st_sep_bj2_soc
cor_1st_sep_ss2_soc
cor_1st_sep_sul2_soc
cor_1st_sep_nt2_soc

cor_1st_soc=rbind(cor_1st_sep_ul2_soc,
                  cor_1st_sep_bj2_soc,
                  cor_1st_sep_ss2_soc,
                  cor_1st_sep_sul2_soc,
                  cor_1st_sep_nt2_soc)
cor_1st_sep_ul

soc_list=unique(cor_1st_soc[,c("Group","Formula","varlab")])
cor_test_1st_re=cor_test_1st %>% left_join(soc_list)
cor_test_1st_re$gr=ifelse(is.na(cor_test_1st_re$varlab),"gry","SOC")
cor_test_1st_grey=subset(cor_test_1st_re,cor_test_1st_re$gr=="gry")

cor_test_1st_grey$Grouplab=factor(cor_test_1st_grey$Group,
                     levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"),
                     labels = c("UB","BJ","SS","SE","NT"))

ggplot()+
  geom_hline(yintercept = 1.5,lty=2,size=1)+
  geom_abline(slope = -1, intercept = 1.1,lty=2,size=1)+
  geom_abline(slope = -0.76, intercept = 0.75, lty=2,size=1)+
  geom_point(data=cor_test_1st_grey, aes(x=O.C, y=H.C),col="grey70",size=2.5)+
  geom_point(data=cor_1st_soc, aes(x=O.C, y=H.C,col=value),size=3.5)+
  facet_rep_grid(.~Grouplab, repeat.tick.labels = "all")+
  #facet_grid(Grouplab~varlab, labeller = label_parsed )+
  scale_x_continuous(name = "O/C",expand = c(0.01,0.01),limits = c(-0.01,1.0), breaks = round(seq(0,2.1,0.1),1),
                     labels =insert_minor_2(c("0","0.2","0.4","0.6","0.8","1.0","1.2","1.4","1.6","1.8","2.0","2.2"),2))+
  scale_y_continuous(name = "H/C",expand = c(0.01,0.01), limits = c(0.0,2.2),breaks = round(seq(0.0,2.5,0.1),1),
                     labels =insert_minor_1(c("0","0.5","1.0","1.5","2.0","2.5"),4))+
  #scale_color_gdradientn(colors=(matlab.like(40)[6:34]),breaks = c(-0.5, 0, 0.5), limits = c(-0.75,0.93))+
  #scale_color_gradientn(colors=(topo.colors(20)[4:17]))+
  scale_color_gradientn(colors=c(topo.colors(40)[1:38]))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 2),
        plot.margin = unit(c(0.2,0.4,0.2,0.4),"cm"),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.text.x = element_text(size = 22, colour = "black",face = "bold", family = "Arial",margin = unit(c(0.3,0,0.3,0),"cm")),
        axis.text.y = element_text(size = 22, colour = "black",face = "bold", family = "Arial",margin = unit(c(0.0,0.3,0.0,0.3),"cm")),
        axis.title.x = element_text(size = 22, colour = "black",face = "bold", family = "Arial"),
        axis.title.y = element_text(size = 22, colour = "black",face = "bold", family = "Arial"),
        strip.text.x = element_text(size = 40, colour = "black",face = "bold",family = "Arial",margin = unit(c(0.6,0,0.4,0),"cm")),
        strip.text.y = element_text(size = 40, colour = "black",face = "bold",family = "Arial",margin = unit(c(0.2,0.6,0.4,0.2),"cm")),
        strip.background = element_blank(),
        legend.text = element_text(size = 22, colour = "black", family = "Arial",hjust = 0.5,margin = unit(c(0,0,0,0),"cm")),
        legend.spacing = unit(0.0,"cm"),
        legend.title = element_text(size = 26,hjust = 0.2 ,vjust = 0.75,colour = "black", family = "Arial",face = "bold"),
        legend.position = "right",
        legend.direction = "vertical",
        #legend.position = "bottom",
        #legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.background = element_blank(),
  )+
  guides(col=guide_colorbar(title = expression(bolditalic("ρ")),ticks.colour = "black", ticks.linewidth = 1.5,
                            barwidth = 2.5, barheight = 25))+
  ggsave(filename("soc_1st"),height = 18, width = 80, units = "cm", dpi = 300, compression="lzw")


#3-1) molecular class distribution======
frd_merge

cor_1st_soc
