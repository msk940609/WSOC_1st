
##1-1) map=====
samp=fread("Datafile/sampling_point.csv")
samp

##1-2) pm trend=====

envi_1st=fread("Datafile/FRIEND_1st_envi_re.csv")
#envi_1st=ev_1st
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

FT_envi_trend2_ul$val2=ifelse(FT_envi_trend2_ul$variable=="OC",FT_envi_trend2_ul$value*1,FT_envi_trend2_ul$value*2)
FT_envi_trend_ul$type=factor(FT_envi_trend_ul$type,levels = c("POC","SOC"))
FT_envi_trend2_ul_pm=subset(FT_envi_trend2_ul,FT_envi_trend2_ul$variable=="PM2.5")

FT_envi_trend_ul$conc2=FT_envi_trend_ul$conc*1.2

FT_envi_trend2_ul_pm
FT_envi_trend_ul
tt_ul=na.omit(FT_envi_trend2_ul_pm,cols="value" )

FT_envi_trend_ul
FT_envi_trend_ul_na=FT_envi_trend_ul[is.na(FT_envi_trend_ul$OC), ]


pmtrend_ul=ggplot()+
  #geom_blank(data = FT_envi_trend2_all_pm ,aes(x=date2, y=value*1.1 ))+
  geom_area(data = FT_envi_trend_ul ,aes(x=date2 , y=conc2,fill=type,col=type),lty=1,position = position_stack(reverse = T),size=2,na.rm = T)+
  geom_area(data = FT_envi_trend_ul ,aes(x=date2 , y=conc2,fill=type),position = position_stack(reverse = T),size=1.5,na.rm = T)+
  geom_rect(data=FT_envi_trend_ul_na,aes(xmin=date2-3600*12,xmax=date2+3600*12, ymin=-Inf, ymax=Inf), fill="white")+
  #geom_bar(data = FT_envi_trend_ul_na ,aes(x=date2 , y=PM2.5), fill="white",stat="identity", position=position_stack(),width = 86400)+
  geom_line(data = tt_ul ,aes(x=date2, y=value ),col="black",size=0.75, na.rm = T)+
  geom_point(data = FT_envi_trend2_ul_pm ,aes(x=date2, y=value ),col="black", na.rm = F, size=3)+
  #annotate(geom = "text",x=16,y=164,label="UT",size=10)+
  scale_color_manual(values = c("#9DBCD4","blue"))+
  scale_fill_manual(values =c("#9DBCD4","#CB7723"))+
  scale_x_datetime('',
                   limits = lims,
                   date_breaks = '2 day',
                   date_labels = "%b-%d", expand = c(0.01,0.01))+
  scale_y_continuous(name = expression(bold("PM"["2.5"]~"("*"\u03bcg/"*m^"3"*")")),expand = c(0.02,0.02),
                     sec.axis = sec_axis(~.*1/1.2, name = expression(bold("OC"~"("*"\u03bcg/"*m^"3"*")")),breaks = seq(0,160,40)),
                     limits = c(0,210),breaks = seq(0,160,50))+
  #ggtitle("UB")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.1,0.05,0.1,0.05),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.line.y.right = element_line(colour="blue", size=1.),  
        axis.text.x = element_text(size = 20,angle = 330,vjust = 1.0, hjust = 0.05,
                                   colour = "black", face = 2,family = "Arial"),
        axis.ticks.length = unit(0.2,"cm"),
        axis.ticks.x = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.left = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.right = element_line(size = 1.5, colour = "blue"),
        axis.title.x = element_text(size = 0.1),
        axis.text.y.left = element_text(size = 20, colour = "black" , face =2,family = "Arial"),
        axis.text.y.right = element_text(size = 20, colour = "blue" , face = 2,family = "Arial"),
        axis.title.y.left = element_text(size = 24, colour = "black",margin = unit(c(0.1,0.2,0.1,0.0),"cm"),family = "Arial"),
        axis.title.y.right = element_text(size = 24, colour = "blue",margin = unit(c(0.1,-0.2,0.1,0.2),"cm"),family = "Arial"),
        legend.text = element_text(size = 18, colour = "black",family = "Arial",margin = unit(c(0.1,0.3,0.1,0.1),"cm"), hjust = 0.5,vjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = c(0.90,1.07))+
  xlab("")+
  guides(col="none",shape="none", fill="none")
#  guides(col="none",shape="none", fill=guide_legend(title = "", reverse = F))
#  ggsave(filename("Ulan_pm_trend_all"),height = 10, width = 40, units = "cm", dpi = 300)

pmtrend_ul

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

FT_envi_trend_bj_na=FT_envi_trend_bj[is.na(FT_envi_trend_bj$OC), ]


pmtrend_bj=ggplot()+
  #geom_rect(aes(xmin=1,xmax=34, ymax=Inf, ymin=-Inf),alpha=0.4,fill="grey50")+
  #geom_rect(aes(xmin=42.5,xmax=73.5, ymax=Inf, ymin=-Inf),alpha=0.4,fill="grey50")+
  #geom_rect(aes(xmin=86.5,xmax=118, ymax=Inf, ymin=-Inf),alpha=0.4,fill="grey50")+
  geom_area(data = FT_envi_trend_bj ,aes(x=date2 , y=conc2,fill=type,col=type),lty=1,position = position_stack(reverse = T),size=2,na.rm = T)+
  geom_area(data = FT_envi_trend_bj ,aes(x=date2 , y=conc2,fill=type),position = position_stack(reverse = T),size=1.5,na.rm = T)+
  geom_rect(data=FT_envi_trend_bj_na,aes(xmin=date2-3600*12,xmax=date2+3600*12, ymin=-Inf, ymax=Inf), fill="white")+
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
                     limits = c(0,80),breaks = seq(0,160,20))+
  #ggtitle("BJ")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.1,0.05,0.1,0.05),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.line.y.right = element_line(colour="blue", size=1.),  
        axis.text.x = element_text(size = 20,angle = 330,vjust = 1.0, hjust = 0.05,
                                   colour = "black", face = 2,family = "Arial"),
        axis.ticks.length = unit(0.2,"cm"),
        axis.ticks.x = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.left = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.right = element_line(size = 1.5, colour = "blue"),
        axis.title.x = element_text(size = 0.1),
        axis.text.y.left = element_text(size = 20, colour = "black" , face =2,family = "Arial"),
        axis.text.y.right = element_text(size = 20, colour = "blue" , face = 2,family = "Arial"),
        axis.title.y.left = element_text(size = 24, colour = "black",margin = unit(c(0.1,0.2,0.1,0.0),"cm"),family = "Arial"),
        axis.title.y.right = element_text(size = 24, colour = "blue",margin = unit(c(0.1,-0.2,0.1,0.2),"cm"),family = "Arial"),
        legend.text = element_text(size = 18, colour = "black",family = "Arial",margin = unit(c(0.1,0.3,0.1,0.1),"cm"), hjust = 0.5,vjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = c(0.90,1.07))+
  xlab("")+
  guides(col="none",shape="none", fill="none")

#  guides(col="none",shape="none", fill=guide_legend(title = "", reverse = F))
#  ggsave(filename("bj_pm_trend_all"),height = 10, width = 40, units = "cm", dpi = 300)

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
FT_envi_trend_ss_na=FT_envi_trend_ss[is.na(FT_envi_trend_ss$OC), ]


pmtrend_ss=ggplot()+
  geom_area(data = FT_envi_trend_ss ,aes(x=date2 , y=conc2,fill=type,col=type),lty=1,position = position_stack(reverse = T),size=2,na.rm = T)+
  geom_area(data = FT_envi_trend_ss ,aes(x=date2 , y=conc2,fill=type),position = position_stack(reverse = T),size=1.5,na.rm = T)+
  geom_rect(data=FT_envi_trend_ss_na,aes(xmin=date2-3600*12,xmax=date2+3600*12, ymin=-Inf, ymax=Inf), fill="white")+
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
                     limits = c(0,80),breaks = seq(0,160,20))+
  #ggtitle("SS")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.1,0.05,0.1,0.05),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.line.y.right = element_line(colour="blue", size=1.),  
        axis.text.x = element_text(size = 20,angle = 330,vjust = 1.0, hjust = 0.05,
                                   colour = "black", face = 2,family = "Arial"),
        axis.ticks.length = unit(0.2,"cm"),
        axis.ticks.x = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.left = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.right = element_line(size = 1.5, colour = "blue"),
        axis.title.x = element_text(size = 0.1),
        axis.text.y.left = element_text(size = 20, colour = "black" , face =2,family = "Arial"),
        axis.text.y.right = element_text(size = 20, colour = "blue" , face = 2,family = "Arial"),
        axis.title.y.left = element_text(size = 24, colour = "black",margin = unit(c(0.1,0.2,0.1,0.0),"cm"),family = "Arial"),
        axis.title.y.right = element_text(size = 24, colour = "blue",margin = unit(c(0.1,-0.2,0.1,0.2),"cm"),family = "Arial"),
        legend.text = element_text(size = 18, colour = "black",family = "Arial",margin = unit(c(0.1,0.3,0.1,0.1),"cm"), hjust = 0.5,vjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = c(0.90,1.07))+
  xlab("")+
  guides(col="none",shape="none", fill="none")
#  guides(col="none",shape="none", fill=guide_legend(title = "", reverse = F))
#  ggsave(filename("ss_pm_trend_all"),height = 10, width = 40, units = "cm", dpi = 300)

### 4)sul====
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
                     limits = c(0,80),breaks = seq(0,160,20))+
 # ggtitle("SE")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.1,0.05,0.1,0.05),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.line.y.right = element_line(colour="blue", size=1.),  
        axis.text.x = element_text(size = 20,angle = 330,vjust = 1.0, hjust = 0.05,
                                   colour = "black", face = 2,family = "Arial"),
        axis.ticks.length = unit(0.2,"cm"),
        axis.ticks.x = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.left = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.right = element_line(size = 1.5, colour = "blue"),
        axis.title.x = element_text(size = 0.1),
        axis.text.y.left = element_text(size = 20, colour = "black" , face =2,family = "Arial"),
        axis.text.y.right = element_text(size = 20, colour = "blue" , face = 2,family = "Arial"),
        axis.title.y.left = element_text(size = 24, colour = "black",margin = unit(c(0.1,0.2,0.1,0.0),"cm"),family = "Arial"),
        axis.title.y.right = element_text(size = 24, colour = "blue",margin = unit(c(0.1,-0.2,0.1,0.2),"cm"),family = "Arial"),
        legend.text = element_text(size = 18, colour = "black",family = "Arial",margin = unit(c(0.1,0.3,0.1,0.1),"cm"), hjust = 0.5,vjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = c(0.90,1.07))+
  xlab("")+
  guides(col="none",shape="none", fill="none")
#  guides(col="none",shape="none", fill=guide_legend(title = "", reverse = F))
#  ggsave(filename("sul_pm_trend_all"),height = 10, width = 40, units = "cm", dpi = 300)


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

FT_envi_trend_nt_na=FT_envi_trend_nt[is.na(FT_envi_trend_nt$conc), ]


pmtrend_nt=ggplot()+
  geom_area(data = FT_envi_trend_nt ,aes(x=date2 , y=conc2,fill=type,col=type),lty=1,position = position_stack(reverse = T),size=2,na.rm = T)+
  geom_area(data = FT_envi_trend_nt ,aes(x=date2 , y=conc2,fill=type),position = position_stack(reverse = T),size=1.5,na.rm = T)+
  geom_rect(data=FT_envi_trend_nt_na,aes(xmin=date2-3600*12,xmax=date2+3600*12, ymin=-Inf, ymax=Inf), fill="white")+
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
                     limits = c(0,10), breaks = seq(0,20,2))+
  #ggtitle("NT")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.1,0.05,0.1,0.05),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.line.y.right = element_line(colour="blue", size=1.),  
        axis.text.x = element_text(size = 20,angle = 330,vjust = 1.0, hjust = 0.05,
                                   colour = "black", face = 2,family = "Arial"),
        axis.ticks.length = unit(0.2,"cm"),
        axis.ticks.x = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.left = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.right = element_line(size = 1.5, colour = "blue"),
        axis.title.x = element_text(size = 0.1),
        axis.text.y.left = element_text(size = 20, colour = "black" , face =2,family = "Arial"),
        axis.text.y.right = element_text(size = 20, colour = "blue" , face = 2,family = "Arial"),
        axis.title.y.left = element_text(size = 24, colour = "black",margin = unit(c(0.1,0.2,0.1,0.0),"cm"),family = "Arial"),
        axis.title.y.right = element_text(size = 24, colour = "blue",margin = unit(c(0.1,-0.2,0.1,0.2),"cm"),family = "Arial"),
        legend.text = element_text(size = 18, colour = "black",family = "Arial",margin = unit(c(0.1,0.3,0.1,0.1),"cm"), hjust = 0.5,vjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = c(0.90,1.07))+
  xlab("")+
  guides(col="none",shape="none", fill="none")
#  guides(col="none",shape="none", fill=guide_legend(title = "", reverse = F))
#  ggsave(filename("nt_pm_trend_all"),height = 10, width = 40, units = "cm", dpi = 300)

pmtrend_all=pmtrend_ul/pmtrend_bj/pmtrend_ss/pmtrend_sul/pmtrend_nt
pmtrend_all+ggsave(filename("pm_trend_all"),height = 40, width = 40, units = "cm", dpi = 300, compression="lzw")


#pm trend all region======
FT_envi_trend

pmtrend_all=ggplot()+
  geom_line(data = FT_envi_trend ,aes(x=date2, y=PM2.5, col=Group),size=0.75, na.rm = T, lwd=2)+
  geom_point(data = FT_envi_trend ,aes(x=date2, y=PM2.5, col=Group, shape=Group, fill=Group), na.rm = F, size=6,stroke = 2)+
  scale_color_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  scale_fill_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  scale_shape_manual(values = c(23,22,21,24,4))+
  scale_x_datetime('',
                   limits = lims,
                   date_breaks = '2 day',
                   date_labels = "%b-%d", expand = c(0.01,0.01))+
  scale_y_continuous(name = expression(bold("PM"["2.5"]~"("*"\u03bcg/"*m^"3"*")")),expand = c(0.02,0.02),breaks = seq(0,160,20))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.1,0.05,0.1,0.05),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.line.y.right = element_line(colour="black", size=1.),  
        axis.text.x = element_text(size = 20,angle = 330,vjust = 1.0, hjust = 0.05,
                                   colour = "black", face = 2,family = "Arial"),
        axis.ticks.length = unit(0.2,"cm"),
        axis.ticks.x = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.left = element_line(size = 1.5, colour = "black"),
        axis.title.x = element_text(size = 0.1),
        axis.text.y.left = element_text(size = 20, colour = "black" , face =2,family = "Arial"),
        axis.title.y.left = element_text(size = 24, colour = "black",margin = unit(c(0.1,0.2,0.1,0.0),"cm"),family = "Arial"),
        legend.text = element_text(size = 18, colour = "black",family = "Arial",margin = unit(c(0.1,0.3,0.1,0.1),"cm"), hjust = 0.5,vjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = c(0.90,1.07))+
  xlab("")+
  guides(col="none",shape="none", fill="none")

pmtrend_all+ggsave(filename("pm_trend_allsite"),height = 20, width = 40, units = "cm", dpi = 300, compression="lzw")

FT_envi_trend

pmtrend_all=ggplot()+
  geom_line(data = FT_envi_trend ,aes(x=date2, y=PM2.5, col=Group),size=0.75, na.rm = T, lwd=1.5)+
  geom_point(data = FT_envi_trend ,aes(x=date2, y=PM2.5, col=Group, shape=Group, fill=Group), na.rm = F, size=6,stroke = 2)+
  scale_color_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  scale_fill_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  scale_shape_manual(values = c(23,22,21,24,4))+
  scale_x_datetime('',
                   limits = lims,
                   date_breaks = '2 day',
                   date_labels = "%b-%d", expand = c(0.01,0.01))+
  scale_y_continuous(name = expression(bold("PM"["2.5"]~"("*"\u03bcg/"*m^"3"*")")),expand = c(0.1,0.1))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(5.5,0.05,0.1,0.05),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.line.y.right = element_line(colour="black", size=1.),  
        axis.text.x = element_text(size = 20,angle = 330,vjust = 1.0, hjust = 0.05,
                                   colour = "black", face = 2,family = "Arial"),
        axis.ticks.length = unit(0.2,"cm"),
        axis.ticks.x = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.left = element_line(size = 1.5, colour = "black"),
        axis.title.x = element_text(size = 0.1),
        axis.text.y.left = element_text(size = 20, colour = "black" , face =2,family = "Arial"),
        axis.title.y.left = element_text(size = 24, colour = "black",margin = unit(c(0.1,0.2,0.1,0.0),"cm"),family = "Arial"),
        legend.text = element_text(size = 18, colour = "black",family = "Arial",margin = unit(c(0.1,0.3,0.1,0.1),"cm"), hjust = 0.5,vjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = c(0.90,1.07))+
  xlab("")+
  coord_cartesian(ylim = c(0,80))+
  guides(col="none",shape="none", fill="none")

pmtrend_all+ggsave(filename("pm_trend_allsite_under"),height = 20, width = 40, units = "cm", dpi = 300, compression="lzw")


ggplot(FT_envi_trend)+
  geom_histogram(aes(x=PM2.5), binwidth = 5)+
  scale_x_continuous(breaks = seq(0,160,20))

pmtrend_all_upper=ggplot()+
  geom_line(data = FT_envi_trend ,aes(x=date2, y=PM2.5, col=Group),size=0.75, na.rm = T, lwd=1.5)+
  geom_point(data = FT_envi_trend ,aes(x=date2, y=PM2.5, col=Group, shape=Group, fill=Group), na.rm = F, size=6,stroke = 2)+
  scale_color_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  scale_fill_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  scale_shape_manual(values = c(23,22,21,24,4))+
  scale_x_datetime('',
                   limits = lims,
                   date_breaks = '2 day',
                   date_labels = "%b-%d", expand = c(0.01,0.01))+
  scale_y_continuous(name = expression(bold("PM"["2.5"]~"("*"\u03bcg/"*m^"3"*")")),expand = c(0.1,0.1),breaks = seq(0,160,40))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.1,0.05,0.1,0.05),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.line.y.right = element_line(colour="black", size=1.),  
        axis.text.x = element_text(size = 20,angle = 330,vjust = 1.0, hjust = 0.05,
                                   colour = "black", face = 2,family = "Arial"),
        axis.ticks.length = unit(0.2,"cm"),
        axis.ticks.x = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.left = element_line(size = 1.5, colour = "black"),
        axis.title.x = element_text(size = 0.1),
        axis.text.y.left = element_text(size = 20, colour = "black" , face =2,family = "Arial"),
        axis.title.y.left = element_text(size = 24, colour = "black",margin = unit(c(0.1,0.2,0.1,0.0),"cm"),family = "Arial"),
        legend.text = element_text(size = 18, colour = "black",family = "Arial",margin = unit(c(0.1,0.3,0.1,0.1),"cm"), hjust = 0.5,vjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = c(0.90,1.07))+
  xlab("")+
  #coord_cartesian(ylim = c(100,160))+
  guides(col="none",shape="none", fill="none")
pmtrend_all_upper+ggsave(filename("pm_trend_allsite_upper"),height = 20, width = 40, units = "cm", dpi = 300, compression="lzw")

pmtrend_all_upper/pmtrend_all+ggsave(filename("pm_trend_allsite_all"),height = 40, width = 40, units = "cm", dpi = 300, compression="lzw")
