cor_all

frd_merge_soc=frd_merge %>% left_join(cor_all[,c("Group","Formula","Type")])
frd_merge_soc$Type2=ifelse(is.na(frd_merge_soc$Type),"WSOC",frd_merge_soc$Type)

#frd_merge_wsoc=frd_merge
#frd_merge_soc_fin=subset(frd_merge_soc,frd_merge_soc$Type2=="SOC")

frd_comp=aggregate(frd_merge_soc$Bromo.Inty, by=list(Group=frd_merge_soc$Group, No=frd_merge_soc$No, Type2=frd_merge_soc$Type2, Comp=frd_merge_soc$Comp),sum)
frd_comp_tot=aggregate(frd_merge_soc$Bromo.Inty, by=list(Group=frd_merge_soc$Group, No=frd_merge_soc$No,Type2=frd_merge_soc$Type2),sum) %>% `colnames<-`(c("Group","No","Type2","tot"))
#frd_comp_tot=aggregate(frd_merge_soc$Bromo.Inty, by=list(Group=frd_merge_soc$Group, No=frd_merge_soc$No,Type2=frd_merge_soc$Type2),sum) %>% `colnames<-`(c("Group","No","Type2","tot"))

frd_comp=frd_comp %>% left_join(frd_comp_tot)
frd_comp$rel=frd_comp$x/frd_comp$tot*100

frd_comp_sel=subset(frd_comp,frd_comp$Comp!="Remainders")
frd_comp_sel$Typelab=factor(frd_comp_sel$Type2, levels = c("WSOC","SOC"))
frd_comp_sel$Grouplab=factor(frd_comp_sel$Group, levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"),
                             labels = c("UT","BJ","SS","SE","NT"))

ggplot()+
  #geom_boxplot(data = frd_comp_sel, aes(x=Grouplab, y=rel, fill=Typelab))+
  #facet_rep_wrap(.~Comp,repeat.tick.labels = T, scales = "free")+
  stat_boxplot(data = frd_comp_sel, aes(x=Comp, y=rel, fill=Typelab), geom = "errorbar", position = position_dodge(width = 0.75),width = 0.5)+
  geom_boxplot(data = frd_comp_sel, aes(x=Comp, y=rel, fill=Typelab),outlier.colour = NA)+
  facet_rep_wrap(.~Grouplab,repeat.tick.labels = T, scales = "fixed")+
  scale_fill_manual(values =c("#9DBCD4","#CB7723"))+
  scale_y_continuous(name = "Chemical composition (%)")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 28, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.1,0.05,0.1,0.05),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.line.y.right = element_line(colour="blue", size=1.),  
        axis.text.x = element_text(size = 20,angle = 0,vjust = 0.5, hjust = 0.5,
                                   colour = "black", face = 2,family = "Arial"),
        axis.ticks.length = unit(0.2,"cm"),
        axis.ticks.x = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.left = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.right = element_line(size = 1.5, colour = "blue"),
        axis.title.x = element_text(size = 0.1),
        axis.text.y.left = element_text(size = 20, colour = "black" , face =2,family = "Arial",margin = unit(c(0.1,0.2,0.1,0.2),"cm")),
        axis.text.y.right = element_text(size = 20, colour = "blue" , face = 2,family = "Arial"),
        axis.title.y.left = element_text(size = 24, colour = "black",face = 2,margin = unit(c(0.1,0.2,0.1,0.2),"cm"),family = "Arial"),
        axis.title.y.right = element_text(size = 24, colour = "blue",margin = unit(c(0.1,0.2,0.1,0.2),"cm"),family = "Arial"),
        legend.text = element_text(size = 18, colour = "black",family = "Arial",margin = unit(c(0.1,0.3,0.1,0.1),"cm"), hjust = 0.5,vjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = c(0.90,1.07))+
  xlab("")+
  guides(col="none",shape="none", fill="none")+
  ggsave(filename("comp_dis_all"),height = 25, width = 45, units = "cm", dpi = 300, compression="lzw")

##chp====
frd_merge_soc$NO3=frd_merge_soc$O./3*frd_merge_soc$N.
frd_merge_soc$SO4=frd_merge_soc$O./4*frd_merge_soc$S.
frd_merge_soc$NOS=frd_merge_soc$O./(3*frd_merge_soc$`N.`+4*frd_merge_soc$`S.`)

frd_merge_soc_sel=subset(frd_merge_soc,frd_merge_soc$NOS!="Inf")

frd_merge_soc_chp=melt(frd_merge_soc_sel[,c("Group","No","Type2","AI","DBE","O.C","H.C","NO3","SO4","NOS")],
                       id.vars=c("Group","No","Type2"), na.rm = T) %>% 
  dcast(Group+No+Type2~variable, mean) %>% 
  melt(id.vars=c("Group","No","Type2"))


frd_merge_soc_sel$O.S=frd_merge_soc_sel$O./frd_merge_soc_sel$S.
frd_merge_soc_sel$O.N=frd_merge_soc_sel$O./frd_merge_soc_sel$N.

#frd_merge_soc_chp=melt(frd_merge_soc_sel[,c("Group","No","Type2","AI","DBE","O.C","H.C","N.C","S.C")],
#                       id.vars=c("Group","No","Type2"), na.rm = T) %>% 
#  dcast(Group+No+Type2~variable, mean) %>% 
#  melt(id.vars=c("Group","No","Type2"))


frd_merge_soc_chp
frd_merge_soc_chp$Typelab=factor(frd_merge_soc_chp$Type2, levels = c("WSOC","SOC"))
frd_merge_soc_chp$Grouplab=factor(frd_merge_soc_chp$Group, levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"),
                             labels = c("UT","BJ","SS","SE","NT"))

frd_merge_soc_chp$varlab=factor(frd_merge_soc_chp$variable, levels = c("AI","DBE","H.C","O.C","NO3","SO4","NOS"),
                                labels = c("Mean AI","Mean DBE","Mean H/C","Mean O/C","Mean O/3N","Mean O/4S","Mean O/(3N+4S)"))

#frd_merge_soc_chp$varlab=factor(frd_merge_soc_chp$variable, levels = c("AI","DBE","H.C","O.C","N.C","S.C"),
#                                labels = c("Mean AI","Mean DBE","Mean H/C","Mean O/C","Mean N/C","Mean O/4S","Mean O/(3N+4S)"))
table(frd_merge_soc_chp$varlab)

ggplot()+
  stat_boxplot(data = frd_merge_soc_chp, aes(x=Grouplab, y=value, fill=Typelab), geom = "errorbar", position = position_dodge(width = 0.75),width = 0.5)+
  geom_boxplot(data = frd_merge_soc_chp, aes(x=Grouplab, y=value, fill=Typelab))+
  facet_rep_wrap(.~varlab,repeat.tick.labels = T, scales = "free", ncol=4,switch = "y",strip.position = "left")+
  scale_fill_manual(values =c("#9DBCD4","#CB7723"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.placement = "outside",
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.line.y.right = element_line(colour="blue", size=1.),  
        axis.text.x = element_text(size = 20,angle = 0,vjust = 0.5, hjust = 0.5,
                                   colour = "black", face = 2,family = "Arial",margin = unit(c(0.1,0.2,0.3,0.2),"cm")),
        axis.ticks.length = unit(0.2,"cm"),
        axis.ticks.x = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.left = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.right = element_line(size = 1.5, colour = "blue"),
        axis.title.x = element_text(size = 0.1),
        axis.text.y.left = element_text(size = 20, colour = "black" , face =2,family = "Arial"),
        axis.text.y.right = element_text(size = 20, colour = "blue" , face = 2,family = "Arial"),
        axis.title.y.left = element_text(size = 0.1, colour = "black",margin = unit(c(0.1,0.2,0.1,0.0),"cm"),family = "Arial"),
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
  guides(col="none",shape="none", fill="none")+
  ggsave(filename("chp_dis_all"),height = 22, width = 55, units = "cm", dpi = 300, compression="lzw")

###chemodiversity=====
envi_1st=fread("Datafile/FRIEND_1st_envi_re.csv")

frd_merge_soc$Sample=paste(frd_merge_soc$Group,frd_merge_soc$No, sep = "_")
frd_merge_soc2=frd_merge_soc %>% left_join(envi_1st[,c("Sample","Event")])

table(frd_merge_soc$Sample)
table(envi_1st$Sample)

frd_merge_soc

chd_all=aggregate(frd_merge_soc$Freq, by=list(Group=frd_merge_soc$Group, No=frd_merge_soc$No),sum)
chd_all$Type2="All"
chd_all$Type2
chd_all=chd_all[,c("Group","No","Type2","x")]

chd=aggregate(frd_merge_soc$Freq, by=list(Group=frd_merge_soc$Group, No=frd_merge_soc$No, Type2=frd_merge_soc$Type2),sum)
chd

chd_merge=rbind(chd_all,chd)
chd_merge_fin=chd_merge
chd_merge_fin$Sample=paste(chd_merge_fin$Group,chd_merge_fin$No, sep = "_")
chd_merge_fin=chd_merge_fin %>% left_join(envi_1st[,c("Sample","Event","PM2.5","Date")])

chd_merge_fin
chd_merge_fin$Group

chd_merge_fin$Grouplab=factor(chd_merge_fin$Group, levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"),
                             labels = c("UT","BJ","SS","SE","NT"))

chd_merge_fin=chd_merge_fin %>% 
  mutate(date2 = as.POSIXct(chd_merge_fin$Date, format = '%Y-%m-%d %H:%M'))

ggplot(chd_merge_fin, aes(x=No, y=x,col=Type2))+
  geom_point()+
  facet_wrap(.~Grouplab, scales = "free")

lims <- as.POSIXct(strptime(c("2020-12-15 6:00","2021-01-14 12:00"), format = "%Y-%m-%d %H:%M"))

min(chd_merge_fin$PM2.5)
max(chd_merge_fin$PM2.5)

min(chd_merge_fin$x)
max(chd_merge_fin$x)

chd_merge_fin$prop=(chd_merge_fin$x-min(chd_merge_fin$x))/(23.68195523)+0.2

min(chd_merge_fin$prop)
max(chd_merge_fin$prop)

ggchd <- ggplot()+
  geom_line(data = chd_merge_fin,aes(x=date2, y=prop, col=Type2), na.rm = F, size=1)+
  geom_point(data = chd_merge_fin,aes(x=date2, y=prop, col=Type2, shape=Type2, fill=Type2), na.rm = F, size=6,stroke = 2)+
  geom_line(data = subset(chd_merge_fin,chd_merge_fin$Type2=="All"),aes(x=date2, y=PM2.5),col="black", na.rm = F, size=1)+
  geom_point(data = subset(chd_merge_fin,chd_merge_fin$Type2=="All"),aes(x=date2, y=PM2.5),col="black", na.rm = F, size=6,stroke = 2)+
  #scale_color_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  #scale_fill_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  #scale_shape_manual(values = c(23,22,21,24,4))+
  facet_wrap(.~Grouplab, scales = "fixed", ncol=1)+
  scale_x_datetime('',
                   limits = lims,
                   date_breaks = '2 day',
                   date_labels = "%b-%d", expand = c(0.01,0.01))+
  scale_y_continuous(name = expression(bold("PM"["2.5"]~"("*"\u03bcg/"*m^"3"*")")),
                     sec.axis = sec_axis(~.*23.68195523, name = expression(bold("Chemodiveristy"))),
                     expand = c(0.02,0.02),breaks = seq(0,160,40))+
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

ggchd+ggsave(filename("chd"),height = 40, width = 40, units = "cm", dpi = 300, compression="lzw")

###1)ut====
FT_envi_trend
table(FT_envi_trend2$variable)
FT_envi_trend_ut=subset(FT_envi_trend,FT_envi_trend$Group=="Ulaanbaatar")
FT_envi_trend_ut$type=factor(FT_envi_trend_ut$type,levels = c("POC","SOC"))

pmtrend_ut=ggplot()+
  geom_bar(data = FT_envi_trend_ut ,aes(x=date2 , y=conc,fill=type),position = position_stack(reverse = F),size=1.5,na.rm = T, stat = "identity")+
  #annotate(geom = "text",x=16,y=164,label="UT",size=10)+
  scale_color_manual(values = c("#9DBCD4","blue"))+
  scale_fill_manual(values =c("#9DBCD4","#CB7723"))+
  scale_x_datetime('',
                   limits = lims,
                   date_breaks = '2 day',
                   date_labels = "%m-%d", expand = c(0.01,0.01))+
  scale_y_continuous(name = expression(bold("OC"~"("*"\u03bcg/"*m^"3"*")")),expand = c(0.02,0.02),
                     breaks = seq(0,160,40),
                     limits = c(0,80))+
  #ggtitle("UB")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.1,0.05,0.1,0.05),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.line.y.right = element_line(colour="blue", size=1.),  
        axis.text.x = element_text(size = 20,angle = 0,vjust = 0.5, hjust = 0.5,
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

pmtrend_ut

chd_merge_fin
chd_merge_soc=chd_merge_fin %>% filter(Type2%in%c("WSOC","SOC"))
chd_merge_soc_ut=subset(chd_merge_soc,chd_merge_soc$Grouplab=="UT")

chdtrend_ut=ggplot()+
  geom_bar(data = chd_merge_soc_ut ,aes(x=date2 , y=x,fill=Type2),position = position_stack(reverse = T),size=1.5,na.rm = T, stat = "identity")+
  scale_fill_manual(values =c("#CB7723","#9DBCD4"))+
  scale_x_datetime('',
                   limits = lims,
                   date_breaks = '2 day',
                   date_labels = "%m-%d", expand = c(0.01,0.01), position="top")+
#  scale_y_continuous(name = expression(bold("Chemodiveristy")),expand = c(0.02,0.02),
#                   )+
  scale_y_reverse(name = expression(bold("Chemodiveristy")),expand = c(0.02,0.02))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.1,0.05,0.1,0.05),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.line.y.right = element_line(colour="blue", size=1.),  
        axis.text.x = element_text(size = 20,angle = 0,vjust = 0.5, hjust = 0.5,
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

chdtrend_ut

pmtrend_ut/chdtrend_ut=ggsave(filename("ut_oc&chd"),height = 40, width = 40, units = "cm", dpi = 300, compression="lzw")


###1)bj====
FT_envi_trend
table(FT_envi_trend2$variable)
FT_envi_trend_bj=subset(FT_envi_trend,FT_envi_trend$Group=="Beijing")
FT_envi_trend_bj$type=factor(FT_envi_trend_bj$type,levels = c("POC","SOC"))

pmtrend_bj=ggplot()+
  geom_bar(data = FT_envi_trend_bj ,aes(x=date2 , y=conc,fill=type),position = position_stack(reverse = F),size=1.5,na.rm = T, stat = "identity")+
  #annotate(geom = "text",x=16,y=164,label="bj",size=10)+
  scale_color_manual(values = c("#9DBCD4","blue"))+
  scale_fill_manual(values =c("#9DBCD4","#CB7723"))+
  scale_x_datetime('',
                   limits = lims,
                   date_breaks = '2 day',
                   date_labels = "%m-%d", expand = c(0.01,0.01))+
  scale_y_continuous(name = expression(bold("OC"~"("*"\u03bcg/"*m^"3"*")")),expand = c(0.02,0.02))+
  #ggtitle("UB")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.1,0.05,0.1,0.05),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.line.y.right = element_line(colour="blue", size=1.),  
        axis.text.x = element_text(size = 20,angle = 0,vjust = 0.5, hjust = 0.5,
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

pmtrend_bj

chd_merge_fin
chd_merge_soc=chd_merge_fin %>% filter(Type2%in%c("WSOC","SOC"))
chd_merge_soc_bj=subset(chd_merge_soc,chd_merge_soc$Grouplab=="BJ")

chdtrend_bj=ggplot()+
  geom_bar(data = chd_merge_soc_bj ,aes(x=date2 , y=x,fill=Type2),position = position_stack(reverse = T),size=1.5,na.rm = T, stat = "identity")+
  scale_fill_manual(values =c("#CB7723","#9DBCD4"))+
  scale_x_datetime('',
                   limits = lims,
                   date_breaks = '2 day',
                   date_labels = "%m-%d", expand = c(0.01,0.01), position="top")+
  #  scale_y_continuous(name = expression(bold("Chemodiveristy")),expand = c(0.02,0.02),
  #                   )+
  scale_y_reverse(name = expression(bold("Chemodiveristy")),expand = c(0.02,0.02))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.1,0.05,0.1,0.05),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.line.y.right = element_line(colour="blue", size=1.),  
        axis.text.x = element_text(size = 20,angle = 0,vjust = 0.5, hjust = 0.5,
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

chdtrend_bj

pmtrend_bj/chdtrend_bj+ggsave(filename("bj_oc&chd"),height = 40, width = 40, units = "cm", dpi = 300, compression="lzw")


###Freq========
chd_wsoc_freq=aggregate(frd_merge_soc$Freq, by=list(Group=frd_merge_soc$Group, No=frd_merge_soc$No),sum)
chd_wsoc_freq$Type2="WSOC (Freq)"
chd_wsoc_freq=chd_wsoc_freq[,c("Group","No","Type2","x")]

chd_freq=aggregate(frd_merge_soc$Freq, by=list(Group=frd_merge_soc$Group, No=frd_merge_soc$No, Type2=frd_merge_soc$Type2),sum)
chd_freq
chd_soc_freq=subset(chd_freq,chd_freq$Type2=="SOC")
chd_soc_freq$Type2="SOC (Freq)"

chd_merge_freq=rbind(chd_wsoc_freq,chd_soc_freq)
chd_merge_freq$Sample=paste(chd_merge_freq$Group, chd_merge_freq$No, sep = "_")
chd_merge_freq=chd_merge_freq %>% left_join()


chd_merge_freq_fin=chd_merge_freq %>% left_join(envi_1st[,c("Sample","Event")])

chd_merge_freq_fin$Grouplab=factor(chd_merge_freq_fin$Group, levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"),
                              labels = c("UT","BJ","SS","SE","NT"))


chd_merge_freq_fin$Type2=factor(chd_merge_freq_fin$Type2, levels = c("WSOC (Freq)","SOC (Freq)"))
chd_merge_freq_fin$Event=factor(chd_merge_freq_fin$Event, levels = c("Event", "Normal","Non-event"))

chd_merge_freq_fin_ev=subset(chd_merge_freq_fin,chd_merge_freq_fin$Event!="Normal") %>% droplevels()

ggplot(chd_merge_freq_fin_ev, aes(x=Grouplab, y=x, fill=Event))+
  stat_boxplot(geom = "errorbar",aes(x=Grouplab, y=x, fill=Event), position = position_dodge(width = 0.75), width=0.5)+
  geom_boxplot(position = position_dodge2())+
  scale_x_discrete(name="")+
  scale_y_continuous(name = "")+
  scale_fill_manual(values = c("grey50","white"))+
  facet_wrap(.~Type2, scales = "free", ncol=1)+
  theme_bw()+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.1,0.05,0.1,0.05),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.line.y.right = element_line(colour="blue", size=1.),  
        axis.text.x = element_text(size = 14,angle = 0,vjust = 0.5, hjust = 0.5,
                                   colour = "black", face = 2,family = "Arial"),
        axis.ticks.length = unit(0.2,"cm"),
        axis.ticks.x = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.left = element_line(size = 1.5, colour = "black"),
        axis.ticks.y.right = element_line(size = 1.5, colour = "blue"),
        axis.title.x = element_text(size = 0.1),
        axis.text.y.left = element_text(size = 14, colour = "black" , face =2,family = "Arial"),
        axis.text.y.right = element_text(size = 14, colour = "blue" , face = 2,family = "Arial"),
        axis.title.y.left = element_text(size = 0.1, colour = "black",margin = unit(c(0.1,0.2,0.1,0.0),"cm"),family = "Arial"),
        axis.title.y.right = element_text(size = 24, colour = "blue",margin = unit(c(0.1,-0.2,0.1,0.2),"cm"),family = "Arial"),
        legend.text = element_text(size = 12, colour = "black",family = "Arial",margin = unit(c(0.2,0.2,0.2,0.2),"cm"), hjust = 0.5,vjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        #legend.key.width = unit(1.5,"cm"),
        #legend.key.height = unit(0.5,"cm"),
        legend.direction = "vertical",
        legend.background = element_blank(),
        legend.position = c(0.13,0.07))+
  xlab("")+
  ggsave(filename("Chemodiversitycompare"),height = 15, width = 15, units = "cm", dpi = 300, compression="lzw")
  



###Inty========
chd_wsoc_Inty=aggregate(frd_merge_soc$Bromo.Inty, by=list(Group=frd_merge_soc$Group, No=frd_merge_soc$No),sum)
chd_wsoc_Inty$Type2="WSOC (Inty)"
chd_wsoc_Inty=chd_wsoc_Inty[,c("Group","No","Type2","x")]

chd_Inty=aggregate(frd_merge_soc$Bromo.Inty, by=list(Group=frd_merge_soc$Group, No=frd_merge_soc$No, Type2=frd_merge_soc$Type2),sum)
chd_Inty
chd_soc_Inty=subset(chd_Inty,chd_Inty$Type2=="SOC")
chd_soc_Inty$Type2="SOC (Inty)"

chd_merge_Inty=rbind(chd_wsoc_Inty,chd_soc_Inty)

chd_merge_info=rbind(chd_merge_freq,chd_merge_Inty)
chd_merge_info$Sample=paste(chd_merge_info$Group,chd_merge_info$No, sep = "_")

chd_merge_info_d=dcast(chd_merge_info,Sample+Group+No~Type2,value.var = "x",sum)

envi_1st
chd_merge_info_fin=chd_merge_info_d %>% left_join(envi_1st[,c("Sample","PM2.5","WSOC","SOC")])
chd_merge_info_fin=chd_merge_info_fin[,c("Sample","Group","PM2.5","WSOC","SOC","WSOC (Freq)","SOC (Freq)","WSOC (Inty)","SOC (Inty)")]


chd_merge_info_fin_ut=subset(chd_merge_info_fin,chd_merge_info_fin$Group=="Ulaanbaatar")
chd_merge_info_fin_bj=subset(chd_merge_info_fin,chd_merge_info_fin$Group=="Beijing")
chd_merge_info_fin_ss=subset(chd_merge_info_fin,chd_merge_info_fin$Group=="Seosan")
chd_merge_info_fin_se=subset(chd_merge_info_fin,chd_merge_info_fin$Group=="Seoul")
chd_merge_info_fin_nt=subset(chd_merge_info_fin,chd_merge_info_fin$Group=="Noto")

chart.Correlation(chd_merge_info_fin_ut[,c("PM2.5","WSOC","SOC","WSOC (Freq)","SOC (Freq)","WSOC (Inty)","SOC (Inty)")],
                  method = "pearson",exact=F, label.pos=0.65,blk=1.6, na.rm=T)

tiff(filename = "Figure/ut_cor_organic_ms.tiff", width = 20,height = 20, units = "cm",res = 300)
chart.Correlation(chd_merge_info_fin_ut[,c("PM2.5","WSOC","SOC","WSOC (Freq)","SOC (Freq)","WSOC (Inty)","SOC (Inty)")],
                  method = "spearman",exact=F, label.pos=0.65,blk=1.6, na.rm=T)
dev.off()

tiff(filename = "Figure/bj_cor_organic_ms.tiff", width = 20,height = 20, units = "cm",res = 300)
chart.Correlation(chd_merge_info_fin_bj[,c("PM2.5","WSOC","SOC","WSOC (Freq)","SOC (Freq)","WSOC (Inty)","SOC (Inty)")],
                  method = "spearman",exact=F, label.pos=0.65,blk=1.6, na.rm=T)
dev.off()

tiff(filename = "Figure/ss_cor_organic_ms.tiff", width = 20,height = 20, units = "cm",res = 300)
chart.Correlation(chd_merge_info_fin_ss[,c("PM2.5","WSOC","SOC","WSOC (Freq)","SOC (Freq)","WSOC (Inty)","SOC (Inty)")],
                  method = "spearman",exact=F, label.pos=0.65,blk=1.6, na.rm=T)
dev.off()

tiff(filename = "Figure/se_cor_organic_ms.tiff", width = 20,height = 20, units = "cm",res = 300)
chart.Correlation(chd_merge_info_fin_se[,c("PM2.5","WSOC","SOC","WSOC (Freq)","SOC (Freq)","WSOC (Inty)","SOC (Inty)")],
                  method = "spearman",exact=F, label.pos=0.65,blk=1.6, na.rm=T)
dev.off()

tiff(filename = "Figure/nt_cor_organic_ms.tiff", width = 20,height = 20, units = "cm",res = 300)
chart.Correlation(chd_merge_info_fin_nt[,c("PM2.5","WSOC","SOC","WSOC (Freq)","SOC (Freq)","WSOC (Inty)","SOC (Inty)")],
                  method = "spearman",exact=F, label.pos=0.65,blk=1.6, na.rm=T)
dev.off()




