trans_fin=fread("Datafile/WSOC_transbysample.csv")
trans_fin

#trans_fin$Type2_x=ifelse(trans_fin$Type2_x=="","WSOC",trans_fin$Type2_x)
#trans_fin$Type2_y=ifelse(trans_fin$Type2_y=="","WSOC",trans_fin$Type2_y)

trans_fin$Transtype=paste(trans_fin$'Type2_x',trans_fin$'Type2_y',sep = "_")
table(trans_fin$Transtype)

trans_fin=trans_fin %>% filter(Transtype%in%c("WSOC_WSOC","WSOC_SOC","SOC_WSOC","SOC_SOC"))
trans_fin$Freq=1
table(trans_fin$Transtype)
trans_fin$Transtype=ifelse(trans_fin$Transtype=="SOC-WSOC","WSOC-SOC",trans_fin$Transtype)

trans_fin_soc=subset(trans_fin,trans_fin$Transtype!="WSOC-WSOC") %>% droplevels()

###Radar plot===============

trans_fin_soc
table(trans_fin_soc$Transtype)

trsbyname_all_rd=aggregate(trans_fin_soc$Freq, by=list(Sample=trans_fin_soc$Sample,Trans=trans_fin_soc$Trans.name),sum)
trsbyname_all_rd=trsbyname_all_rd %>% separate(Sample, c("Group","No"))
trsbyname_all_rd_mean=aggregate(trsbyname_all_rd$x, by=list(Group=trsbyname_all_rd$Group, Trans=trsbyname_all_rd$Trans),mean)

trsbyname_all_rd_mean

trans_info=fread("Datafile/potential_trans_info.csv")
trans_info=trans_info %>% `colnames<-`(c("Trans","Mass","Trans_class"))

trsbyname_all_rd_mean2=trsbyname_all_rd_mean %>% left_join(trans_info)

trsbyname_all_rd_mean2$Grouplab=factor(trsbyname_all_rd_mean2$Group, levels = c("Ulan","Beijing","Seosan","Seoul","Noto"),
                               labels = c("UT","BJ","SS","SE","NT"))

trsbyname_all_rd_mean2$tlab=factor(trsbyname_all_rd_mean2$Trans, levels = c("2H","C2H2O","CH2","C2H4","C2H6","C3H4","C3H6","C2H2",
                                                            "O","2O","3O","-2H+2O","H2O",
                                                            "CO2","CO (COOH-OH)","CH2O","C",
                                                            "NH","NH3-O","-NH3+2O","CH3N-O (CH3NH-OH)","CON-H (CONH-H2)","CN-H (CONH-H2O)",
                                                            "NO2-H","NO2-OH",
                                                            "SO2","SO3","SO4","SH2","S","SH-OH")
)

labs = c(expression(bold("-2H")),expression(bold("-C"["2"]*"H"["2"]*"O")),
         expression(bold("-CH"["2"])), expression(bold("-C"["2"]*"H"["4"])),expression(bold("-C"["2"]*"H"["6"])),
         expression(bold("-C"["3"]*"H"["4"])),expression(bold("-C"["3"]*"H"["6"])),expression(bold("-C"["2"]*"H"["2"])),
         expression(bold("+O")),expression(bold("+2O")),expression(bold("+3O")), expression(bold("-2H"*"+2O")), expression(bold("+H"["2"]*"O")),
         expression(bold("-C"*"O"["2"])), expression(bold("-COOH"*"+OH")), expression(bold("-C"*"H"["2"]*"O")),expression(bold("-C")),
         expression(bold("-NH")), expression(bold("-NH"["3"]*"+O")),expression(bold("-NH"["3"]*"+2O")),
         expression(bold("-CH"["3"]*"NH"*"+OH")),expression(bold("-CONH"*"+H"["2"])),expression(bold("-CONH"*"+H"["2"]*"O")),
         expression(bold("-NO"["2"]*"+H")),expression(bold("-NO"["2"]*"+OH")),
         expression(bold("-SO"["2"])),expression(bold("-SO"["3"])),expression(bold("-SO"["4"])),
         expression(bold("-SH"["2"])),expression(bold("-S")),expression(bold("-SH"*"+OH")))

trsbyname_all_rd_mean2$tlab2=factor(trsbyname_all_rd_mean2$Trans, levels = c("2H","C2H2O","CH2","C2H4","C2H6","C3H4","C3H6","C2H2",
                                                             "O","2O","3O","-2H+2O","H2O",
                                                             "CO2","CO (COOH-OH)","CH2O","C",
                                                             "NH","NH3-O","-NH3+2O","CH3N-O (CH3NH-OH)","CON-H (CONH-H2)","CN-H (CONH-H2O)",
                                                             "NO2-H","NO2-OH",
                                                             "SO2","SO3","SO4","SH2","S","SH-OH"),
                            labels = labs)

trsbyname_all_rd_mean2=trsbyname_all_rd_mean2[order(trsbyname_all_rd_mean2$tlab),]

ggplot(trsbyname_all_rd_mean2, aes(x=tlab, y=x,col=Grouplab, group=Grouplab))+
  geom_segment(aes(x=tlab, xend=tlab,y=0, yend=800), col="black")+
  geom_hline(yintercept = 200, lty=2)+
  geom_hline(yintercept = 400, lty=2)+
  geom_hline(yintercept = 600, lty=2)+
  geom_hline(yintercept = 800, lty=2)+
  annotate('text', x = 30.5, y = c(200,400,600,800), label = c('200','400','600','800'), size=6)+
  geom_text(aes(x=tlab,y=1000,label=tlab2),angle=15, parse=T, col="black",size=4)+
#  facet_wrap(.~Type)+
  geom_polygon(fill=NA, size=1)+
  geom_point(stat='identity', size=3.5) +
  scale_x_discrete(labels=labs)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_color_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  coord_polar(start = -pi/24*4)+
  theme_bw()+
  theme(
    plot.margin = unit(c(0,-1,0,-1),"cm"),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=0.1),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "bottom")+
  guides(col=guide_legend(title = "Sampling sites",
                          override.aes = list(fill=c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))))+
  ggsave(filename("soc_radar_trans_all"),height = 22, width = 20, units = "cm", dpi = 300, compression="lzw")


ggplot(trsbyname_all_rd_mean2, aes(x=tlab, y=x,col=Grouplab, group=Grouplab))+
  geom_segment(aes(x=tlab, xend=tlab,y=0, yend=800), col="black")+
  geom_hline(yintercept = 200, lty=2)+
  geom_hline(yintercept = 400, lty=2)+
  geom_hline(yintercept = 600, lty=2)+
  geom_hline(yintercept = 800, lty=2)+
  annotate('text', x = 30.5, y = c(200,400,600,800), label = c('200','400','600','800'), size=6)+
  #geom_text(aes(x=tlab,y=1000,label=tlab2),angle=15, parse=T, col="black",size=4)+
  #  facet_wrap(.~Type)+
  geom_polygon(fill=NA, size=1)+
  geom_point(stat='identity', size=3.5) +
  scale_x_discrete(labels=labs)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_color_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  coord_polar(start = -pi/24*4)+
  theme_bw()+
  theme(
    plot.margin = unit(c(0,-1,0,-1),"cm"),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=0.1),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "bottom")+
  guides(col=guide_legend(title = "Sampling sites",
                          override.aes = list(fill=c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))))+
  ggsave(filename("soc_radar_trans_nolabel"),height = 22, width = 20, units = "cm", dpi = 300, compression="lzw")


trsbyname_all_rd_mean2_zoom=trsbyname_all_rd_mean2
trsbyname_all_rd_mean2_zoom$x2=ifelse(trsbyname_all_rd_mean2_zoom$x>400,0,trsbyname_all_rd_mean2_zoom$x)

ggplot(trsbyname_all_rd_mean2_zoom, aes(x=tlab, y=x2,col=Grouplab, group=Grouplab))+
  geom_segment(aes(x=tlab, xend=tlab,y=0, yend=400), col="black")+
  geom_hline(yintercept = 100, lty=2)+
  geom_hline(yintercept = 200, lty=2)+
  geom_hline(yintercept = 300, lty=2)+
  geom_hline(yintercept = 400, lty=2)+
  annotate('text', x = 30.5, y = c(100,200,300), label = c('100','200','300'), size=6)+
  geom_text(aes(x=tlab,y=380,label=tlab2),angle=15, parse=T, col="black",size=4)+
  #  facet_wrap(.~Type)+
  geom_polygon(fill=NA, size=1)+
  geom_point(stat='identity', size=3.5) +
  scale_x_discrete(labels=labs)+
  scale_y_continuous(expand = c(0.02,0.02),limits=c(0,400))+
  scale_color_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  coord_polar(start = -pi/24*4)+
  theme_bw()+
  theme(
    plot.margin = unit(c(0,-1,0,-1),"cm"),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=0.1),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "bottom")+
  guides(col=guide_legend(title = "Sampling sites",
                          override.aes = list(fill=c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))))+
  ggsave(filename("soc_radar_trans_zoom"),height = 22, width = 20, units = "cm", dpi = 300, compression="lzw")

ggplot(trsbyname_all_rd_mean2_zoom, aes(x=tlab, y=x2,col=Grouplab, group=Grouplab))+
  geom_segment(aes(x=tlab, xend=tlab,y=0, yend=400), col="black")+
  geom_hline(yintercept = 100, lty=2)+
  geom_hline(yintercept = 200, lty=2)+
  geom_hline(yintercept = 300, lty=2)+
  geom_hline(yintercept = 400, lty=2)+
  annotate('text', x = 30.5, y = c(100,200,300), label = c('100','200','300'), size=6)+
  #geom_text(aes(x=tlab,y=380,label=tlab2),angle=15, parse=T, col="black",size=4)+
  #  facet_wrap(.~Type)+
  geom_polygon(fill=NA, size=1)+
  geom_point(stat='identity', size=3.5) +
  scale_x_discrete(labels=labs)+
  scale_y_continuous(expand = c(0.02,0.02),limits=c(0,420))+
  scale_color_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  coord_polar(start = -pi/24*4)+
  theme_bw()+
  theme(
    plot.margin = unit(c(0,-1,0,-1),"cm"),
    panel.background = element_rect(fill = "transparent", color = NA, size = 0), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA, size=0), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    strip.text = element_text(size = 28, family = "Arial",face = "bold"),
    legend.title = element_text(size = 16, vjust = 0.75,colour = "black", family = "Arial",face = "bold"),
    legend.text = element_text(size = 12, colour = "black", family = "Arial",hjust = 0.5,margin = unit(c(0,0,0,0),"cm")),
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent", color = NA, size=0), # get rid of legend panel bg
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    panel.border = element_blank(),
    legend.position = "NULL",
    legend.direction = "horizontal"
  )+
  #guides(col=guide_legend(title = "Sampling sites",
  #                        override.aes = list(fill=c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))))+
  guides(fill=guide_legend(reverse = T))+
  ggsave("Figure/230804pie_soc_zoom.png",height = 22, width = 20, units = "cm", dpi = 300)




