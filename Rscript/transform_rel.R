
trans_fin=fread("Datafile/WSOC_transbysample.csv")
trans_fin

#trans_fin$Type2_x=ifelse(trans_fin$Type2_x=="","WSOC",trans_fin$Type2_x)
#trans_fin$Type2_y=ifelse(trans_fin$Type2_y=="","WSOC",trans_fin$Type2_y)

trans_fin$Transtype=paste(trans_fin$'Type2_x',trans_fin$'Type2_y',sep = "_")
table(trans_fin$Transtype)

trans_fin=trans_fin %>% filter(Transtype%in%c("WSOC_WSOC","WSOC_SOC","SOC_WSOC","SOC_SOC"))
trans_fin$Freq=1

head(trans_fin)

##Total=====
trans_fin
trans_fin$Transtype=ifelse(trans_fin$Transtype=="SOC_WSOC","WSOC_SOC",trans_fin$Transtype)
table(trans_fin$Transtype)

trans_fin_soc=trans_fin %>% filter(Transtype%in%c("WSOC_SOC","SOC_WSOC","SOC_SOC"))

trsbyname_all=aggregate(trans_fin_soc$Freq, by=list(Sample=trans_fin_soc$Sample,Trans=trans_fin_soc$Trans.name, Transtype=trans_fin_soc$Transtype),sum)
trsbyname_all_tot=aggregate(trans_fin_soc$Freq, by=list(Sample=trans_fin_soc$Sample),sum) %>% `colnames<-`(c("Sample","Tot"))
table(trsbyname_all$Transtype)

trsbyname_all=trsbyname_all %>% left_join(trsbyname_all_tot)
trsbyname_all$rel=trsbyname_all$x/trsbyname_all$Tot*100
trsbyname_all

trsbyname_all=trsbyname_all %>% separate(Sample, c("Group","No"))

table(trsbyname_all$Transtype)

trsbyname_all$Transtypelab=factor(trsbyname_all$Transtype, levels = c("WSOC_SOC","SOC_SOC"),
                                  labels = c("WSOC-SOC","SOC-SOC"))

table(trsbyname_all$Transtypelab)
trsbyname_all_soc=trsbyname_all %>% filter(Transtypelab%in%c("WSOC-SOC","SOC-SOC"))

trsbyname_all_soc$Transtypelab2=factor(trsbyname_all_soc$Transtypelab, levels = c("WSOC-WSOC","WSOC-SOC","SOC-SOC"),
                                       labels = c("WSOC","inter SOC","intra SOC"))
table(trsbyname_all_soc$Transtypelab2)

trsbyname_all_soc$rel=trsbyname_all_soc$x/trsbyname_all_soc$Tot*100

table(trsbyname_all_soc$Transtypelab)
trans_mean_grp=aggregate(trsbyname_all_soc$rel, by=list(Group=trsbyname_all_soc$Group,Trans=trsbyname_all_soc$Trans, Transtype=trsbyname_all_soc$Transtypelab2), mean) %>% `colnames<-`(c("Group","Trans","Type","mean"))
trans_sd_grp=aggregate(trsbyname_all_soc$rel, by=list(Group=trsbyname_all_soc$Group,Trans=trsbyname_all_soc$Trans, Transtype=trsbyname_all_soc$Transtypelab2), sd) %>% `colnames<-`(c("Group","Trans","Type","sd"))

trans_mean_grp
trans_mean_grp$Grouplab=factor(trans_mean_grp$Group, levels = c("Ulan","Beijing","Seosan","Seoul","Noto"),
                               labels = c("UT","BJ","SS","SE","NT"))

trans_mean_grp

table(trans_mean_grp$Trans)
trans_mean_grp$tlab=factor(trans_mean_grp$Trans, levels = c("2H","C2H2O","CH2","C2H4","C2H6","C3H4","C3H6","C2H2",
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

trans_mean_grp$tlab2=factor(trans_mean_grp$Trans, levels = c("2H","C2H2O","CH2","C2H4","C2H6","C3H4","C3H6","C2H2",
                                                             "O","2O","3O","-2H+2O","H2O",
                                                             "CO2","CO (COOH-OH)","CH2O","C",
                                                             "NH","NH3-O","-NH3+2O","CH3N-O (CH3NH-OH)","CON-H (CONH-H2)","CN-H (CONH-H2O)",
                                                             "NO2-H","NO2-OH",
                                                             "SO2","SO3","SO4","SH2","S","SH-OH"),
                            labels = labs)

trans_mean_grp=trans_mean_grp[order(trans_mean_grp$tlab),]
trans_mean_grp

trans_info=fread("Datafile/potential_trans_info.csv")
trans_info=trans_info %>% `colnames<-`(c("Trans","Mass","Trans_class"))
head(trans_mean_grp)

#trn_stat=trn_stat %>% `colnames<-`(c("Tmp","stat","site","Trans","Grouplab"))
#trans_mean_grp2=trans_mean_grp2 %>% left_join(trn_stat, by=c("Grouplab","Trans"))

trans_mean_grp$tlab=factor(trans_mean_grp$Trans, levels = c("2H","C2H2O","CH2","C2H4","C2H6","C3H4","C3H6","C2H2",
                                                            "O","2O","3O","-2H+2O","H2O",
                                                            "CO2","CO (COOH-OH)","CH2O","C",
                                                            "NH","NH3-O","-NH3+2O","CH3N-O (CH3NH-OH)","CON-H (CONH-H2)","CN-H (CONH-H2O)",
                                                            "NO2-H","NO2-OH",
                                                            "SO2","SO3","SO4","SH2","S","SH-OH"))

trans_mean_grp2=trans_mean_grp %>% left_join(trans_info)
trans_mean_grp2=trans_mean_grp2 %>% left_join(trans_sd_grp)

trans_mean_grp_interc=subset(trans_mean_grp2,trans_mean_grp2$Type=="WSOC-SOC")
trans_mean_grp_intrasoc=subset(trans_mean_grp2,trans_mean_grp2$Type=="SOC-SOC")

barwidth=0.3
ggplot()+
  #geom_blank(data=trans_max, aes(x=as.numeric(Time), y=round(x,0)*1.2/1000))+
  geom_bar_pattern(data=trans_mean_grp_interc,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab), y=round(mean,0),
                        fill    = Group,
                        pattern_fill =Group,
                   ), 
                   pattern = "none",
                   width=barwidth,
                   alpha=0.9,
                   pattern_density          = 0.8, 
                   pattern_key_scale_factor = 1.11
  )+
  geom_bar_pattern(data=trans_mean_grp_intrasoc,
                   position= position_stack(reverse = T),stat = "identity",
                   aes(x=as.numeric(tlab)+barwidth+0.01, y=round(mean,0),
                       group=tlab,
                       fill    = Group,
                       pattern_fill =Group,
                   ), 
                   pattern = "stripe",
                   width=barwidth,
                   alpha=0.9,
                   pattern_alpha=0.6,
                   pattern_spacing = 0.025,
                   pattern_density= 0.7, 
                   pattern_key_scale_factor = 1.5) +
  scale_fill_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  scale_pattern_fill_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  scale_y_continuous(name = "",expand = c(0.01,0.05), breaks= pretty_breaks())+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(colour = "black",size = 1.0),
        strip.placement = "outside",
        strip.text = element_text(size = 22,face=2,margin=unit(c(0.2,0.4,0.2,0.2),"cm")),
        strip.background = element_blank(),
        axis.ticks.length.x = unit(0, "cm"),
        axis.ticks.length.y = unit(0.15, "cm"),
        plot.margin = unit(c(0.2,0.4,0.2,0.2),"cm"),
        plot.title =element_text(size = 26,face=2,margin=unit(c(0.2,0.4,0.2,0.0),"cm"), hjust = 0.5),
        #axis.title = element_text(margin = unit(c(0,0.2,0,0.2),"cm")),
        axis.text.x = element_text(size = 20, color = "black", margin = unit(c(0.2,0,.1,0),"cm"),face=1),
        axis.text.y = element_text(size = 20,color = "black",  margin = unit(c(0,0.1,0,0.2),"cm"),face=1),
        legend.title = element_text(size=20,margin =unit(c(0.22,0,0.1,0.2),"cm"),face = 2),
        legend.text = element_text(size=20,margin =unit(c(0.05,0,0.0,0),"cm"),hjust = 0.5, vjust = 0.5),
        axis.title.x = element_text(face = "bold",size=5,margin = unit(c(0.1,0.2,0.1,0.2),"cm")),
        axis.title.y = element_text(face = "bold",size=0.1,margin = unit(c(0.1,0.4,0.1,0.0),"cm")),
        legend.margin = margin(0,0,0,0, unit="cm"),
        legend.key.height = unit(0.8,"cm"),
        legend.key.width = unit(0.8,"cm"),
        legend.background = element_blank(),
        legend.box = "vertical",
        legend.direction = "vertical",
        legend.position = "NULL")+
  # coord_flip()+
  guides(fill=guide_legend(title = "", reverse = F, byrow = T, title.position = "left"))+
  ggsave(filename("trans_bar_comp_pattern"),height = 25, width = 55, units = "cm",  compression = "lzw",dpi = 300)


labs_dealkyl = c(expression(bold("-CH"["2"])), expression(bold("-C"["2"]*"H"["4"])),expression(bold("-C"["2"]*"H"["6"])),
                 expression(bold("-C"["3"]*"H"["4"])),expression(bold("-C"["3"]*"H"["6"])),expression(bold("-C"["2"]*"H"["2"])))

trans_mean_grp2_dealkyl=subset(trans_mean_grp2,trans_mean_grp2$Trans_class=="Dealkyl group")

trans_mean_grp2_dealkyl_ut=subset(trans_mean_grp2_dealkyl,trans_mean_grp2_dealkyl$Group=="Ulan")
trans_mean_grp2_dealkyl_bj=subset(trans_mean_grp2_dealkyl,trans_mean_grp2_dealkyl$Group=="Beijing")
trans_mean_grp2_dealkyl_ss=subset(trans_mean_grp2_dealkyl,trans_mean_grp2_dealkyl$Group=="Seosan")
trans_mean_grp2_dealkyl_se=subset(trans_mean_grp2_dealkyl,trans_mean_grp2_dealkyl$Group=="Seoul")
trans_mean_grp2_dealkyl_nt=subset(trans_mean_grp2_dealkyl,trans_mean_grp2_dealkyl$Group=="Noto")
barwidth=0.15

trans_mean_grp2_dealkyl_ut
table(trans_mean_grp2_dealkyl_ut$Type)

p_dealkyl <- ggplot()+
  geom_bar_pattern(data=trans_mean_grp2_dealkyl_ut,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+0.01, y=round(mean,3),
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#3C5488FF",
                   pattern_fill    = "#3C5488FF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 1.1
  )+
  geom_bar_pattern(data=trans_mean_grp2_dealkyl_bj,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+barwidth+0.01, y=round(mean,3),
                        #fill    = Group,
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#E64B35FF",
                   pattern_fill    = "#E64B35FF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 0.85
  )+
  geom_bar_pattern(data=trans_mean_grp2_dealkyl_ss,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+barwidth*2+0.01, y=round(mean,3),
                        #fill    = Group,
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#00A087FF",
                   pattern_fill    = "#00A087FF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 0.85
  )+
  geom_bar_pattern(data=trans_mean_grp2_dealkyl_se,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+barwidth*3+0.01, y=round(mean,3),
                        #fill    = Group,
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#4DBBD5FF",
                   pattern_fill    = "#4DBBD5FF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 0.85
  )+
  geom_bar_pattern(data=trans_mean_grp2_dealkyl_nt,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+barwidth*4+0.01, y=round(mean,3),
                        #fill    = Group,
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#F39B7FFF",
                   pattern_fill    = "#F39B7FFF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 0.85
  )+
  scale_pattern_manual(values = c("none","stripe","circle"))+
  #scale_x_continuous(name="",expand = c(0.02,0.02))+
  scale_x_continuous(name="",breaks = (c(3.3,4.3,5.3,6.3,7.3,8.3)),labels = labs_dealkyl,expand = c(0.02,0.02))+
  scale_y_continuous(expand = c(0.02,0.02))+
  ggtitle("Dealkyl group")+
  theme_bw()+
  theme(
    plot.margin = unit(c(0.2,1.2,-0.3,0.0),"cm"),
    plot.title = element_text(size=18, face = 2, hjust = 0.5),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size=18, face = 2),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_text(size = 13, colour = "black", angle = 335, hjust = 0.2, vjust = 0.5),
    axis.text.y = element_text(size=15, colour = "black",face = 2),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    #legend.position = "top"
    legend.position = "NULL")
#  ggsave(filename("soc_trans_dealkyl"),height = 10, width = 15, units = "cm", dpi = 300, compression="lzw")

p_dealkyl

##Oxy=====
labs_oxy = c(expression(bold("+O")),expression(bold("+2O")),expression(bold("+3O")), expression(bold("-2H"*"+2O")), expression(bold("+H"["2"]*"O")))
trans_mean_grp2_oxy=subset(trans_mean_grp2,trans_mean_grp2$Trans_class=="Oxygen addition")

trans_mean_grp2_oxy_ut=subset(trans_mean_grp2_oxy,trans_mean_grp2_oxy$Group=="Ulan")
trans_mean_grp2_oxy_bj=subset(trans_mean_grp2_oxy,trans_mean_grp2_oxy$Group=="Beijing")
trans_mean_grp2_oxy_ss=subset(trans_mean_grp2_oxy,trans_mean_grp2_oxy$Group=="Seosan")
trans_mean_grp2_oxy_se=subset(trans_mean_grp2_oxy,trans_mean_grp2_oxy$Group=="Seoul")
trans_mean_grp2_oxy_nt=subset(trans_mean_grp2_oxy,trans_mean_grp2_oxy$Group=="Noto")
barwidth=0.15

trans_mean_grp2_oxy_ut
table(trans_mean_grp2_oxy_ut$Type)

p_oxy <- ggplot()+
  geom_bar_pattern(data=trans_mean_grp2_oxy_ut,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+0.01, y=round(mean,3),
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#3C5488FF",
                   pattern_fill    = "#3C5488FF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 1.1
  )+
  geom_bar_pattern(data=trans_mean_grp2_oxy_bj,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+barwidth+0.01, y=round(mean,3),
                        #fill    = Group,
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#E64B35FF",
                   pattern_fill    = "#E64B35FF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 0.85
  )+
  geom_bar_pattern(data=trans_mean_grp2_oxy_ss,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+barwidth*2+0.01, y=round(mean,3),
                        #fill    = Group,
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#00A087FF",
                   pattern_fill    = "#00A087FF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 0.85
  )+
  geom_bar_pattern(data=trans_mean_grp2_oxy_se,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+barwidth*3+0.01, y=round(mean,3),
                        #fill    = Group,
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#4DBBD5FF",
                   pattern_fill    = "#4DBBD5FF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 0.85
  )+
  geom_bar_pattern(data=trans_mean_grp2_oxy_nt,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+barwidth*4+0.01, y=round(mean,3),
                        #fill    = Group,
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#F39B7FFF",
                   pattern_fill    = "#F39B7FFF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 0.85
  )+
  scale_pattern_manual(values = c("none","stripe","circle"))+
  #scale_x_continuous(name="",expand = c(0.02,0.02))+
  scale_x_continuous(name="",breaks = (c(3.3,4.3,5.3,6.3,7.3)+6),labels = labs_oxy,expand = c(0.02,0.02))+
  scale_y_continuous(expand = c(0.02,0.02))+
  ggtitle("Oxygen addition")+
  theme_bw()+
  theme(
    plot.margin = unit(c(0.2,1.2,-0.3,0.0),"cm"),
    plot.title = element_text(size=18, face = 2, hjust = 0.5),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size=18, face = 2),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_text(size = 13, colour = "black", angle = 335, hjust = 0.2, vjust = 0.5),
    axis.text.y = element_text(size=15, colour = "black",face = 2),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "NULL")
#  ggsave(filename("soc_trans_oxy"),height = 10, width = 15, units = "cm", dpi = 300, compression="lzw")
p_oxy

##carboxyl=====
labs_carboxyl = c(expression(bold("-C"*"O"["2"])), expression(bold("-COOH"*"+OH")), expression(bold("-C"*"H"["2"]*"O")),expression(bold("-C")))
trans_mean_grp2_carboxyl=subset(trans_mean_grp2,trans_mean_grp2$Trans_class=="Reaction of carboxylic acid")

trans_mean_grp2_carboxyl_ut=subset(trans_mean_grp2_carboxyl,trans_mean_grp2_carboxyl$Group=="Ulan")
trans_mean_grp2_carboxyl_bj=subset(trans_mean_grp2_carboxyl,trans_mean_grp2_carboxyl$Group=="Beijing")
trans_mean_grp2_carboxyl_ss=subset(trans_mean_grp2_carboxyl,trans_mean_grp2_carboxyl$Group=="Seosan")
trans_mean_grp2_carboxyl_se=subset(trans_mean_grp2_carboxyl,trans_mean_grp2_carboxyl$Group=="Seoul")
trans_mean_grp2_carboxyl_nt=subset(trans_mean_grp2_carboxyl,trans_mean_grp2_carboxyl$Group=="Noto")
barwidth=0.15

trans_mean_grp2_carboxyl_ut
table(trans_mean_grp2_carboxyl_ut$Type)

p_carboxyl <- ggplot()+
  geom_bar_pattern(data=trans_mean_grp2_carboxyl_ut,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+0.01, y=round(mean,2),
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#3C5488FF",
                   pattern_fill    = "#3C5488FF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 1.1
  )+
  geom_bar_pattern(data=trans_mean_grp2_carboxyl_bj,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+barwidth+0.01, y=round(mean,2),
                        #fill    = Group,
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#E64B35FF",
                   pattern_fill    = "#E64B35FF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 0.85
  )+
  geom_bar_pattern(data=trans_mean_grp2_carboxyl_ss,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+barwidth*2+0.01, y=round(mean,2),
                        #fill    = Group,
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#00A087FF",
                   pattern_fill    = "#00A087FF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 0.85
  )+
  geom_bar_pattern(data=trans_mean_grp2_carboxyl_se,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+barwidth*3+0.01, y=round(mean,2),
                        #fill    = Group,
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#4DBBD5FF",
                   pattern_fill    = "#4DBBD5FF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 0.85
  )+
  geom_bar_pattern(data=trans_mean_grp2_carboxyl_nt,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+barwidth*4+0.01, y=round(mean,2),
                        #fill    = Group,
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#F39B7FFF",
                   pattern_fill    = "#F39B7FFF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 0.85
  )+
  scale_pattern_manual(values = c("none","stripe","circle"))+
  #scale_x_continuous(name="",expand = c(0.02,0.02))+
  scale_x_continuous(name="",breaks = c(14.3,15.3,16.3,17.3),labels = labs_carboxyl,expand = c(0.02,0.02))+
  scale_y_continuous(expand = c(0.02,0.02))+
  ggtitle("Reaction of carboxylic acid")+
  theme_bw()+
  theme(
    plot.margin = unit(c(0.2,1.2,-0.2,0.0),"cm"),
    plot.title = element_text(size=18, face = 2, hjust = 0.5),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size=18, face = 2),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_text(size = 14, colour = "black", angle = 335, hjust = 0.2, vjust = 0.5),
    axis.text.y = element_text(size=15, colour = "black",face = 2),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "NULL")
#  ggsave(filename("soc_trans_carboxyl"),height = 10, width = 15, units = "cm", dpi = 300, compression="lzw")

p_carboxyl

##amine=====
labs_amine = c(expression(bold("-NH")), expression(bold("-NH"["3"]*"+O")),expression(bold("-NH"["3"]*"+2O")),
               expression(bold("-CH"["3"]*"NH"*"+OH")),expression(bold("-CONH"*"+H"["2"])),expression(bold("-CONH"*"+H"["2"]*"O")))

trans_mean_grp2_amine=subset(trans_mean_grp2,trans_mean_grp2$Trans_class=="Reaction of amine")

trans_mean_grp2_amine_ut=subset(trans_mean_grp2_amine,trans_mean_grp2_amine$Group=="Ulan")
trans_mean_grp2_amine_bj=subset(trans_mean_grp2_amine,trans_mean_grp2_amine$Group=="Beijing")
trans_mean_grp2_amine_ss=subset(trans_mean_grp2_amine,trans_mean_grp2_amine$Group=="Seosan")
trans_mean_grp2_amine_se=subset(trans_mean_grp2_amine,trans_mean_grp2_amine$Group=="Seoul")
trans_mean_grp2_amine_nt=subset(trans_mean_grp2_amine,trans_mean_grp2_amine$Group=="Noto")
barwidth=0.15

trans_mean_grp2_amine_ut
table(trans_mean_grp2_amine_ut$Type)

p_amine <- ggplot()+
  geom_bar_pattern(data=trans_mean_grp2_amine_ut,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+0.01, y=round(mean,3),
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#3C5488FF",
                   pattern_fill    = "#3C5488FF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 1.1
  )+
  geom_bar_pattern(data=trans_mean_grp2_amine_bj,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+barwidth+0.01, y=round(mean,3),
                        #fill    = Group,
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#E64B35FF",
                   pattern_fill    = "#E64B35FF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 0.85
  )+
  geom_bar_pattern(data=trans_mean_grp2_amine_ss,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+barwidth*2+0.01, y=round(mean,3),
                        #fill    = Group,
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#00A087FF",
                   pattern_fill    = "#00A087FF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 0.85
  )+
  geom_bar_pattern(data=trans_mean_grp2_amine_se,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+barwidth*3+0.01, y=round(mean,3),
                        #fill    = Group,
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#4DBBD5FF",
                   pattern_fill    = "#4DBBD5FF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 0.85
  )+
  geom_bar_pattern(data=trans_mean_grp2_amine_nt,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+barwidth*4+0.01, y=round(mean,3),
                        #fill    = Group,
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#F39B7FFF",
                   pattern_fill    = "#F39B7FFF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 0.85
  )+
  scale_pattern_manual(values = c("none","stripe","circle"))+
  scale_x_continuous(name="",breaks = c(18.3,19.3,20.3,21.3,22.3,23.3),labels = labs_amine,expand = c(0.02,0.02))+
  scale_y_continuous(expand = c(0.02,0.02))+
  ggtitle("Reaction of amine")+
  theme_bw()+
  theme(
    plot.margin = unit(c(0.2,1.2,-0.3,0.0),"cm"),
    plot.title = element_text(size=18, face = 2, hjust = 0.5),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size=18, face = 2),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_text(size = 13, colour = "black", angle = 335, hjust = 0.2, vjust = 0.5),
    axis.text.y = element_text(size=15, colour = "black",face = 2),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "NULL")
#  ggsave(filename("soc_trans_amine"),height = 10, width = 16, units = "cm", dpi = 300, compression="lzw")

p_amine

##nitro=====
labs_nitro = c(expression(bold("-NO"["2"]*"+H")),expression(bold("-NO"["2"]*"+OH")))

trans_mean_grp2_nitro=subset(trans_mean_grp2,trans_mean_grp2$Trans_class=="Reaction of the nitro or nitroso")

trans_mean_grp2_nitro_ut=subset(trans_mean_grp2_nitro,trans_mean_grp2_nitro$Group=="Ulan")
trans_mean_grp2_nitro_bj=subset(trans_mean_grp2_nitro,trans_mean_grp2_nitro$Group=="Beijing")
trans_mean_grp2_nitro_ss=subset(trans_mean_grp2_nitro,trans_mean_grp2_nitro$Group=="Seosan")
trans_mean_grp2_nitro_se=subset(trans_mean_grp2_nitro,trans_mean_grp2_nitro$Group=="Seoul")
trans_mean_grp2_nitro_nt=subset(trans_mean_grp2_nitro,trans_mean_grp2_nitro$Group=="Noto")
barwidth=0.15

trans_mean_grp2_nitro_ut
table(trans_mean_grp2_nitro_ut$Type)

p_nitro <- ggplot()+
  geom_bar_pattern(data=trans_mean_grp2_nitro_ut,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+0.01, y=round(mean,3),
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#3C5488FF",
                   pattern_fill    = "#3C5488FF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 1.1
  )+
  geom_bar_pattern(data=trans_mean_grp2_nitro_bj,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+barwidth+0.01, y=round(mean,3),
                        #fill    = Group,
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#E64B35FF",
                   pattern_fill    = "#E64B35FF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 0.85
  )+
  geom_bar_pattern(data=trans_mean_grp2_nitro_ss,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+barwidth*2+0.01, y=round(mean,3),
                        #fill    = Group,
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#00A087FF",
                   pattern_fill    = "#00A087FF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 0.85
  )+
  geom_bar_pattern(data=trans_mean_grp2_nitro_se,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+barwidth*3+0.01, y=round(mean,3),
                        #fill    = Group,
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#4DBBD5FF",
                   pattern_fill    = "#4DBBD5FF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 0.85
  )+
  geom_bar_pattern(data=trans_mean_grp2_nitro_nt,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+barwidth*4+0.01, y=round(mean,3),
                        #fill    = Group,
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#F39B7FFF",
                   pattern_fill    = "#F39B7FFF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 0.85
  )+
  scale_pattern_manual(values = c("none","stripe","circle"))+
  #scale_x_continuous(name="",expand = c(0.02,0.02))+
  scale_x_continuous(name="",breaks = c(24.3,25.3),labels = labs_nitro,expand = c(0.02,0.02))+
  scale_y_continuous(expand = c(0.02,0.02))+
  ggtitle("Reaction of the nitro or nitroso")+
  theme_bw()+
  theme(
    plot.margin = unit(c(0.2,1.2,-0.2,0.0),"cm"),
    plot.title = element_text(size=18, face = 2, hjust = 0.5),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size=18, face = 2),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_text(size = 14, colour = "black", angle = 335, hjust = 0.2, vjust = 0.5),
    axis.text.y = element_text(size=15, colour = "black",face = 2),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "NULL")
#  ggsave(filename("soc_trans_nitro"),height = 10, width = 16, units = "cm", dpi = 300, compression="lzw")

p_nitro

##sufur=====
labs_sulfur = c(expression(bold("-SO"["2"])),expression(bold("-SO"["3"])),expression(bold("-SO"["4"])),
                expression(bold("-SH"["2"])),expression(bold("-S")),expression(bold("-SH"*"+OH")))

trans_mean_grp2_sufur=subset(trans_mean_grp2,trans_mean_grp2$Trans_class=="Reaction of sulfur")

trans_mean_grp2_sufur_ut=subset(trans_mean_grp2_sufur,trans_mean_grp2_sufur$Group=="Ulan")
trans_mean_grp2_sufur_bj=subset(trans_mean_grp2_sufur,trans_mean_grp2_sufur$Group=="Beijing")
trans_mean_grp2_sufur_ss=subset(trans_mean_grp2_sufur,trans_mean_grp2_sufur$Group=="Seosan")
trans_mean_grp2_sufur_se=subset(trans_mean_grp2_sufur,trans_mean_grp2_sufur$Group=="Seoul")
trans_mean_grp2_sufur_nt=subset(trans_mean_grp2_sufur,trans_mean_grp2_sufur$Group=="Noto")
barwidth=0.15

trans_mean_grp2_sufur_ut
table(trans_mean_grp2_sufur_ut$Type)

p_sulfur <- ggplot()+
  geom_bar_pattern(data=trans_mean_grp2_sufur_ut,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+0.01, y=round(mean,3),
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#3C5488FF",
                   pattern_fill    = "#3C5488FF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 1.1
  )+
  geom_bar_pattern(data=trans_mean_grp2_sufur_bj,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+barwidth+0.01, y=round(mean,3),
                        #fill    = Group,
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#E64B35FF",
                   pattern_fill    = "#E64B35FF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 0.85
  )+
  geom_bar_pattern(data=trans_mean_grp2_sufur_ss,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+barwidth*2+0.01, y=round(mean,3),
                        #fill    = Group,
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#00A087FF",
                   pattern_fill    = "#00A087FF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 0.85
  )+
  geom_bar_pattern(data=trans_mean_grp2_sufur_se,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+barwidth*3+0.01, y=round(mean,3),
                        #fill    = Group,
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#4DBBD5FF",
                   pattern_fill    = "#4DBBD5FF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 0.85
  )+
  geom_bar_pattern(data=trans_mean_grp2_sufur_nt,position= position_stack(reverse = T), stat = "identity",
                   aes( x=as.numeric(tlab)+barwidth*4+0.01, y=round(mean,3),
                        #fill    = Group,
                        pattern =as.factor(Type),
                   ), 
                   #pattern = "none",
                   col="black",
                   fill    = "#F39B7FFF",
                   pattern_fill    = "#F39B7FFF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 0.85
  )+
  scale_pattern_manual(values = c("none","stripe","circle"))+
  #scale_x_continuous(name="",expand = c(0.02,0.02))+
  scale_x_continuous(name="",breaks = c(26.3,27.3,28.3,29.3,30.3,31.3),labels = labs_sulfur,expand = c(0.02,0.02))+
  scale_y_continuous(expand = c(0.02,0.02))+
  ggtitle("Reaction of sulfur")+
  theme_bw()+
  theme(
    plot.margin = unit(c(0.2,1.2,0,0.2),"cm"),
    plot.title = element_text(size=18, face = 2, hjust = 0.5),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size=18, face = 2),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_text(size = 14, colour = "black", angle = 335, hjust = 0.2, vjust = 0.5),
    axis.text.y = element_text(size=15, colour = "black",face = 2),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "NULL",
    #legend.position = "bottom",
  )
#  ggsave(filename("soc_trans_sufur"),height = 10, width = 16, units = "cm", dpi = 300, compression="lzw")

p_sulfur

p_dealkyl+p_oxy+p_carboxyl+p_nitro+p_amine+p_sulfur+
  ggsave(filename("soc_trans_soc_rel"),height = 20, width = 40, units = "cm", dpi = 300, compression="lzw")




###Radar plot (rel)===============
trans_mean_grp2

trans_fin_soc
table(trans_fin_soc$Transtype)

trans_mean_grp

trsbyname_all_rd_rel=aggregate(trans_fin_soc$Freq, by=list(Sample=trans_fin_soc$Sample,Trans=trans_fin_soc$Trans.name),sum)
trsbyname_all_rd_rel_tot=aggregate(trans_fin_soc$Freq, by=list(Sample=trans_fin_soc$Sample),sum) %>% `colnames<-`(c("Sample","Tot"))

trsbyname_all_rd_rel=trsbyname_all_rd_rel %>% left_join(trsbyname_all_rd_rel_tot[,c("Sample","Tot")])
trsbyname_all_rd_rel$rel=trsbyname_all_rd_rel$x/trsbyname_all_rd_rel$Tot*100

trsbyname_all_rd_rel=trsbyname_all_rd_rel %>% separate(Sample, c("Group","No"))

trsbyname_all_rd_rel_mean=aggregate(trsbyname_all_rd_rel$rel, by=list(Group=trsbyname_all_rd_rel$Group, Trans=trsbyname_all_rd_rel$Trans),mean)
trsbyname_all_rd_rel_mean

trans_info=fread("Datafile/potential_trans_info.csv")
trans_info=trans_info %>% `colnames<-`(c("Trans","Mass","Trans_class"))

trsbyname_all_rd_rel_mean2=trsbyname_all_rd_rel_mean %>% left_join(trans_info)

trsbyname_all_rd_rel_mean2$Grouplab=factor(trsbyname_all_rd_rel_mean2$Group, levels = c("Ulan","Beijing","Seosan","Seoul","Noto"),
                                       labels = c("UT","BJ","SS","SE","NT"))

trsbyname_all_rd_rel_mean2$tlab=factor(trsbyname_all_rd_rel_mean2$Trans, levels = c("2H","C2H2O","CH2","C2H4","C2H6","C3H4","C3H6","C2H2",
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

trsbyname_all_rd_rel_mean2$tlab2=factor(trsbyname_all_rd_rel_mean2$Trans, levels = c("2H","C2H2O","CH2","C2H4","C2H6","C3H4","C3H6","C2H2",
                                                                             "O","2O","3O","-2H+2O","H2O",
                                                                             "CO2","CO (COOH-OH)","CH2O","C",
                                                                             "NH","NH3-O","-NH3+2O","CH3N-O (CH3NH-OH)","CON-H (CONH-H2)","CN-H (CONH-H2O)",
                                                                             "NO2-H","NO2-OH",
                                                                             "SO2","SO3","SO4","SH2","S","SH-OH"),
                                    labels = labs)

#trsbyname_all_rd_mean2

trsbyname_all_rd_rel_mean2=trsbyname_all_rd_rel_mean2[order(trsbyname_all_rd_rel_mean2$tlab),]
trsbyname_all_rd_rel_mean2

max(trsbyname_all_rd_rel_mean2$x)

ggplot(trsbyname_all_rd_rel_mean2, aes(x=tlab, y=x,col=Grouplab, group=Grouplab))+
  geom_segment(aes(x=tlab, xend=tlab,y=0, yend=6), col="black")+
  geom_hline(yintercept = 2, lty=2)+
  geom_hline(yintercept = 4, lty=2)+
  geom_hline(yintercept = 6, lty=2)+
#  geom_hline(yintercept = 800, lty=2)+
  annotate('text', x = 30.5, y = c(2.25,4.25,6.25), label = c('2','4','6'), size=6)+
  geom_text(aes(x=tlab,y=6.5,label=tlab2),angle=15, parse=T, col="black",size=4)+
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
  ggsave(filename("soc_radar_trans_rel"),height = 22, width = 20, units = "cm", dpi = 300, compression="lzw")

ggplot(trsbyname_all_rd_rel_mean2, aes(x=tlab, y=x,col=Grouplab, group=Grouplab))+
  geom_segment(aes(x=tlab, xend=tlab,y=0, yend=6), col="black")+
  geom_hline(yintercept = 2, lty=2)+
  geom_hline(yintercept = 4, lty=2)+
  geom_hline(yintercept = 6, lty=2)+
  geom_polygon(fill=NA, size=1)+
  geom_point(stat='identity', size=3.5) +
  annotate('text', x = 30.5, y = c(2.25,4.25,6.25), label = c('2','4','6'), size=6)+
  scale_x_discrete(labels=labs)+
  scale_y_continuous(expand = c(0.02,0.02))+
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
  ggsave("Figure/230808soc_radar_trans_rel_nolabel.png",height = 22, width = 20, units = "cm", dpi = 300)


##
dall_sel #data from script build_transformationdataset.R
frd_1st_soc
frd_merge

trans_num=dall_sel[,c("Group","No","m/z","Trans")] %>% `colnames<-`(c("Group","No","Calc.m.z","Trans"))
frd_1st_soc$Type2=ifelse(is.na(frd_1st_soc$Type),"WSOC","SOC")
table(trans_num$Group)

frd_1st_soc$Group=ifelse(frd_1st_soc$Group=="Ulaanbaatar","Ulan",frd_1st_soc$Group)

frd_1st_soc_fin=frd_1st_soc %>% left_join(trans_num[,c("Group","No","Calc.m.z","Trans")])
frd_1st_soc_fin2=frd_1st_soc_fin

frd_1st_soc_fin2$Group=ifelse(frd_1st_soc_fin2$Group=="Ulaanbaatar","Ulan",frd_1st_soc_fin2$Group)
frd_1st_soc_fin2$Trans=ifelse(is.na(frd_1st_soc_fin2$Trans),0,frd_1st_soc_fin2$Trans)
frd_1st_soc_fin2$Trans_mat=ifelse(frd_1st_soc_fin2$Trans==0,"Mismatch","Match")
table(frd_1st_soc_fin2$Trans_mat)

frd_match_all=aggregate(frd_1st_soc_fin2$Bromo.Inty, by=list(Group=frd_1st_soc_fin2$Group, No=frd_1st_soc_fin2$No, Transmat=frd_1st_soc_fin2$Trans_mat),sum)
frd_match_all_tot=aggregate(frd_1st_soc_fin2$Bromo.Inty, by=list(Group=frd_1st_soc_fin2$Group, No=frd_1st_soc_fin2$No),sum) %>% `colnames<-`(c("Group","No","Tot"))

frd_match_all=frd_match_all %>% left_join(frd_match_all_tot)

frd_match_all$rel=frd_match_all$x/frd_match_all$Tot*100
frd_match_all$Grouplab=factor(frd_match_all$Group, levels = c("U"))

frd_match_all$Grouplab=factor(frd_match_all$Group, levels = c("Ulan","Beijing","Seosan","Seoul","Noto"),
                              labels = c("UT","BJ","SS","SE","NT"))

frd_match_all$No=as.numeric(frd_match_all$No)

frd_match_all$Sample=paste(frd_match_all$Group, frd_match_all$No, sep = "_")

ggmatch <- ggplot(frd_match_all, aes(x=as.factor(No), y=rel, fill=Transmat))+
  geom_bar(stat = "identity", position = position_fill(reverse = T))+
  scale_x_discrete("", expand = c(0.03,0.03))+
  scale_y_continuous("",labels = scales::percent, expand = c(0.01,0.01))+
  scale_fill_manual(values = c("#7EC97F","#FFFEA2"))+
  facet_rep_wrap(.~Grouplab, scales = "free_y",repeat.tick.labels = T)+
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
        axis.title.y.left = element_text(size = 24, colour = "black",margin = unit(c(0.1,0.2,0.1,0.0),"cm"),family = "Arial"),
        axis.title.y.right = element_text(size = 24, colour = "blue",margin = unit(c(0.1,-0.2,0.1,0.2),"cm"),family = "Arial"),
        legend.text = element_text(size = 18, colour = "black",family = "Arial",margin = unit(c(0.3,0.3,0.3,0.3),"cm"), hjust = 0.5,vjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color="black",size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "vertical",
        legend.background = element_blank(),
        legend.position = c(0.72,0.35))+
  xlab("")
ggmatch <-   ggsave(filename("wsoc_trans_match"),height = 32, width = 65, units = "cm", dpi = 300, compression="lzw")



frd_match_all
frd_match_all_mean=dcast(frd_match_all, Group+Grouplab~Transmat,value.var = "rel", mean) %>% 
  melt(id.vars=c("Group","Grouplab"))

frd_match_all_mean

ggmatch_avg <- ggplot(frd_match_all_mean, aes(x=Grouplab, y=value, fill=variable))+
  geom_bar(stat = "identity", position = position_fill(reverse = T))+
  scale_x_discrete("", expand = c(0.12,0.12))+
  scale_y_continuous("",labels = scales::percent, expand = c(0.01,0.01))+
  scale_fill_manual(values = c("#7EC97F","#FFFEA2"))+
  #facet_rep_wrap(.~Grouplab, scales = "free_y",repeat.tick.labels = T)+
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
        legend.text = element_text(size = 18, colour = "black",family = "Arial",margin = unit(c(0.2,0.2,0.2,0.2),"cm"), hjust = 0.5,vjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = "bottom")+
  xlab("")

ggmatch_avg <-   ggsave(filename("wsoc_trans_match_mean"),height = 17, width = 12, units = "cm", dpi = 300, compression="lzw")

##Event vs Non-event==========
envi_1st
trsbyname_all_rd_rel

table(trsbyname_all_rd_rel$Group)
table(envi_1st$Group)
envi_1st$Group=ifelse(envi_1st$Group=="Ulaanbaatar","Ulan",envi_1st$Group)
envi_1st$Sample=paste(envi_1st$Group,envi_1st$No,sep = "_")
trsbyname_all_rd_rel$Sample=paste(trsbyname_all_rd_rel$Group,trsbyname_all_rd_rel$No,sep = "_")

trsbyname_all_rd_rel_ev=trsbyname_all_rd_rel %>% left_join(envi_1st[,c("Sample","Event")])
library(sf)
trsbyname_all_rd_rel_ev_fin=trsbyname_all_rd_rel_ev %>% filter(Event%in%c("Event","Non-event"))





trsbyname_all_rd_mean_en=aggregate(trsbyname_all_rd_rel_ev_fin$rel, by=list(Group=trsbyname_all_rd_rel_ev_fin$Group,
                                                                           Event=trsbyname_all_rd_rel_ev_fin$Event,Trans=trsbyname_all_rd_rel_ev_fin$Trans),mean)

trsbyname_all_rd_mean_en

trsbyname_all_rd_mean_en2=trsbyname_all_rd_mean_en %>% left_join(trans_info)

trsbyname_all_rd_mean_en2$Grouplab=factor(trsbyname_all_rd_mean_en2$Group, levels = c("Ulan","Beijing","Seosan","Seoul","Noto"),
                                           labels = c("UT","BJ","SS","SE","NT"))

trsbyname_all_rd_mean_en2$tlab=factor(trsbyname_all_rd_mean_en2$Trans, levels = c("2H","C2H2O","CH2","C2H4","C2H6","C3H4","C3H6","C2H2",
                                                                                    "O","2O","3O","-2H+2O","H2O",
                                                                                    "CO2","CO (COOH-OH)","CH2O","C",
                                                                                    "NH","NH3-O","-NH3+2O","CH3N-O (CH3NH-OH)","CON-H (CONH-H2)","CN-H (CONH-H2O)",
                                                                                    "NO2-H","NO2-OH",
                                                                                    "SO2","SO3","SO4","SH2","S","SH-OH")
)

trsbyname_all_rd_mean_en2$tlab2=factor(trsbyname_all_rd_mean_en2$Trans, levels = c("2H","C2H2O","CH2","C2H4","C2H6","C3H4","C3H6","C2H2",
                                                                                     "O","2O","3O","-2H+2O","H2O",
                                                                                     "CO2","CO (COOH-OH)","CH2O","C",
                                                                                     "NH","NH3-O","-NH3+2O","CH3N-O (CH3NH-OH)","CON-H (CONH-H2)","CN-H (CONH-H2O)",
                                                                                     "NO2-H","NO2-OH",
                                                                                     "SO2","SO3","SO4","SH2","S","SH-OH"),
                                        labels = labs)

trsbyname_all_rd_mean_en2=trsbyname_all_rd_mean_en2[order(trsbyname_all_rd_mean_en2$tlab),]
trsbyname_all_rd_mean_en2


##ut====
trsbyname_all_rd_mean_en2
trsbyname_all_rd_mean_en2_ut=subset(trsbyname_all_rd_mean_en2,trsbyname_all_rd_mean_en2$Grouplab=="UT")

ggplot(trsbyname_all_rd_mean_en2_ut, aes(x=tlab, y=x,col=Event, group=Event))+
  geom_segment(aes(x=tlab, xend=tlab,y=0, yend=6), col="black")+
  geom_hline(yintercept = 2, lty=2)+
  geom_hline(yintercept = 4, lty=2)+
  geom_hline(yintercept = 6, lty=2)+
  #  geom_hline(yintercept = 800, lty=2)+
  annotate('text', x = 30.5, y = c(2.25,4.25,6.25), label = c('2','4','6'), size=6)+
  geom_text(aes(x=tlab,y=6.5,label=tlab2),angle=15, parse=T, col="black",size=4)+
  #  facet_wrap(.~Type)+
  geom_polygon(fill=NA, size=1)+
  geom_point(stat='identity', size=3.5) +
  scale_x_discrete(labels=labs)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_color_manual(values = c("#F99417","grey70"))+
  coord_polar(start = -pi/24*4)+
  ggtitle("UT")+
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
  ggsave(filename("ut_soc_radar_trans_ev"),height = 22, width = 20, units = "cm", dpi = 300, compression="lzw")

ggplot(trsbyname_all_rd_mean_en2_ut, aes(x=tlab, y=x,col=Event, group=Event))+
  geom_segment(aes(x=tlab, xend=tlab,y=0, yend=6), col="black")+
  geom_hline(yintercept = 2, lty=2)+
  geom_hline(yintercept = 4, lty=2)+
  geom_hline(yintercept = 6, lty=2)+
  #  geom_hline(yintercept = 800, lty=2)+
  annotate('text', x = 30.5, y = c(2.25,4.25,6.25), label = c('2','4','6'), size=6)+
  #geom_text(aes(x=tlab,y=6.5,label=tlab2),angle=15, parse=T, col="black",size=4)+
  #  facet_wrap(.~Type)+
  geom_polygon(fill=NA, size=1)+
  geom_point(stat='identity', size=3.5) +
  scale_x_discrete(labels=labs)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_color_manual(values = c("#F99417","grey70"))+
  coord_polar(start = -pi/24*4)+
  ggtitle("UT")+
  theme_bw()+
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
  ggsave("Figure/230808_ut_radar_trans_rel_nolabel.png",height = 22, width = 20, units = "cm", dpi = 300)


##bj====
trsbyname_all_rd_mean_en2_bj=subset(trsbyname_all_rd_mean_en2,trsbyname_all_rd_mean_en2$Grouplab=="BJ")

ggplot(trsbyname_all_rd_mean_en2_bj, aes(x=tlab, y=x,col=Event, group=Event))+
  geom_segment(aes(x=tlab, xend=tlab,y=0, yend=6), col="black")+
  geom_hline(yintercept = 2, lty=2)+
  geom_hline(yintercept = 4, lty=2)+
  geom_hline(yintercept = 6, lty=2)+
  #  geom_hline(yintercept = 800, lty=2)+
  annotate('text', x = 30.5, y = c(2.25,4.25,6.25), label = c('2','4','6'), size=6)+
  geom_text(aes(x=tlab,y=6.5,label=tlab2),angle=15, parse=T, col="black",size=4)+
  #  facet_wrap(.~Type)+
  geom_polygon(fill=NA, size=1)+
  geom_point(stat='identity', size=3.5) +
  scale_x_discrete(labels=labs)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_color_manual(values = c("#F99417","grey70"))+
  coord_polar(start = -pi/24*4)+
  ggtitle("bj")+
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
  ggsave(filename("bj_soc_radar_trans_ev"),height = 22, width = 20, units = "cm", dpi = 300, compression="lzw")

ggplot(trsbyname_all_rd_mean_en2_bj, aes(x=tlab, y=x,col=Event, group=Event))+
  geom_segment(aes(x=tlab, xend=tlab,y=0, yend=6), col="black")+
  geom_hline(yintercept = 2, lty=2)+
  geom_hline(yintercept = 4, lty=2)+
  geom_hline(yintercept = 6, lty=2)+
  #  geom_hline(yintercept = 800, lty=2)+
  annotate('text', x = 30.5, y = c(2.25,4.25,6.25), label = c('2','4','6'), size=6)+
  #geom_text(aes(x=tlab,y=6.5,label=tlab2),angle=15, parse=T, col="black",size=4)+
  #  facet_wrap(.~Type)+
  geom_polygon(fill=NA, size=1)+
  geom_point(stat='identity', size=3.5) +
  scale_x_discrete(labels=labs)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_color_manual(values = c("#F99417","grey70"))+
  coord_polar(start = -pi/24*4)+
  ggtitle("bj")+
  theme_bw()+
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
  ggsave("Figure/230808_bj_radar_trans_rel_nolabel.png",height = 22, width = 20, units = "cm", dpi = 300)


##ss====
trsbyname_all_rd_mean_en2_ss=subset(trsbyname_all_rd_mean_en2,trsbyname_all_rd_mean_en2$Grouplab=="SS")

ggplot(trsbyname_all_rd_mean_en2_ss, aes(x=tlab, y=x,col=Event, group=Event))+
  geom_segment(aes(x=tlab, xend=tlab,y=0, yend=6), col="black")+
  geom_hline(yintercept = 2, lty=2)+
  geom_hline(yintercept = 4, lty=2)+
  geom_hline(yintercept = 6, lty=2)+
  #  geom_hline(yintercept = 800, lty=2)+
  annotate('text', x = 30.5, y = c(2.25,4.25,6.25), label = c('2','4','6'), size=6)+
  geom_text(aes(x=tlab,y=6.5,label=tlab2),angle=15, parse=T, col="black",size=4)+
  #  facet_wrap(.~Type)+
  geom_polygon(fill=NA, size=1)+
  geom_point(stat='identity', size=3.5) +
  scale_x_discrete(labels=labs)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_color_manual(values = c("#F99417","grey70"))+
  coord_polar(start = -pi/24*4)+
  ggtitle("ss")+
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
  ggsave(filename("ss_soc_radar_trans_ev"),height = 22, width = 20, units = "cm", dpi = 300, compression="lzw")

ggplot(trsbyname_all_rd_mean_en2_ss, aes(x=tlab, y=x,col=Event, group=Event))+
  geom_segment(aes(x=tlab, xend=tlab,y=0, yend=6), col="black")+
  geom_hline(yintercept = 2, lty=2)+
  geom_hline(yintercept = 4, lty=2)+
  geom_hline(yintercept = 6, lty=2)+
  #  geom_hline(yintercept = 800, lty=2)+
  annotate('text', x = 30.5, y = c(2.25,4.25,6.25), label = c('2','4','6'), size=6)+
  #geom_text(aes(x=tlab,y=6.5,label=tlab2),angle=15, parse=T, col="black",size=4)+
  #  facet_wrap(.~Type)+
  geom_polygon(fill=NA, size=1)+
  geom_point(stat='identity', size=3.5) +
  scale_x_discrete(labels=labs)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_color_manual(values = c("#F99417","grey70"))+
  coord_polar(start = -pi/24*4)+
  ggtitle("ss")+
  theme_bw()+
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
  ggsave("Figure/230808_ss_radar_trans_rel_nolabel.png",height = 22, width = 20, units = "cm", dpi = 300)


##se====
trsbyname_all_rd_mean_en2_se=subset(trsbyname_all_rd_mean_en2,trsbyname_all_rd_mean_en2$Grouplab=="SE")

ggplot(trsbyname_all_rd_mean_en2_se, aes(x=tlab, y=x,col=Event, group=Event))+
  geom_segment(aes(x=tlab, xend=tlab,y=0, yend=6), col="black")+
  geom_hline(yintercept = 2, lty=2)+
  geom_hline(yintercept = 4, lty=2)+
  geom_hline(yintercept = 6, lty=2)+
  #  geom_hline(yintercept = 800, lty=2)+
  annotate('text', x = 30.5, y = c(2.25,4.25,6.25), label = c('2','4','6'), size=6)+
  geom_text(aes(x=tlab,y=6.5,label=tlab2),angle=15, parse=T, col="black",size=4)+
  #  facet_wrap(.~Type)+
  geom_polygon(fill=NA, size=1)+
  geom_point(stat='identity', size=3.5) +
  scale_x_discrete(labels=labs)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_color_manual(values = c("#F99417","grey70"))+
  coord_polar(start = -pi/24*4)+
  ggtitle("se")+
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
  ggsave(filename("se_soc_radar_trans_ev"),height = 22, width = 20, units = "cm", dpi = 300, compression="lzw")

ggplot(trsbyname_all_rd_mean_en2_se, aes(x=tlab, y=x,col=Event, group=Event))+
  geom_segment(aes(x=tlab, xend=tlab,y=0, yend=6), col="black")+
  geom_hline(yintercept = 2, lty=2)+
  geom_hline(yintercept = 4, lty=2)+
  geom_hline(yintercept = 6, lty=2)+
  #  geom_hline(yintercept = 800, lty=2)+
  annotate('text', x = 30.5, y = c(2.25,4.25,6.25), label = c('2','4','6'), size=6)+
  #geom_text(aes(x=tlab,y=6.5,label=tlab2),angle=15, parse=T, col="black",size=4)+
  #  facet_wrap(.~Type)+
  geom_polygon(fill=NA, size=1)+
  geom_point(stat='identity', size=3.5) +
  scale_x_discrete(labels=labs)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_color_manual(values = c("#F99417","grey70"))+
  coord_polar(start = -pi/24*4)+
  ggtitle("se")+
  theme_bw()+
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
  ggsave("Figure/230808_se_radar_trans_rel_nolabel.png",height = 22, width = 20, units = "cm", dpi = 300)


##se====
trsbyname_all_rd_mean_en2_se=subset(trsbyname_all_rd_mean_en2,trsbyname_all_rd_mean_en2$Grouplab=="SE")

ggplot(trsbyname_all_rd_mean_en2_se, aes(x=tlab, y=x,col=Event, group=Event))+
  geom_segment(aes(x=tlab, xend=tlab,y=0, yend=6), col="black")+
  geom_hline(yintercept = 2, lty=2)+
  geom_hline(yintercept = 4, lty=2)+
  geom_hline(yintercept = 6, lty=2)+
  #  geom_hline(yintercept = 800, lty=2)+
  annotate('text', x = 30.5, y = c(2.25,4.25,6.25), label = c('2','4','6'), size=6)+
  geom_text(aes(x=tlab,y=6.5,label=tlab2),angle=15, parse=T, col="black",size=4)+
  #  facet_wrap(.~Type)+
  geom_polygon(fill=NA, size=1)+
  geom_point(stat='identity', size=3.5) +
  scale_x_discrete(labels=labs)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_color_manual(values = c("#F99417","grey70"))+
  coord_polar(start = -pi/24*4)+
  ggtitle("se")+
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
  ggsave(filename("se_soc_radar_trans_ev"),height = 22, width = 20, units = "cm", dpi = 300, compression="lzw")

ggplot(trsbyname_all_rd_mean_en2_se, aes(x=tlab, y=x,col=Event, group=Event))+
  geom_segment(aes(x=tlab, xend=tlab,y=0, yend=6), col="black")+
  geom_hline(yintercept = 2, lty=2)+
  geom_hline(yintercept = 4, lty=2)+
  geom_hline(yintercept = 6, lty=2)+
  #  geom_hline(yintercept = 800, lty=2)+
  annotate('text', x = 30.5, y = c(2.25,4.25,6.25), label = c('2','4','6'), size=6)+
  #geom_text(aes(x=tlab,y=6.5,label=tlab2),angle=15, parse=T, col="black",size=4)+
  #  facet_wrap(.~Type)+
  geom_polygon(fill=NA, size=1)+
  geom_point(stat='identity', size=3.5) +
  scale_x_discrete(labels=labs)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_color_manual(values = c("#F99417","grey70"))+
  coord_polar(start = -pi/24*4)+
  ggtitle("se")+
  theme_bw()+
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
  ggsave("Figure/230808_se_radar_trans_rel_nolabel.png",height = 22, width = 20, units = "cm", dpi = 300)

##nt====
trsbyname_all_rd_mean_en2_nt=subset(trsbyname_all_rd_mean_en2,trsbyname_all_rd_mean_en2$Grouplab=="NT")

ggplot(trsbyname_all_rd_mean_en2_nt, aes(x=tlab, y=x,col=Event, group=Event))+
  geom_segment(aes(x=tlab, xend=tlab,y=0, yend=6), col="black")+
  geom_hline(yintercept = 2, lty=2)+
  geom_hline(yintercept = 4, lty=2)+
  geom_hline(yintercept = 6, lty=2)+
  #  geom_hline(yintercept = 800, lty=2)+
  annotate('text', x = 30.5, y = c(2.25,4.25,6.25), label = c('2','4','6'), size=6)+
  geom_text(aes(x=tlab,y=6.5,label=tlab2),angle=15, parse=T, col="black",size=4)+
  #  facet_wrap(.~Type)+
  geom_polygon(fill=NA, size=1)+
  geom_point(stat='identity', size=3.5) +
  scale_x_discrete(labels=labs)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_color_manual(values = c("#F99417","grey70"))+
  coord_polar(start = -pi/24*4)+
  ggtitle("nt")+
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
  ggsave(filename("nt_soc_radar_trans_ev"),height = 22, width = 20, units = "cm", dpi = 300, compression="lzw")

ggplot(trsbyname_all_rd_mean_en2_nt, aes(x=tlab, y=x,col=Event, group=Event))+
  geom_segment(aes(x=tlab, xend=tlab,y=0, yend=8), col="black")+
  geom_hline(yintercept = 2, lty=2)+
  geom_hline(yintercept = 4, lty=2)+
  geom_hline(yintercept = 6, lty=2)+
  geom_hline(yintercept = 8, lty=2)+
  #  geom_hline(yintercept = 800, lty=2)+
  annotate('text', x = 30.5, y = c(2.25,4.25,6.25), label = c('2','4','6'), size=6)+
  #geom_text(aes(x=tlab,y=6.5,label=tlab2),angle=15, parse=T, col="black",size=4)+
  #  facet_wrap(.~Type)+
  geom_polygon(fill=NA, size=1)+
  geom_point(stat='identity', size=3.5) +
  scale_x_discrete(labels=labs)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_color_manual(values = c("#F99417","grey70"))+
  coord_polar(start = -pi/24*4)+
  ggtitle("nt")+
  theme_bw()+
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
  ggsave("Figure/230808_nt_radar_trans_rel_nolabel.png",height = 22, width = 20, units = "cm", dpi = 300)




