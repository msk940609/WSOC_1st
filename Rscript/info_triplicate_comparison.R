
cam_merge=fread("Datafile/cam_merge_fin.csv")
cam_merge$Freq=1
cam_merge

#0 add chemcial info=====
cam_merge

cam_merge$lipid=ifelse(cam_merge$`O.C`>=0&cam_merge$`O.C`<0.3,
                       ifelse(cam_merge$`H.C`>1.5&cam_merge$`H.C`<2.0, "Lipids",""),"")
cam_merge$protein=ifelse(cam_merge$`O.C`>=0.3&cam_merge$`O.C`<=0.67,
                         ifelse(cam_merge$`H.C`>1.5&cam_merge$`H.C`<2.2,
                                ifelse(cam_merge$`N.C`>=0.05,"Proteins","") ,""),"")
cam_merge$carbo=ifelse(cam_merge$`O.C`>0.67&cam_merge$`O.C`<2.0,
                       ifelse(cam_merge$`H.C`>1.5&cam_merge$`H.C`<2.3, "Carbohydrates",""),"")
cam_merge$Unhydro=ifelse(cam_merge$`O.C`>0&cam_merge$`O.C`<0.1,
                         ifelse(cam_merge$`H.C`>=0.7&cam_merge$`H.C`<=1.5, "Unsaturated hydrocarbons",""),"")
cam_merge$lignin=ifelse(cam_merge$`O.C`>=0.1&cam_merge$`O.C`<=0.67,
                        ifelse(cam_merge$`H.C`>=0.7&cam_merge$`H.C`<=1.5, 
                               ifelse(cam_merge$AI<0.67, "Lignins",""),""),"")
cam_merge$tannin=ifelse(cam_merge$`O.C`>0.67&cam_merge$`O.C`<2.0,
                        ifelse(cam_merge$`H.C`>=0.5&cam_merge$`H.C`<=1.5, 
                               ifelse(cam_merge$AI<0.67, "Tannins",""),""),"")
cam_merge$conaro=ifelse(cam_merge$`O.C`>=0&cam_merge$`O.C`<=0.67,
                        ifelse(cam_merge$`H.C`>=0.2&cam_merge$`H.C`<0.67, 
                               ifelse(cam_merge$AI>=0.67, "Condensed aromatics",""),""),"")

cam_merge=cam_merge %>%  unite("Molecularclass",c("lipid","protein","carbo","Unhydro","lignin","tannin","conaro"), sep = "")
cam_merge$Molecularclass=ifelse(cam_merge$Molecularclass=="","Unassigned", cam_merge$Molecularclass) ###colum of molecular class

cam_merge$DBEAI=1+cam_merge$`C.`-0.5*cam_merge$`O.`-cam_merge$`S.`-0.5*cam_merge$`N.`-0.5*cam_merge$`H.`
cam_merge$CAI=cam_merge$`C.`-0.5*cam_merge$`O.`-cam_merge$`S.`-cam_merge$`N.`

cam_merge$AI=ifelse(cam_merge$DBEAI/cam_merge$CAI<0,0,cam_merge$DBEAI/cam_merge$CAI)

cam_merge$PCA=ifelse(cam_merge$AI>0.66, "Polycyclic condensed aromatics","")
cam_merge$aroma=ifelse(cam_merge$AI>0.5,ifelse(cam_merge$AI<=0.66,"Polyphenolic",""),"")
cam_merge$humic=ifelse(cam_merge$AI<=0.5,
                       ifelse(cam_merge$`H.C`<1.5,"Highly unsaturated and phenolic",""),"")
cam_merge$aliphatic=ifelse(cam_merge$`H.C`<2.0&cam_merge$`H.C`>=1.5,
                           ifelse(cam_merge$AI<=0.5,"Aliphatic compounds",""),"")
cam_merge=cam_merge %>%  unite("AIClass",c("PCA","aroma","humic","aliphatic"), sep = "")
cam_merge$AIClass=ifelse(cam_merge$AIClass=="","Saturated compounds", cam_merge$AIClass) ###colum of molecular class

cam_merge$AIClass=factor(cam_merge$AIClass, levels = c("Polycyclic condensed aromatics","Polyphenolic","Highly unsaturated and phenolic",
                                                       "Aliphatic compounds"))

cam_merge=cam_merge %>% mutate(Comp= gsub('[[:digit:]]', '', Formula))
cam_merge$Comp=ifelse(cam_merge$O.==0,"Remainders",cam_merge$Comp)

cam_merge$Comp=factor(cam_merge$Comp,levels = c("CHO","CHNO","CHOS","CHNOS","Remainders"),
                      labels = c("CHO","CHON","CHOS","CHONS","Remainders"))

cam_merge

#1. m/z distribution (avg reconstructed plot)=====
cam_merge
cam_mz=melt(cam_merge[,c("Calc.m.z","Bromo.Inty","Depth","Trt")], id.vars = c("Depth","Trt","Calc.m.z")) %>% 
  dcast(Depth+Trt~Calc.m.z, mean) %>% 
  melt(id.vars=c("Depth","Trt"))

cam_mz[is.na(cam_mz)] <- 0
cam_mz

cam_mz_sel=subset(cam_mz,cam_mz$value>0)
cam_mz_sel$Calc.m.z=as.numeric(as.character(cam_mz_sel$variable))

ggplot()+
  introdataviz::geom_flat_violin(data=cam_mz_sel, aes(x = 100000, y = Calc.m.z, width=50000),fill="grey50",scale="area",trim=T, alpha = 1,
                                 position = position_nudge(x = 0))+
  coord_flip(xlim=c(0,130000))+
  geom_segment(data=cam_mz_sel,aes(x=0,xend=value,y=Calc.m.z,yend=Calc.m.z),col="black",size=0.65)+
  facet_rep_wrap(Depth~Trt, scales = "free_y",ncol = 2,repeat.tick.labels = T)+ 
  scale_y_continuous(name = "", breaks = seq(0,1000,100), limits = c(200,620), expand=c(0.02,0.02))+
  scale_x_continuous(name = "",expand = c(0.02,0.02))+
  #scale_fill_manual(values = c("#7BC7FA","#DA5417"))+
  #scale_color_manual(values = c("#7BC7FA","#DA5417"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 2),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.text.x = element_text(size = 22, colour = "black",face = "bold", family = "Arial"),
        axis.text.y = element_text(size = 22, colour = "black",face = "bold", family = "Arial"),
        axis.title.x = element_text(size = 0.1, colour = "black",face = "bold", family = "Arial"),
        axis.title.y = element_text(size = 0.1, colour = "black",face = "bold", family = "Arial"),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(size=28, colour = "black",face = "bold",family = "Arial",
                                  margin = unit(c(0.6,0.4,0.2,0.2),"cm")),
        legend.text = element_text(size = 22,face=2, colour = "black", family = "Arial", vjust = 0.5,margin = unit(c(0.1,0.1,0.1,0.1),"cm")),
        legend.spacing = unit(0.0,"cm"),
        legend.key.width = unit(1.0,"cm"),
        legend.key.height = unit(1.0,"cm"),
        legend.title = element_text(size = 26, colour = "black", family = "Arial",face = "bold"),
        legend.position = c(0.82,1.025),
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.box.background = element_rect(fill=NA, colour = NA),
  )+
  #guides(color=guide_legend(title = "", override.aes = list(size=1.5)),fill="none")+
  guides(fill=guide_legend(title = ""),color="none")+
  ggsave(filename("avg_mz distribution"), height = 25, width = 50, units = "cm", dpi = 300)

#vkplot======
cam_merge

chp_info=unique(cam_merge[,c("Formula","O.C","H.C","Comp","AIClass")])

cam_vk=melt(cam_merge[,c("Formula","Bromo.Inty","Depth","Trt","Time")], id.vars = c("Depth","Trt","Time","Formula")) %>% 
  dcast(Depth+Trt~Formula, mean) %>% 
  melt(id.vars=c("Depth","Trt"),na.rm = T) %>% 
  `colnames<-`(c("Depth","Trt","Formula","Inty"))

cam_vk=cam_vk %>% left_join(chp_info)
cam_vk

table(cam_vk$Comp)

cam_vk_d1=subset(cam_vk,cam_vk$Depth=="D1")
table(cam_vk_d1$Trt)

cam_vk$Comp=factor(cam_vk$Comp,levels = c("CHO","CHON","CHOS","CHONS","Remainders"))

cam_vk=cam_vk[order(cam_vk$Comp),]

cam_vk$Dlab=factor(cam_vk$Depth, levels = c("D1","D2"),
                    labels = c("Oragnic layer","Mineral layer"))

cam_vk$tlab=factor(cam_vk$Trt, levels = c("Ctrl","W"),
                    labels = c("Control","Warming"))

ggplot(cam_vk, aes(x=O.C, y=H.C,col=Comp))+
  geom_hline(yintercept = 1.5,lty=2,size=1)+
  geom_abline(slope = -0.58, intercept = 1.1,lty=2,size=1)+
  geom_abline(slope = -0.5, intercept = 0.8, lty=2,size=1)+
  geom_point(aes(size=Inty), alpha=0.4)+
  facet_rep_grid(Dlab~tlab, repeat.tick.labels = "all",scales = "free")+
  scale_x_continuous(name = "O/C",expand = c(0.01,0.01),limits = c(-0.01,1.0), breaks = round(seq(0,2.1,0.1),1),
                     labels =insert_minor_2(c("0","0.2","0.4","0.6","0.8","1.0","1.2","1.4","1.6","1.8","2.0","2.2"),2))+
  scale_y_continuous(name = "H/C",expand = c(0.01,0.01), limits = c(0.0,2.2),breaks = round(seq(0.0,2.5,0.1),1),
                     labels =insert_minor_1(c("0","0.5","1.0","1.5","2.0","2.5"),4))+
  scale_color_manual(values = c("#BC3C29FF","#EFC000FF","#008B45FF","#5F559BFF","grey50"))+
  scale_size_continuous(range = c(2.5,8))+
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
        legend.title = element_text(size = 26, vjust = 0.75,colour = "black", family = "Arial",face = "bold"),
        #legend.position = "right",
        #legend.direction = "vertical",
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.background = element_blank(),
  )+
  guides(col=guide_legend(title = "",ncol = 5,override.aes = list(shape=21,size=7, fill=c("#BC3C29FF","#EFC000FF","#008B45FF","#7876B1FF","grey50"),alpha=1)), 
         size="none")+
  ggsave(filename("vk_avg"),height = 40, width = 40, units = "cm", dpi = 300)


##including sample period=====
cam_merge

chp_info=unique(cam_merge[,c("Formula","O.C","H.C","Comp","AIClass")])

cam_vk=melt(cam_merge[,c("Formula","Bromo.Inty","Depth","Trt","Time")], id.vars = c("Depth","Trt","Time","Formula")) %>% 
  dcast(Depth+Trt+Time~Formula, mean) %>% 
  melt(id.vars=c("Depth","Trt","Time"),na.rm = T) %>% 
  `colnames<-`(c("Depth","Trt","Time","Formula","Inty"))

cam_vk=cam_vk %>% left_join(chp_info)
cam_vk

table(cam_vk$Comp)

cam_vk_d1=subset(cam_vk,cam_vk$Depth=="D1")
table(cam_vk_d1$Trt)

cam_vk$Comp=factor(cam_vk$Comp,levels = c("CHO","CHON","CHOS","CHONS","Remainders"))
cam_vk=cam_vk[order(cam_vk$Comp),]

cam_vk$Dlab=factor(cam_vk$Depth, levels = c("D1","D2"),
                   labels = c("Oragnic layer","Mineral layer"))

cam_vk$tlab=factor(cam_vk$Trt, levels = c("Ctrl","W"),
                   labels = c("Control","Warming"))

cam_vk$smp=factor(cam_vk$Time, levels = c(1,2,3,4,5),
                   labels = c("6/28","7/14","7/31","8/17","9/2"))

cam_vk_d1=subset(cam_vk,cam_vk$Depth=="D1")
cam_vk_d2=subset(cam_vk,cam_vk$Depth=="D2")

cam_vk_d1
cam_vk_d1$Freq=1

tt=aggregate.data.frame(cam_vk_d1$Freq,by=list(D=cam_vk_d1$Depth, Trt=cam_vk_d1$Trt, smp=cam_vk_d1$smp),sum)

ggplot(cam_vk_d1, aes(x=O.C, y=H.C,col=Comp))+
  geom_hline(yintercept = 1.5,lty=2,size=1)+
  geom_abline(slope = -0.58, intercept = 1.1,lty=2,size=1)+
  geom_abline(slope = -0.5, intercept = 0.8, lty=2,size=1)+
  geom_point(aes(size=Inty), alpha=0.4)+
  facet_rep_grid(tlab~smp, repeat.tick.labels = "all",scales = "free")+
  scale_x_continuous(name = "O/C",expand = c(0.01,0.01),limits = c(-0.01,1.0), breaks = round(seq(0,2.1,0.1),1),
                     labels =insert_minor_2(c("0","0.2","0.4","0.6","0.8","1.0","1.2","1.4","1.6","1.8","2.0","2.2"),2))+
  scale_y_continuous(name = "H/C",expand = c(0.01,0.01), limits = c(0.0,2.2),breaks = round(seq(0.0,2.5,0.1),1),
                     labels =insert_minor_1(c("0","0.5","1.0","1.5","2.0","2.5"),4))+
  scale_color_manual(values = c("#BC3C29FF","#EFC000FF","#008B45FF","#5F559BFF","grey50"))+
  scale_size_continuous(range = c(2.5,8))+
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
        legend.title = element_text(size = 26, vjust = 0.75,colour = "black", family = "Arial",face = "bold"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.background = element_blank(),
  )+
  guides(col=guide_legend(title = "",ncol = 5,override.aes = list(shape=21,size=7, fill=c("#BC3C29FF","#EFC000FF","#008B45FF","#7876B1FF","grey50"),alpha=1)), 
         size="none")+
  ggsave(filename("vk_smp_d1"),height = 30, width = 75, units = "cm", dpi = 300)


ggplot(cam_vk_d2, aes(x=O.C, y=H.C,col=Comp))+
  geom_hline(yintercept = 1.5,lty=2,size=1)+
  geom_abline(slope = -0.58, intercept = 1.1,lty=2,size=1)+
  geom_abline(slope = -0.5, intercept = 0.8, lty=2,size=1)+
  geom_point(aes(size=Inty), alpha=0.4)+
  facet_rep_grid(tlab~smp, repeat.tick.labels = "all",scales = "free")+
  scale_x_continuous(name = "O/C",expand = c(0.01,0.01),limits = c(-0.01,1.0), breaks = round(seq(0,2.1,0.1),1),
                     labels =insert_minor_2(c("0","0.2","0.4","0.6","0.8","1.0","1.2","1.4","1.6","1.8","2.0","2.2"),2))+
  scale_y_continuous(name = "H/C",expand = c(0.01,0.01), limits = c(0.0,2.2),breaks = round(seq(0.0,2.5,0.1),1),
                     labels =insert_minor_1(c("0","0.5","1.0","1.5","2.0","2.5"),4))+
  scale_color_manual(values = c("#BC3C29FF","#EFC000FF","#008B45FF","#5F559BFF","grey50"))+
  scale_size_continuous(range = c(2.5,8))+
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
        legend.title = element_text(size = 26, vjust = 0.75,colour = "black", family = "Arial",face = "bold"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.background = element_blank(),
  )+
  guides(col=guide_legend(title = "",ncol = 5,override.aes = list(shape=21,size=7, fill=c("#BC3C29FF","#EFC000FF","#008B45FF","#7876B1FF","grey50"),alpha=1)), 
         size="none")+
  ggsave(filename("vk_smp_d2"),height = 30, width = 75, units = "cm", dpi = 300)


###venn diagram====
cam_merge

cam_test=cam_merge

cam_test$id=paste(cam_test$Depth,cam_test$Trt,cam_test$Time, sep = "_")

###Control=====
cam_test_ctr_t1=subset(cam_test,cam_test$id=="D1_Ctrl_1")

agg_ctr_t1=aggregate(cam_test_ctr_t1$Freq, by=list(D=cam_test_ctr_t1$Depth,Trt=cam_test_ctr_t1$Trt, Rep=cam_test_ctr_t1$Rep),sum)
agg_ctr_t1

mean(agg_ctr_t1$x)

cam_test_ctr_t1_r1=subset(cam_test_ctr_t1,cam_test_ctr_t1$Rep=="1")
cam_test_ctr_t1_r2=subset(cam_test_ctr_t1,cam_test_ctr_t1$Rep=="2")
cam_test_ctr_t1_r3=subset(cam_test_ctr_t1,cam_test_ctr_t1$Rep=="3")

ctr_t1_r1_fm=unique(cam_test_ctr_t1_r1$Formula)
ctr_t1_r2_fm=unique(cam_test_ctr_t1_r2$Formula)
ctr_t1_r3_fm=unique(cam_test_ctr_t1_r3$Formula)


ctr_t1_fm <- list(
  `B2` = ctr_t1_r1_fm,
  `B4` = ctr_t1_r2_fm,
  `B5` = ctr_t1_r3_fm
)

venn.diagram(ctr_t1_fm,
             fill =  c("#3C5488FF","#E64B35FF","#4DBBD5FF"),
             cat.dist = c(0.1, 0.1, 0.1),
             lty = 1,  lwd = 1,
             cat.cex=0,
             cex=2,
             cat.fontfamily= "Arial",main.fontface = "bold",sub.fontface = "bold",
             fontfamily= "Arial",
             filename = "venn-Ctr_t1.tiff")

99+87+103+1408+395+77+386
395+1408+77+87

100+127+210+59+145+1658+95
127+1658+59+145

###warming====
cam_test_w_t1=subset(cam_test,cam_test$id=="D1_W_1")

agg_w_t1=aggregate(cam_test_w_t1$Freq, by=list(D=cam_test_w_t1$Depth,Trt=cam_test_w_t1$Trt, Rep=cam_test_w_t1$Rep),sum)
agg_w_t1
mean(agg_w_t1$x)

cam_test_w_t1_r1=subset(cam_test_w_t1,cam_test_w_t1$Rep=="1")
cam_test_w_t1_r2=subset(cam_test_w_t1,cam_test_w_t1$Rep=="2")
cam_test_w_t1_r3=subset(cam_test_w_t1,cam_test_w_t1$Rep=="3")

cam_test_w_t1_r1
cam_test_w_t1_r2
cam_test_w_t1_r3

w_t1_r1_fm=unique(cam_test_w_t1_r1$Formula)
w_t1_r2_fm=unique(cam_test_w_t1_r2$Formula)
w_t1_r3_fm=unique(cam_test_w_t1_r3$Formula)

w_t1_fm <- list(
  B2 = w_t1_r1_fm,
  B4 = w_t1_r2_fm,
  B5 = w_t1_r3_fm
)

venn.diagram(w_t1_fm,
             fill =  c("#3C5488FF","#E64B35FF","#4DBBD5FF"),
             cat.dist = c(0.1, 0.1, 0.1),
             lty = 1,  lwd = 1,
             cat.cex=0,
             cex=2,
             cat.fontfamily= "Arial",main.fontface = "bold",sub.fontface = "bold",
             fontfamily= "Arial",
             filename = "venn-w_t1.tiff")


##t2=====
###Control=====
cam_test_ctr_t2=subset(cam_test,cam_test$id=="D1_Ctrl_2")

agg_ctr_t2=aggregate(cam_test_ctr_t2$Freq, by=list(D=cam_test_ctr_t2$Depth,Trt=cam_test_ctr_t2$Trt, Rep=cam_test_ctr_t2$Rep),sum)
agg_ctr_t2

mean(agg_ctr_t2$x)
length(unique(cam_test_ctr_t2$Formula))


cam_test_ctr_t2_r1=subset(cam_test_ctr_t2,cam_test_ctr_t2$Rep=="1")
cam_test_ctr_t2_r2=subset(cam_test_ctr_t2,cam_test_ctr_t2$Rep=="2")
cam_test_ctr_t2_r3=subset(cam_test_ctr_t2,cam_test_ctr_t2$Rep=="3")

ctr_t2_r1_fm=unique(cam_test_ctr_t2_r1$Formula)
ctr_t2_r2_fm=unique(cam_test_ctr_t2_r2$Formula)
ctr_t2_r3_fm=unique(cam_test_ctr_t2_r3$Formula)


ctr_t2_fm <- list(
  `B2` = ctr_t2_r1_fm,
  `B4` = ctr_t2_r2_fm,
  `B5` = ctr_t2_r3_fm
)

venn.diagram(ctr_t2_fm,
             fill =  c("#3C5488FF","#E64B35FF","#4DBBD5FF"),
             cat.dist = c(0.1, 0.1, 0.1),
             lty = 1,  lwd = 1,
             cat.cex=0,
             cex=2,
             cat.fontfamily= "Arial",main.fontface = "bold",sub.fontface = "bold",
             fontfamily= "Arial",
             filename = "venn-Ctr_t2.tiff")

###warming====
cam_test_w_t2=subset(cam_test,cam_test$id=="D1_W_2")

agg_w_t2=aggregate(cam_test_w_t2$Freq, by=list(D=cam_test_w_t2$Depth,Trt=cam_test_w_t2$Trt, Rep=cam_test_w_t2$Rep),sum)
agg_w_t2

mean(agg_w_t2$x)
length(unique(cam_test_w_t2$Formula))


cam_test_w_t2_r1=subset(cam_test_w_t2,cam_test_w_t2$Rep=="1")
cam_test_w_t2_r2=subset(cam_test_w_t2,cam_test_w_t2$Rep=="2")
cam_test_w_t2_r3=subset(cam_test_w_t2,cam_test_w_t2$Rep=="3")

cam_test_w_t2_r1
cam_test_w_t2_r2
cam_test_w_t2_r3

w_t2_r1_fm=unique(cam_test_w_t2_r1$Formula)
w_t2_r2_fm=unique(cam_test_w_t2_r2$Formula)
w_t2_r3_fm=unique(cam_test_w_t2_r3$Formula)

w_t2_fm <- list(
  B2 = w_t2_r1_fm,
  B4 = w_t2_r2_fm,
  B5 = w_t2_r3_fm
)

venn.diagram(w_t2_fm,
             fill =  c("#3C5488FF","#E64B35FF","#4DBBD5FF"),
             cat.dist = c(0.1, 0.1, 0.1),
             lty = 1,  lwd = 1,
             cat.cex=0,
             cex=2,
             cat.fontfamily= "Arial",main.fontface = "bold",sub.fontface = "bold",
             fontfamily= "Arial",
             filename = "venn-w_t2.tiff")



mean(agg_w_t2$x)-mean(agg_ctr_t2$x) ##gap:49
length(unique(cam_test_w_t2$Formula))-length(unique(cam_test_ctr_t2$Formula)) ##gap:10

1448+358+83+41
1538+196+206+37 #gap:47

##t3=====
###Control=====
cam_test_ctr_t3=subset(cam_test,cam_test$id=="D1_Ctrl_3")

agg_ctr_t3=aggregate(cam_test_ctr_t3$Freq, by=list(D=cam_test_ctr_t3$Depth,Trt=cam_test_ctr_t3$Trt, Rep=cam_test_ctr_t3$Rep),sum)
agg_ctr_t3

mean(agg_ctr_t3$x)
length(unique(cam_test_ctr_t3$Formula))

cam_test_ctr_t3_r1=subset(cam_test_ctr_t3,cam_test_ctr_t3$Rep=="1")
cam_test_ctr_t3_r2=subset(cam_test_ctr_t3,cam_test_ctr_t3$Rep=="2")
cam_test_ctr_t3_r3=subset(cam_test_ctr_t3,cam_test_ctr_t3$Rep=="3")

ctr_t3_r1_fm=unique(cam_test_ctr_t3_r1$Formula)
ctr_t3_r2_fm=unique(cam_test_ctr_t3_r2$Formula)
ctr_t3_r3_fm=unique(cam_test_ctr_t3_r3$Formula)

ctr_t3_fm <- list(
  `B2` = ctr_t3_r1_fm,
  `B4` = ctr_t3_r2_fm,
  `B5` = ctr_t3_r3_fm
)

venn.diagram(ctr_t3_fm,
             fill =  c("#3C5488FF","#E64B35FF","#4DBBD5FF"),
             cat.dist = c(0.1, 0.1, 0.1),
             lty = 1,  lwd = 1,
             cat.cex=0,
             cex=2,
             cat.fontfamily= "Arial",main.fontface = "bold",sub.fontface = "bold",
             fontfamily= "Arial",
             filename = "venn-Ctr_t3.tiff")

###warming====
cam_test_w_t3=subset(cam_test,cam_test$id=="D1_W_3")

agg_w_t3=aggregate(cam_test_w_t3$Freq, by=list(D=cam_test_w_t3$Depth,Trt=cam_test_w_t3$Trt, Rep=cam_test_w_t3$Rep),sum)
agg_w_t3

mean(agg_w_t3$x)
length(unique(cam_test_w_t3$Formula))

cam_test_w_t3_r1=subset(cam_test_w_t3,cam_test_w_t3$Rep=="1")
cam_test_w_t3_r2=subset(cam_test_w_t3,cam_test_w_t3$Rep=="2")
cam_test_w_t3_r3=subset(cam_test_w_t3,cam_test_w_t3$Rep=="3")

cam_test_w_t3_r1
cam_test_w_t3_r2
cam_test_w_t3_r3

w_t3_r1_fm=unique(cam_test_w_t3_r1$Formula)
w_t3_r2_fm=unique(cam_test_w_t3_r2$Formula)
w_t3_r3_fm=unique(cam_test_w_t3_r3$Formula)

w_t3_fm <- list(
  B2 = w_t3_r1_fm,
  B4 = w_t3_r2_fm,
  B5 = w_t3_r3_fm
)

venn.diagram(w_t3_fm,
             fill =  c("#3C5488FF","#E64B35FF","#4DBBD5FF"),
             cat.dist = c(0.1, 0.1, 0.1),
             lty = 1,  lwd = 1,
             cat.cex=0,
             cex=2,
             cat.fontfamily= "Arial",main.fontface = "bold",sub.fontface = "bold",
             fontfamily= "Arial",
             filename = "venn-w_t3.tiff")


mean(agg_w_t3$x)-mean(agg_ctr_t3$x) ##gap:243.7
length(unique(cam_test_w_t3$Formula))-length(unique(cam_test_ctr_t3$Formula)) ##gap:95

1379+106+286+60
1706+147+129+158 #gap:309

##t4=====
###Control=====
cam_test_ctr_t4=subset(cam_test,cam_test$id=="D1_Ctrl_4")

agg_ctr_t4=aggregate(cam_test_ctr_t4$Freq, by=list(D=cam_test_ctr_t4$Depth,Trt=cam_test_ctr_t4$Trt, Rep=cam_test_ctr_t4$Rep),sum)
agg_ctr_t4

mean(agg_ctr_t4$x)
length(unique(cam_test_ctr_t4$Formula))

cam_test_ctr_t4_r1=subset(cam_test_ctr_t4,cam_test_ctr_t4$Rep=="1")
cam_test_ctr_t4_r2=subset(cam_test_ctr_t4,cam_test_ctr_t4$Rep=="2")
cam_test_ctr_t4_r3=subset(cam_test_ctr_t4,cam_test_ctr_t4$Rep=="3")

ctr_t4_r1_fm=unique(cam_test_ctr_t4_r1$Formula)
ctr_t4_r2_fm=unique(cam_test_ctr_t4_r2$Formula)
ctr_t4_r3_fm=unique(cam_test_ctr_t4_r3$Formula)

ctr_t4_fm <- list(
  `B2` = ctr_t4_r1_fm,
  `B4` = ctr_t4_r2_fm,
  `B5` = ctr_t4_r3_fm
)

venn.diagram(ctr_t4_fm,
             fill =  c("#3C5488FF","#E64B35FF","#4DBBD5FF"),
             cat.dist = c(0.1, 0.1, 0.1),
             lty = 1,  lwd = 1,
             cat.cex=0,
             cex=2,
             cat.fontfamily= "Arial",main.fontface = "bold",sub.fontface = "bold",
             fontfamily= "Arial",
             filename = "venn-Ctr_t4.tiff")

###warming====
cam_test_w_t4=subset(cam_test,cam_test$id=="D1_W_4")

agg_w_t4=aggregate(cam_test_w_t4$Freq, by=list(D=cam_test_w_t4$Depth,Trt=cam_test_w_t4$Trt, Rep=cam_test_w_t4$Rep),sum)
agg_w_t4

mean(agg_w_t4$x)
length(unique(cam_test_w_t4$Formula))

cam_test_w_t4_r1=subset(cam_test_w_t4,cam_test_w_t4$Rep=="1")
cam_test_w_t4_r2=subset(cam_test_w_t4,cam_test_w_t4$Rep=="2")
cam_test_w_t4_r3=subset(cam_test_w_t4,cam_test_w_t4$Rep=="3")

cam_test_w_t4_r1
cam_test_w_t4_r2
cam_test_w_t4_r3

w_t4_r1_fm=unique(cam_test_w_t4_r1$Formula)
w_t4_r2_fm=unique(cam_test_w_t4_r2$Formula)
w_t4_r3_fm=unique(cam_test_w_t4_r3$Formula)

w_t4_fm <- list(
  B2 = w_t4_r1_fm,
  B4 = w_t4_r2_fm,
  B5 = w_t4_r3_fm
)

venn.diagram(w_t4_fm,
             fill =  c("#3C5488FF","#E64B35FF","#4DBBD5FF"),
             cat.dist = c(0.1, 0.1, 0.1),
             lty = 1,  lwd = 1,
             cat.cex=0,
             cex=2,
             cat.fontfamily= "Arial",main.fontface = "bold",sub.fontface = "bold",
             fontfamily= "Arial",
             filename = "venn-w_t4.tiff")


mean(agg_w_t4$x)-mean(agg_ctr_t4$x) ##gap:160.7
length(unique(cam_test_w_t4$Formula))-length(unique(cam_test_ctr_t4$Formula)) ##gap:62

985+101+320+41
1197+257+107+94 #gap:208

##t5=====
###Control=====
cam_test_ctr_t5=subset(cam_test,cam_test$id=="D1_Ctrl_5")

agg_ctr_t5=aggregate(cam_test_ctr_t5$Freq, by=list(D=cam_test_ctr_t5$Depth,Trt=cam_test_ctr_t5$Trt, Rep=cam_test_ctr_t5$Rep),sum)
agg_ctr_t5

mean(agg_ctr_t5$x)
length(unique(cam_test_ctr_t5$Formula))

cam_test_ctr_t5_r1=subset(cam_test_ctr_t5,cam_test_ctr_t5$Rep=="1")
cam_test_ctr_t5_r2=subset(cam_test_ctr_t5,cam_test_ctr_t5$Rep=="2")
cam_test_ctr_t5_r3=subset(cam_test_ctr_t5,cam_test_ctr_t5$Rep=="3")

ctr_t5_r1_fm=unique(cam_test_ctr_t5_r1$Formula)
ctr_t5_r2_fm=unique(cam_test_ctr_t5_r2$Formula)
ctr_t5_r3_fm=unique(cam_test_ctr_t5_r3$Formula)

ctr_t5_fm <- list(
  `B2` = ctr_t5_r1_fm,
  `B4` = ctr_t5_r2_fm,
  `B5` = ctr_t5_r3_fm
)

venn.diagram(ctr_t5_fm,
             fill =  c("#3C5488FF","#E64B35FF","#4DBBD5FF"),
             cat.dist = c(0.1, 0.1, 0.1),
             lty = 1,  lwd = 1,
             cat.cex=0,
             cex=2,
             cat.fontfamily= "Arial",main.fontface = "bold",sub.fontface = "bold",
             fontfamily= "Arial",
             filename = "venn-Ctr_t5.tiff")

###warming====
cam_test_w_t5=subset(cam_test,cam_test$id=="D1_W_5")

agg_w_t5=aggregate(cam_test_w_t5$Freq, by=list(D=cam_test_w_t5$Depth,Trt=cam_test_w_t5$Trt, Rep=cam_test_w_t5$Rep),sum)
agg_w_t5

mean(agg_w_t5$x)
length(unique(cam_test_w_t5$Formula))

cam_test_w_t5_r1=subset(cam_test_w_t5,cam_test_w_t5$Rep=="1")
cam_test_w_t5_r2=subset(cam_test_w_t5,cam_test_w_t5$Rep=="2")
cam_test_w_t5_r3=subset(cam_test_w_t5,cam_test_w_t5$Rep=="3")

cam_test_w_t5_r1
cam_test_w_t5_r2
cam_test_w_t5_r3

w_t5_r1_fm=unique(cam_test_w_t5_r1$Formula)
w_t5_r2_fm=unique(cam_test_w_t5_r2$Formula)
w_t5_r3_fm=unique(cam_test_w_t5_r3$Formula)

w_t5_fm <- list(
  B2 = w_t5_r1_fm,
  B4 = w_t5_r2_fm,
  B5 = w_t5_r3_fm
)

venn.diagram(w_t5_fm,
             fill =  c("#3C5488FF","#E64B35FF","#4DBBD5FF"),
             cat.dist = c(0.1, 0.1, 0.1),
             lty = 1,  lwd = 1,
             cat.cex=0,
             cex=2,
             cat.fontfamily= "Arial",main.fontface = "bold",sub.fontface = "bold",
             fontfamily= "Arial",
             filename = "venn-w_t5.tiff")

mean(agg_w_t5$x)-mean(agg_ctr_t5$x) ##gap:69
length(unique(cam_test_w_t5$Formula))-length(unique(cam_test_ctr_t5$Formula)) ##gap:180

1370+57+124+193
1334+132+285+56 #gap:63

