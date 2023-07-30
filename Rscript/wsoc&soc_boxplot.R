cor_all

frd_merge_soc=frd_merge %>% left_join(cor_all[,c("Group","Formula","Type")])
frd_merge_soc$Type2=ifelse(is.na(frd_merge_soc$Type),"WSOC",frd_merge_soc$Type)

#frd_merge_wsoc=frd_merge
#frd_merge_soc_fin=subset(frd_merge_soc,frd_merge_soc$Type2=="SOC")

frd_comp=aggregate(frd_merge_soc$Bromo.Inty, by=list(Group=frd_merge_soc$Group, No=frd_merge_soc$No, Type2=frd_merge_soc$Type2, Comp=frd_merge_soc$Comp),sum)
frd_comp_tot=aggregate(frd_merge_soc$Bromo.Inty, by=list(Group=frd_merge_soc$Group, No=frd_merge_soc$No),sum) %>% `colnames<-`(c("Group","No","tot"))
#frd_comp_tot=aggregate(frd_merge_soc$Bromo.Inty, by=list(Group=frd_merge_soc$Group, No=frd_merge_soc$No,Type2=frd_merge_soc$Type2),sum) %>% `colnames<-`(c("Group","No","Type2","tot"))

frd_comp=frd_comp %>% left_join(frd_comp_tot)
frd_comp$rel=frd_comp$x/frd_comp$tot*100

frd_comp_sel=subset(frd_comp,frd_comp$Comp!="Remainders")
frd_comp_sel$Typelab=factor(frd_comp_sel$Type2, levels = c("WSOC","SOC"))
frd_comp_sel$Grouplab=factor(frd_comp_sel$Group, levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"),
                             labels = c("UB","BJ","SS","SE","NT"))

ggplot()+
  geom_boxplot(data = frd_comp_sel, aes(x=Grouplab, y=rel, fill=Typelab))+
  facet_rep_wrap(.~Comp,repeat.tick.labels = T, scales = "free")+
  scale_fill_manual(values =c("#9DBCD4","#CB7723"))+
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
  guides(col="none",shape="none", fill="none")+
  ggsave(filename("comp_dis_all"),height = 20, width = 25, units = "cm", dpi = 300, compression="lzw")




##chp====
frd_merge_soc$NO3=frd_merge_soc$O./3*frd_merge_soc$N.
frd_merge_soc$SO4=frd_merge_soc$O./4*frd_merge_soc$S.
frd_merge_soc$NOS=frd_merge_soc$O./(3*frd_merge_soc$`N.`+4*frd_merge_soc$`S.`)

frd_merge_soc_sel=subset(frd_merge_soc,frd_merge_soc$NOS!="Inf")

frd_merge_soc_chp=melt(frd_merge_soc_sel[,c("Group","No","Type2","AI","DBE","O.C","H.C","NO3","SO4","NOS")],
                       id.vars=c("Group","No","Type2"), na.rm = T) %>% 
  dcast(Group+No+Type2~variable, mean) %>% 
  melt(id.vars=c("Group","No","Type2"))


frd_merge_soc_chp
frd_merge_soc_chp$Typelab=factor(frd_merge_soc_chp$Type2, levels = c("WSOC","SOC"))
frd_merge_soc_chp$Grouplab=factor(frd_merge_soc_chp$Group, levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"),
                             labels = c("UB","BJ","SS","SE","NT"))

frd_merge_soc_chp$varlab=factor(frd_merge_soc_chp$variable, levels = c("AI","DBE","H.C","O.C","NO3","SO4","NOS"),
                                labels = c("AI","DBE","H/C","O/C","O/3N","O/4S","O/(3N+4S)"))

ggplot()+
  geom_boxplot(data = frd_merge_soc_chp, aes(x=Grouplab, y=value, fill=Typelab))+
  facet_rep_wrap(.~varlab,repeat.tick.labels = T, scales = "free", ncol=4)+
  scale_fill_manual(values =c("#9DBCD4","#CB7723"))+
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
  guides(col="none",shape="none", fill="none")+
  ggsave(filename("chp_dis_all"),height = 20, width = 45, units = "cm", dpi = 300, compression="lzw")


  