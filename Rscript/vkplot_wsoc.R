#2.vkplot======
frd_merge=subset(frd_merge,frd_merge$cnt>4)

fm_prop=unique(frd_merge[,c("Formula","O.C","H.C","AI","DBE","Molecularclass","AIClass","Comp")])

vk_frd=melt(frd_merge[,c("Group","pd","Formula","Bromo.Inty")], id.vars = c("Group","pd","Formula")) %>% 
  dcast(Group+pd~Formula, mean) %>% 
  melt(id.vars=c("Group","pd"), na.rm = T) %>% `colnames<-`(c("Group","pd","Formula","val"))

vk_frd=vk_frd %>% left_join(fm_prop)

vk_frd$Grouplab

vk_frd$Grouplab=factor(vk_frd$Group,
                       levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"),
                       labels = c("UT","BJ","SS","SE","NT"))

vk_frd$pdlab=factor(vk_frd$pd,
                    levels = c("1st","2nd","3rd"),
                    labels = c("1st (Winter)","2nd (Summer)","3rd (Spring)"))

vk_frd

vk_frd$ord=ifelse(vk_frd$Comp=="CHO",2,
                  ifelse(vk_frd$Comp=="CHON",3,
                         ifelse(vk_frd$Comp=="CHOS",4,
                                ifelse(vk_frd$Comp=="CHONS",5,6))))
vk_frd=vk_frd[order(vk_frd$ord,decreasing = T),]

frd_merge

cnt=aggregate(frd_merge$Freq, by=list(Group=frd_merge$Group, pd=frd_merge$pd, Formula=frd_merge$Formula),sum)
cnt$freq=1

cnt2=aggregate(cnt$freq, by=list(Group=cnt$Group, pd=cnt$pd),sum)

cnt2$Grouplab=factor(cnt2$Group,
                     levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"),
                     labels = c("UT","BJ","SS","SE","NT"))

cnt2$pdlab=factor(cnt2$pd,
                  levels = c("1st","2nd","3rd"),
                  labels = c("1st (Winter)","2nd (Summer)","3rd (Spring)"))

ggplot()+
  geom_hline(yintercept = 1.5,lty=2,size=1)+
  geom_abline(slope = -1, intercept = 1.1,lty=2,size=1)+
  geom_abline(slope = -0.76, intercept = 0.75, lty=2,size=1)+
  geom_point(data=vk_frd, aes(x=O.C, y=H.C,fill=Comp,col=Comp),size=2, shape=21, alpha=0.4)+
  geom_text(data=cnt2, aes(x=0.15, y=0.15, label=paste0("italic('n')=","=",x)),size=9, parse = TRUE)+
  facet_rep_wrap(.~Grouplab, repeat.tick.labels = "all", ncol=3)+
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
  ggsave(filename("vk_all"),height = 34, width = 48, units = "cm", dpi = 300, compression="lzw")


vk_frd_ub=subset(vk_frd,vk_frd$Grouplab=="UT")

vk_ub=ggplot()+
  geom_hline(yintercept = 1.5,lty=2,size=1)+
  geom_abline(slope = -1, intercept = 1.1,lty=2,size=1)+
  geom_abline(slope = -0.76, intercept = 0.75, lty=2,size=1)+
  geom_point(data=vk_frd, aes(x=O.C, y=H.C,fill=Comp,col=Comp),size=2, shape=21, alpha=0.4)+
  geom_text(data=cnt2, aes(x=0.15, y=0.15, label=paste0("italic('n')=","=",x)),size=9, parse = TRUE)+
  facet_rep_wrap(.~Grouplab, repeat.tick.labels = "all", ncol=3)+
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
  )

vk_ub

