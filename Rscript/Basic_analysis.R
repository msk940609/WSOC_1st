
#MDS analysis================
frd_merge

frd_all=melt(frd_merge[,c("id","pd","Group","No","Formula","Bromo.Inty")], id.vars = c("id","pd","Group","No","Formula")) %>% 
  dcast(id+pd+Group+No~Formula, sum)

frd_all[,1:23]

PC_all=prcomp(frd_all[,-c(1:4)], center = T, scale. = F)
PC_all 

gpca_all=as.data.table(PC_all$x)
gpca_all$id=frd_all$id

gpca_all=gpca_all %>% inner_join(frd_id)

gpca_all

ggplot()+
  geom_point(data = gpca_all, aes(x=PC1, y=PC2, col=Group))


###1. NMDS======
MDS_all=melt(frd_merge[,c("id","pd","Group","No","Formula","Bromo.Inty")], id.vars = c("id","pd","Group","No","Formula")) %>% 
  dcast(id+pd+Group+No~Formula, sum)

MDS_all[,1:23]

MDS_all_in=MDS_all

NMDS_all=metaMDS(MDS_all_in[,-c(1:4)], k=5, distance = "bray", trymax = 20)
NMDS_all ##Stress 0.08

gnmds_all=as.data.table(NMDS_all$points)
gnmds_all$id=MDS_all_in$id

frd_id=MDS_all_in[,1:4]
gnmds_all=gnmds_all %>% inner_join(frd_id)
gnmds_all

ggplot()+
  geom_point(data = gnmds_all, aes(x=MDS1, y=MDS2, col=Group, shape=pd))


gnmds_all$Group=factor(gnmds_all$Group,
                       levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"))


gnmds_all$Grouplab=factor(gnmds_all$Group,
                       levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"),
                       labels = c("UB","BJ","SS","SE","NT"))

gnmds_all$pdlab=factor(gnmds_all$pd,
                          levels = c("1st","2nd","3rd"),
                          labels = c("Winter","Summer","Spring"))

gnmds_all$pdlab=factor(gnmds_all$pdlab,
                       levels = c("Winter","Spring","Summer"))


#gnmds_all$id=ifelse(gnmds_all$Group=="Beijing",5,
#                    ifelse(gnmds_all$Group=="Seosan",4,
#                           ifelse(gnmds_all$Group=="seoul",3,2)))
#gnmds_all=gnmds_all[order(gnmds_all$id)]
gnmds_all

ggplot()+
  geom_vline(xintercept = 0, lty="dashed")+
  geom_hline(yintercept = 0,lty="dashed")+
  geom_point(data = gnmds_all, aes(x=MDS1, y=MDS2, fill=pdlab, shape=Grouplab), col="black", size=5)+
  scale_x_continuous(name="NMDS1")+
  scale_y_continuous(name="NMDS2")+
  scale_fill_manual(values = c("#89CFF0","#228B22","#FFA500"))+
  scale_shape_manual(values = c(21,22,23,24,25))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 2),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.text.x = element_text(size = 22, colour = "black",face = "bold", family = "Arial"),
        axis.text.y = element_text(size = 22, colour = "black",face = "bold", family = "Arial"),
        axis.title.x = element_text(size = 22, colour = "black",face = "bold", family = "Arial"),
        axis.title.y = element_text(size = 22, colour = "black",face = "bold", family = "Arial"),
        legend.text = element_text(size = 22, colour = "black", family = "Arial", vjust = 0.6,margin = unit(c(0.1,-0.2,0.1,-0.2),"cm")),
        legend.spacing = unit(0.0,"cm"),
        legend.title = element_text(size = 26, colour = "black", family = "Arial",face = "bold"),
        legend.position = "right",
        legend.direction = "vertical",
        legend.box = "vertical",
        legend.background =element_rect(fill="white")
  )+
  guides(shape=guide_legend(title = "Group",order=2,title.hjust = 0.5,byrow = T , ncol = 1,
                            override.aes = list(size=7, shape=c(21,22,23,24,25), fill=c("black","black","black","black","black"))),
             fill=guide_legend(title = "Season",order=1,title.hjust = 0.5,byrow = T , ncol = 1,
                           override.aes = list(size=7, shape=c(21,21,21)))
              )+
  ggsave(filename("MDS_all"), height = 20, width = 24, units = "cm", dpi = 300, compression="lzw")


###1st====
frd_merge_1st=subset(frd_merge,frd_merge$pd=="1st")

MDS_1st=melt(frd_merge_1st[,c("id","pd","Group","No","Formula","Bromo.Inty")], id.vars = c("id","pd","Group","No","Formula")) %>% 
  dcast(id+pd+Group+No~Formula, sum)

MDS_1st[,1:23]

MDS_1st_in=MDS_1st

NMDS_1st=metaMDS(MDS_1st_in[,-c(1:4)], k=2, distance = "bray", trymax = 20)
NMDS_1st ##Stress 0.02

gnmds_1st=as.data.table(NMDS_1st$points)
gnmds_1st$id=MDS_1st_in$id

frd_id=MDS_1st_in[,1:4]

gnmds_1st=gnmds_1st %>% inner_join(frd_id)
gnmds_1st

gnmds_1st$Group=factor(gnmds_1st$Group,
                       levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"))

gnmds_1st$Grouplab=factor(gnmds_1st$Group,
                          levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"),
                          labels = c("UB","BJ","SS","SE","NT"))

gnmds_1st$pdlab=factor(gnmds_1st$pd,
                       levels = c("1st","2nd","3rd"),
                       labels = c("1st (2020-2021)","2nd (2021)","3rd (2023)"))
gnmds_1st

ggplot()+
  geom_vline(xintercept = 0, lty="dashed")+
  geom_hline(yintercept = 0,lty="dashed")+
  geom_point(data = gnmds_1st, aes(x=MDS1, y=MDS2, fill=Grouplab, shape=pdlab), col="black", size=7)+
  scale_fill_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  scale_shape_manual(values = c(21,22,23,24,25), guide=F)+
  scale_x_continuous(name="NMDS1")+
  scale_y_continuous(name="NMDS2")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 2),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.text.x = element_text(size = 22, colour = "black",face = "bold", family = "Arial"),
        axis.text.y = element_text(size = 22, colour = "black",face = "bold", family = "Arial"),
        axis.title.x = element_text(size = 22, colour = "black",face = "bold", family = "Arial",margin = unit(c(0.2,0.2,0.2,0.2),"cm")),
        axis.title.y = element_text(size = 22, colour = "black",face = "bold", family = "Arial"),
        legend.text = element_text(size = 22, colour = "black", family = "Arial", vjust = 0.6,margin = unit(c(0.2,0.2,0.2,0.2),"cm")),
        legend.spacing = unit(0.0,"cm"),
        legend.title = element_text(size = 26, colour = "black", family = "Arial",face = "bold"),
        legend.position = "right",
        legend.direction = "vertical",
        legend.box = "vertical",
        legend.background =element_rect(fill="white")
  )+
  guides(fill=guide_legend(title = "Group",order=1,title.hjust = 0.5,byrow = T , ncol = 1,
                           override.aes = list(size=7, shape=c(21,21,21,21,21))),
         shape=NULL
         #shape=guide_legend(title = "Campaign",order=2,title.hjust = 0.5,byrow = T , ncol = 1,
        #                    override.aes = list(size=7, shape=c(21,22,23), fill=c("black","black","black")))
  )+
  ggsave(filename("MDS_1st"), height = 20, width = 23, units = "cm", dpi = 300, compression="lzw")


###2nd====
frd_merge_2nd=subset(frd_merge,frd_merge$pd=="2nd")

MDS_2nd=melt(frd_merge_2nd[,c("id","pd","Group","No","Formula","Bromo.Inty")], id.vars = c("id","pd","Group","No","Formula")) %>% 
  dcast(id+pd+Group+No~Formula, sum)

MDS_2nd[,1:23]

MDS_2nd_in=MDS_2nd

NMDS_2nd=metaMDS(MDS_2nd_in[,-c(1:4)], k=5, distance = "bray", trymax = 20)
NMDS_2nd ##Stress 0.03

gnmds_2nd=as.data.table(NMDS_2nd$points)
gnmds_2nd$id=MDS_2nd_in$id

frd_id=MDS_2nd_in[,1:4]

gnmds_2nd=gnmds_2nd %>% inner_join(frd_id)
gnmds_2nd

gnmds_2nd$Group=factor(gnmds_2nd$Group,
                       levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"))

gnmds_2nd$Grouplab=factor(gnmds_2nd$Group,
                          levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"),
                          labels = c("UB","BJ","SS","SE","NT"))

gnmds_2nd$pdlab=factor(gnmds_2nd$pd,
                       levels = c("1st","2nd","3rd"),
                       labels = c("1st (2020-2021)","2nd (2021)","3rd (2023)"))

gnmds_2nd

ggplot()+
  geom_vline(xintercept = 0, lty="dashed")+
  geom_hline(yintercept = 0,lty="dashed")+
  geom_point(data = gnmds_2nd, aes(x=MDS1, y=MDS2, fill=Grouplab,shape=pdlab), col="black", size=7)+
  scale_fill_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  scale_shape_manual(values = c(21,22,23,24,25), guide=F)+
  scale_x_continuous(name="NMDS1")+
  scale_y_continuous(name="NMDS2")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 2),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.text.x = element_text(size = 22, colour = "black",face = "bold", family = "Arial"),
        axis.text.y = element_text(size = 22, colour = "black",face = "bold", family = "Arial"),
        axis.title.x = element_text(size = 22, colour = "black",face = "bold", family = "Arial",margin = unit(c(0.2,0.2,0.2,0.2),"cm")),
        axis.title.y = element_text(size = 22, colour = "black",face = "bold", family = "Arial"),
        legend.text = element_text(size = 22, colour = "black", family = "Arial", vjust = 0.6,margin = unit(c(0.2,0.2,0.2,0.2),"cm")),
        legend.spacing = unit(0.0,"cm"),
        legend.title = element_text(size = 26, colour = "black", family = "Arial",face = "bold"),
        legend.position = "right",
        legend.direction = "vertical",
        legend.box = "vertical",
        legend.background =element_rect(fill="white")
  )+
  guides(fill=guide_legend(title = "Group",order=1,title.hjust = 0.5,byrow = T , ncol = 1,
                           override.aes = list(size=7, shape=c(21,21,21,21,21))),
         shape=NULL
         #shape=guide_legend(title = "Campaign",order=2,title.hjust = 0.5,byrow = T , ncol = 1,
         #                    override.aes = list(size=7, shape=c(21,22,23), fill=c("black","black","black")))
  )+
  ggsave(filename("MDS_2nd"), height = 20, width = 23, units = "cm", dpi = 300, compression="lzw")

##3rd====
frd_merge_3rd=subset(frd_merge,frd_merge$pd=="3rd")

MDS_3rd=melt(frd_merge_3rd[,c("id","pd","Group","No","Formula","Bromo.Inty")], id.vars = c("id","pd","Group","No","Formula")) %>% 
  dcast(id+pd+Group+No~Formula, sum)

MDS_3rd[,1:23]

MDS_3rd_in=MDS_3rd

NMDS_3rd=metaMDS(MDS_3rd_in[,-c(1:4)], k=5, distance = "bray", trymax = 20)
NMDS_3rd ##Stress 0.04

gnmds_3rd=as.data.table(NMDS_3rd$points)
gnmds_3rd$id=MDS_3rd_in$id

frd_id=MDS_3rd_in[,1:4]

gnmds_3rd=gnmds_3rd %>% inner_join(frd_id)
gnmds_3rd

gnmds_3rd$Group=factor(gnmds_3rd$Group,
                       levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"))

gnmds_3rd$Grouplab=factor(gnmds_3rd$Group,
                          levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"),
                          labels = c("UB","BJ","SS","SE","NT"))

gnmds_3rd$pdlab=factor(gnmds_3rd$pd,
                       levels = c("1st","3rd","3rd"),
                       labels = c("1st (2020-2021)","3rd (2021)","3rd (2023)"))

gnmds_3rd

ggplot()+
  geom_vline(xintercept = 0, lty="dashed")+
  geom_hline(yintercept = 0,lty="dashed")+
  geom_point(data = gnmds_3rd, aes(x=MDS1, y=MDS2, fill=Grouplab,shape=pdlab), col="black", size=7)+
  scale_fill_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  scale_shape_manual(values = c(21,22,23,24,25), guide=F)+
  scale_x_continuous(name="NMDS1")+
  scale_y_continuous(name="NMDS2")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 2),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.text.x = element_text(size = 22, colour = "black",face = "bold", family = "Arial"),
        axis.text.y = element_text(size = 22, colour = "black",face = "bold", family = "Arial"),
        axis.title.x = element_text(size = 22, colour = "black",face = "bold", family = "Arial",margin = unit(c(0.2,0.2,0.2,0.2),"cm")),
        axis.title.y = element_text(size = 22, colour = "black",face = "bold", family = "Arial"),
        legend.text = element_text(size = 22, colour = "black", family = "Arial", vjust = 0.6,margin = unit(c(0.2,0.2,0.2,0.2),"cm")),
        legend.spacing = unit(0.0,"cm"),
        legend.title = element_text(size = 26, colour = "black", family = "Arial",face = "bold"),
        legend.position = "right",
        legend.direction = "vertical",
        legend.box = "vertical",
        legend.background =element_rect(fill="white")
  )+
  guides(fill=guide_legend(title = "Group",order=1,title.hjust = 0.5,byrow = T , ncol = 1,
                           override.aes = list(size=7, shape=c(21,21,21,21,21))),
         shape=NULL
         #shape=guide_legend(title = "Campaign",order=2,title.hjust = 0.5,byrow = T , ncol = 1,
         #                    override.aes = list(size=7, shape=c(21,22,23), fill=c("black","black","black")))
  )+
  ggsave(filename("MDS_3rd"), height = 20, width = 23, units = "cm", dpi = 300, compression="lzw")

#2.vkplot======
frd_merge

fm_prop=unique(frd_merge[,c("Formula","O.C","H.C","AI","DBE","Molecularclass","AIClass","Comp")])

vk_frd=melt(frd_merge[,c("Group","pd","Formula","Bromo.Inty")], id.vars = c("Group","pd","Formula")) %>% 
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

frd_merge

frd_merge

cnt=aggregate(frd_merge$Freq, by=list(Group=frd_merge$Group, pd=frd_merge$pd, Formula=frd_merge$Formula),sum)
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
  ggsave(filename("vk_all"),height = 45, width = 80, units = "cm", dpi = 300, compression="lzw")


#chemical composition distribution======
frd_merge
frd_merge$Sample=paste(frd_merge$pd, frd_merge$Group, frd_merge$No, sep = "_")

frd_merge

ft_tot=aggregate(frd_merge$Bromo.Inty, by=list(Sample=frd_merge$Sample),sum) %>% `colnames<-`(c("Sample","Tot"))
comp_dis=aggregate(frd_merge$Bromo.Inty, by=list(Sample=frd_merge$Sample, Comp=frd_merge$Comp),sum)

comp_dis=comp_dis %>% left_join(ft_tot)
comp_dis$rel=comp_dis$x/comp_dis$Tot*100

comp_dis=comp_dis %>% separate(Sample, c("Sampling","Group","No"), sep = "_")
comp_dis$Sample=paste(comp_dis$Sampling, comp_dis$Group, comp_dis$No, sep = "_")
comp_dis$No=as.numeric(comp_dis$No)
head(comp_dis)


comp_dis$Grouplab=factor(comp_dis$Group,
                     levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"),
                     labels = c("UB","BJ","SS","SE","NT"))

comp_dis$pdlab=factor(comp_dis$Sampling,
                  levels = c("1st","2nd","3rd"),
                  labels = c("1st (Winter)","2nd (Summer)","3rd (Spring)"))

ggplot(comp_dis, aes(x=as.factor(No), y=rel, fill=Comp))+
  geom_bar(stat = "identity", position = position_fill(reverse = T))+
  scale_x_discrete(name="",expand = c(0.03,0.03))+
  scale_y_continuous(name = "", expand = c(0.01,0.01),labels = scales::percent)+
  facet_rep_grid(Grouplab~pdlab, repeat.tick.labels = T, scale="free")+
  scale_fill_manual(values = (c("#BC3C29FF","#EFC000FF","#008B45FF","#7876B1FF","grey50")),guide="none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 2),
        plot.margin = unit(c(0.2,0.4,0.2,0.4),"cm"),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.text.x = element_text(size = 19, colour = "black",face = "bold", family = "Arial",margin = unit(c(0.3,0,0.3,0),"cm")),
        axis.text.y = element_text(size = 19, colour = "black",face = "bold", family = "Arial",margin = unit(c(0.0,0.3,0.0,0.3),"cm")),
        axis.title.x = element_text(size = 22, colour = "black",face = "bold", family = "Arial"),
        axis.title.y = element_text(size = 22, colour = "black",face = "bold", family = "Arial"),
        strip.text.x = element_text(size = 30, colour = "black",face = "bold",family = "Arial",margin = unit(c(0.6,0,0.4,0),"cm")),
        strip.text.y = element_text(size = 30, colour = "black",face = "bold",family = "Arial",margin = unit(c(0.2,0.6,0.4,0.4),"cm")),
        strip.background = element_blank(),
        legend.text = element_text(size = 22, colour = "black", family = "Arial",hjust = 0.5,margin = unit(c(0,0,0,0),"cm")),
        legend.spacing = unit(0.0,"cm"),
        legend.title = element_text(size = 26,hjust = 0.2 ,vjust = 0.75,colour = "black", family = "Arial",face = "bold"),
        legend.position = "NULL",
        legend.direction = "vertical",
        #legend.position = "bottom",
        #legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.background = element_blank(),
  )+
  ggsave(filename("comp_distribution"),height = 75, width = 90, units = "cm", dpi = 300, compression="lzw")
  


comp_dis_avg=aggregate(comp_dis$rel,by=list(Grouplab=comp_dis$Grouplab, pdlab=comp_dis$pdlab, Comp=comp_dis$Comp),mean)
comp_dis_avg
comp_dis_avg$val=round(comp_dis_avg$x,1)

comp_dis_avg_sel=subset(comp_dis_avg,comp_dis_avg$Comp!="Remainders")
comp_dis_avg_sel

ggplot(data=comp_dis_avg,aes(x="",y=x/100, fill=Comp),)+
  geom_bar(stat="identity", size=0.5,
           position = position_fill(reverse = T), col="white")+
  geom_text(data=comp_dis_avg_sel,aes(x=1.05,label=sprintf("%0.1f", val)), position = position_stack(vjust=0.5,reverse = T), size=8)+
  coord_polar(theta = "y")+
  scale_fill_manual(values = (c("#BC3C29FF","#EFC000FF","#008B45FF","#7876B1FF","grey50")),guide="none")+
  #scale_fill_manual(values = rev(c("grey70","#B8CBB5","#9DB9D1","#F9D38A","#958CC5")))+
  #facet_wrap(variable+Season~Group, ncol = 4)+
  facet_rep_grid(Grouplab~pdlab)+
  theme_bw()+
  theme(
    panel.background = element_rect(fill = "transparent", color = NA, size = 0), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA, size=0), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    strip.text = element_text(size = 20, family = "Arial",face = "bold"),
    legend.title = element_text(size = 16, vjust = 0.75,colour = "black", family = "Arial",face = "bold"),
    legend.text = element_text(size = 12, colour = "black", family = "Arial",hjust = 0.5,margin = unit(c(0,0,0,0),"cm")),
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent", color = NA, size=0), # get rid of legend panel bg
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    panel.border = element_blank()
  )+
  guides(fill=guide_legend(reverse = T))+
  ggsave("Figure/230404pie_all_legend.png",height = 50, width = 80, units = "cm", dpi = 300)



#molecular distribution======
frd_merge
frd_merge$Sample=paste(frd_merge$pd, frd_merge$Group, frd_merge$No, sep = "_")

frd_merge

ft_tot=aggregate(frd_merge$Bromo.Inty, by=list(Sample=frd_merge$Sample),sum) %>% `colnames<-`(c("Sample","Tot"))
mole_dis=aggregate(frd_merge$Bromo.Inty, by=list(Sample=frd_merge$Sample, mole=frd_merge$AIClass),sum)

mole_dis=mole_dis %>% left_join(ft_tot)
mole_dis$rel=mole_dis$x/mole_dis$Tot*100

mole_dis=mole_dis %>% separate(Sample, c("Sampling","Group","No"), sep = "_")
mole_dis$Sample=paste(mole_dis$Sampling, mole_dis$Group, mole_dis$No, sep = "_")
mole_dis$No=as.numeric(mole_dis$No)
head(mole_dis)

mole_dis$Grouplab=factor(mole_dis$Group,
                         levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"),
                         labels = c("UB","BJ","SS","SE","NT"))

mole_dis$pdlab=factor(mole_dis$Sampling,
                      levels = c("1st","2nd","3rd"),
                      labels = c("1st (Winter)","2nd (Summer)","3rd (Spring)"))

ggplot(mole_dis, aes(x=as.factor(No), y=rel, fill=mole))+
  geom_bar(stat = "identity", position = position_fill(reverse = T))+
  scale_x_discrete(name="",expand = c(0.03,0.03))+
  scale_y_continuous(name = "", expand = c(0.01,0.01),labels = scales::percent)+
  scale_fill_manual(values = rev(c("grey70","#B8CBB5","#9DB9D1","#F9D38A","#958CC5")))+
  facet_rep_grid(Grouplab~pdlab, repeat.tick.labels = T, scale="free")+
  #scale_fill_manual(values = (c("#BC3C29FF","#EFC000FF","#008B45FF","#7876B1FF","grey50")),guide="none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 2),
        plot.margin = unit(c(0.2,0.4,0.2,0.4),"cm"),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.text.x = element_text(size = 19, colour = "black",face = "bold", family = "Arial",margin = unit(c(0.3,0,0.3,0),"cm")),
        axis.text.y = element_text(size = 19, colour = "black",face = "bold", family = "Arial",margin = unit(c(0.0,0.3,0.0,0.3),"cm")),
        axis.title.x = element_text(size = 22, colour = "black",face = "bold", family = "Arial"),
        axis.title.y = element_text(size = 22, colour = "black",face = "bold", family = "Arial"),
        strip.text.x = element_text(size = 30, colour = "black",face = "bold",family = "Arial",margin = unit(c(0.6,0,0.4,0),"cm")),
        strip.text.y = element_text(size = 30, colour = "black",face = "bold",family = "Arial",margin = unit(c(0.2,0.6,0.4,0.4),"cm")),
        strip.background = element_blank(),
        legend.text = element_text(size = 22, colour = "black", family = "Arial",hjust = 0.5,margin = unit(c(0,0,0,0),"cm")),
        legend.spacing = unit(0.0,"cm"),
        legend.title = element_text(size = 26,hjust = 0.2 ,vjust = 0.75,colour = "black", family = "Arial",face = "bold"),
        legend.position = "NULL",
        legend.direction = "vertical",
        #legend.position = "bottom",
        #legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.background = element_blank(),
  )+
  ggsave(filename("mole_distribution"),height = 75, width = 90, units = "cm", dpi = 300, compression="lzw")

####3
mole_dis_avg=aggregate(mole_dis$rel,by=list(Grouplab=mole_dis$Grouplab, pdlab=mole_dis$pdlab, mole=mole_dis$mole),mean)
mole_dis_avg
mole_dis_avg$val=round(mole_dis_avg$x,1)

mole_dis_avg_sel=subset(mole_dis_avg,mole_dis_avg$mole!="Remainders")
mole_dis_avg_sel

ggplot(data=mole_dis_avg,aes(x="",y=x/100, fill=mole),)+
  geom_bar(stat="identity", size=0.5,
           position = position_fill(reverse = T), col="white")+
  geom_text(data=mole_dis_avg_sel,aes(x=1.05,label=sprintf("%0.1f", val)), position = position_stack(vjust=0.5,reverse = T), size=8)+
  coord_polar(theta = "y")+
  scale_fill_manual(values = rev(c("grey70","#B8CBB5","#9DB9D1","#F9D38A","#958CC5")))+
  #facet_wrap(variable+Season~Group, ncol = 4)+
  facet_rep_grid(Grouplab~pdlab)+
  theme_bw()+
  theme(
    panel.background = element_rect(fill = "transparent", color = NA, size = 0), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA, size=0), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    strip.text = element_text(size = 20, family = "Arial",face = "bold"),
    legend.title = element_text(size = 16, vjust = 0.75,colour = "black", family = "Arial",face = "bold"),
    legend.text = element_text(size = 12, colour = "black", family = "Arial",hjust = 0.5,margin = unit(c(0,0,0,0),"cm")),
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent", color = NA, size=0), # get rid of legend panel bg
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    panel.border = element_blank()
  )+
  guides(fill=guide_legend(reverse = T))+
  ggsave("Figure/230405pie_all_legend.png",height = 50, width = 80, units = "cm", dpi = 300)




