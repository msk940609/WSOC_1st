library(openair)

cluster::pam()
mapproj::mapproject()
env=fread("Datafile/Envi_2019.csv")
env_bj=subset(env,env$Group=="Beijing")
#env_se=subset(env_2d,env_2d$group=="Seoul")
traj_bj_18 <- readRDS("Datafile/traj/TrajData_BJ_w2018.rds")
traj_bj_19 <- readRDS("Datafile/traj/TrajData_BJ_s2019.rds")

traj_bj=rbind(traj_bj_18,traj_bj_19)

##Beijing (winter)=====
traj_bjw_w=selectByDate(traj_bj,
             start = "2018-12-27",
             end="2019-01-25")
traj_bjw_w
traj_bjw_w$local=traj_bjw_w$date+3600*8
traj_bjw_w$Date=as.Date(traj_bjw_w$local-3600*10)

traj_bjw_w$Date=as.Date(traj_bjw_w$Date)
traj_bjw_w

env_bj$Date=as.Date(env_bj$Date)

traj_bjw_w2 <- inner_join(traj_bjw_w, env_bj[,-c(1:3)], by = "Date")
traj_bjw_w2

trajPlot(traj_bjw_w2,
         group = "Date",
         col = "turbo", lwd = 2)

traj_bjw_w2_n=traj_bjw_w2
traj_bjw_w2_n$d3=traj_bjw_w2_n$date-3600*10
traj_bjw_w2_n$dn=as.character(traj_bjw_w2_n$Date)
traj_bjw_w2_n$ev=as.character(traj_bjw_w2_n$Event)
traj_bjw_w2_n$ev=factor(traj_bjw_w2_n$ev, levels = c("Event","Normal","Non-event"))

mycol2=c("#BC3C29FF","#0072B5FF","#E18727FF","#EE4C97FF","royalblue4","#65463E","#704F70","#C03450","grey54")

trajMap(traj_bjw_w2_n, 
        colour  = "dn",
        cols = mycol2,
        control = "ev",
        provider = "Stamen.Terrain"
       # provider = "CartoDB.Positron"
)

mycol_c=c("#e41a1c","royalblue4","#4daf4a","#984ea3")


for (i in 2:5) {
  
  tiff(paste0("Figure/bjw_ctraj_n",i,"_angle.tiff"), width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
  bjw_clust <- trajCluster(traj_bjw_w2_n, 
                           #method = "Euclid",
                           method = "angle", 
                           n.cluster = i, 
                           col = c("#FF5D00","royalblue4","#EBA91A","#7868E6","#638C80"),
                           projection ="lambert",
                           orientation = c(0,45,0),
                           map.cols ="grey45",
                           xlab="Longitude (°)",
                           ylab="Latitude (°)",
                           xlim=c(95,130),
                           ylim=c(33,55)
  )
  dev.off()
  
  tiff(paste0("Figure/gww_ctraj_n",i,"_angle.tiff"), width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
  gww_clust <- trajCluster(traj_gww_w2_n, 
                           #method = "Euclid",
                           method = "angle", 
                           n.cluster = i, 
                           col = c("#FF5D00","royalblue4","#EBA91A","#7868E6","#638C80"),
                           projection ="lambert",
                           orientation = c(0,45,0),
                           map.cols ="grey45",
                           xlab="Longitude (°)",
                           ylab="Latitude (°)",
                           xlim=c(95,130),
                           ylim=c(33,55)
  )
  dev.off()
  
  tiff(paste0("Figure/bjs_ctraj_n",i,"_angle.tiff"), width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
  bjs_clust <- trajCluster(traj_bjs_w2_n, 
                           #method = "Euclid",
                           method = "angle", 
                           n.cluster = i, 
                           col = c("#FF5D00","royalblue4","#EBA91A","#7868E6","#638C80"),
                           projection ="lambert",
                           orientation = c(0,45,0),
                           map.cols ="grey45",
                           xlab="Longitude (°)",
                           ylab="Latitude (°)",
                           xlim=c(95,130),
                           ylim=c(33,55)
  )
  dev.off()
  
  tiff(paste0("Figure/gws_ctraj_n",i,"_angle.tiff"), width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
  gws_clust <- trajCluster(traj_gws_w2_n, 
                           #method = "Euclid",
                           method = "angle", 
                           n.cluster = i, 
                           col = c("#FF5D00","royalblue4","#EBA91A","#7868E6","#638C80"),
                           projection ="lambert",
                           orientation = c(0,45,0),
                           map.cols ="grey45",
                           xlab="Longitude (°)",
                           ylab="Latitude (°)",
                           xlim=c(103,138),
                           ylim=c(28,50)
  )
  dev.off()
  
}

bjw_clust <- trajCluster(traj_bjw_w2_n, 
                         #method = "euclid", 
                         method = "angle", 
                         n.cluster = 4, 
                         #col = c("#BC3C29FF","royalblue4","#E18727FF","#65463E"),
                         projection ="lambert",
                         orientation = c(0,45,0),
                         map.cols ="grey45",
                         xlab="Longitude",
                         ylab="Latitude",
                         xlim=c(95,130),
                         ylim=c(33,55)
)

##비율순서로 cluster 순서 변경
col_bw=c("royalblue3","#4daf4a","#e41a1c")
col_bw=c("royalblue4","#638C80","#FF5D00","#EBA91A","#7868E6")

col_bw=c("royalblue4","#638C80","#FF5D00")
tiff("Figure/bjw_ctraj_fin.tiff", width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
bjw_clust <- trajCluster(traj_bjw_w2_n, 
                         method = "angle", 
                         n.cluster = 3, 
                         #col = c("#e41a1c","royalblue4","#4daf4a"),
                         col = col_bw,
                         projection ="albers",
                         orientation = c(0,45,0),
                         map.cols ="grey45",
                         xlab="Longitude (°)",
                         ylab="Latitude (°)",
                         xlim=c(90,130),
                         ylim=c(33,55),
                         grid.col="black",
                         lwd = 6,
                         key=F
)
dev.off()


tiff("Figure/bjw_ctraj_nolabel.tiff", width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
bjw_clust_nolabel <- trajCluster_ms(traj_bjw_w2_n, 
                                    method = "angle", 
                                    n.cluster = 3, 
                                    #col = c("#e41a1c","royalblue4","#4daf4a"),
                                    col = col_bw,
                                    projection ="albers",
                                    orientation = c(0,45,0),
                                    map.cols ="grey45",
                                    xlab="Longitude (°)",
                                    ylab="Latitude (°)",
                                    xlim=c(90,130),
                                    ylim=c(33,55),
                                    grid.col="black",
                                    #grid.col="transparent,
                                    lwd = 6,
                                    key=F
)
dev.off()

traj_bjw_c=filter(bjw_clust$data$traj, hour.inc == 0)
is.Date(traj_bjw_c$Date)
is.Date(traj_bjw_c$date)
traj_bjw_c$Freq=1
traj_bjw_c$newd=as.factor(format(traj_bjw_c$Date, "%b-%d"))
traj_bjw_c$Event

tbw=unique(traj_bjw_c[,c("Date","Event")])

tbw$col <- ifelse(tbw$Event == "Event", "red",
            ifelse(tbw$Event == "Normal", "black","blue"))

vtbw=as.vector(rev(tbw$col))

data_dend_bjw=traj_bjw_c[,c("Freq","cluster","newd")]
data_dend_bjw2=dcast(data_dend_bjw,newd~cluster,sum, value.var = "Freq")
data_dend_bjw2=data_dend_bjw2 %>% `row.names<-`(data_dend_bjw2$newd)
dend_bjw <- as.dendrogram(hclust(dist(data_dend_bjw2)))

traj_bjw_c$n2 <- factor(traj_bjw_c$newd, levels = labels(dend_bjw))
tbw=unique(traj_bjw_c[,c("n2","Event")])
tbw$col <- ifelse(tbw$Event == "Event", "red",
                 ifelse(tbw$Event == "Normal", "black","blue"))

nv=data.frame(n2=labels(dend_bjw))
nv$n=row.names(nv)
nv=nv %>% inner_join(tbw)
nv
cs=as.vector((nv$col))

p1 <- ggplot(dend_bjw, horiz = T)+
  scale_x_continuous(expand = c(0.015, 0.015))
p2 <- ggplot(traj_bjw_c, aes(x=n2, y=Freq, fill=cluster, colour=cluster))+
  geom_bar(stat = "identity", position = position_fill())+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  #scale_x_discrete(limits = rev(levels(traj_bjw_c$n2)))+
  scale_fill_manual(values =col_bw)+
  scale_color_manual(values =col_bw)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=14, face="bold", color = "black"),
        #axis.text.y = element_text(size=14,face="bold",color = "black"),
        axis.text.y = element_text(size=14,face="bold",color = cs),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = "right"
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)

plot_grid(p1, p2, align = "h")+
  ggsave(filename("bjw_trajc_cluster_leg"),height = 30, width = 40, units = "cm", dpi = 300, compression="lzw")

dend_bjw <- data_dend_bjw2 %>% dist %>% hclust %>% as.dendrogram %>% set("labels_cex", c(0))

p1 <- ggplot(dend_bjw, horiz = T)+
  scale_x_continuous(expand = c(0.019, 0.019))+
  theme(plot.margin = unit(c(0.2,-3.2,0.2,0.2),"cm"))

p2 <- ggplot(traj_bjw_c, aes(x=n2, y=Freq, fill=cluster),colour=NA,alpha=0.7)+
  geom_bar(stat = "identity", position = position_fill(reverse = T), alpha=0.8)+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  #scale_x_discrete(limits = rev(levels(traj_bjw_c$n2)))+
  scale_fill_manual(values = col_bw)+
  scale_color_manual(values =col_bw)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=14, face="bold", color = "black"),
        #axis.text.y = element_text(size=14,face="bold",color = "black"),
        axis.text.y = element_text(size=14,face="bold",color = cs),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = "NULL"
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)

plot_grid(p1, p2, align = "h")+
  ggsave(filename("bjw_trajc_cluster_fin"),height = 30, width = 40, units = "cm", dpi = 300, compression="lzw")


##Gwangju (Winter)=====
env_gw=subset(env,env$Group=="Gwangju")

traj_gw_18 <- readRDS("Datafile/traj/TrajData_GW_w2018.rds")
traj_gw_19 <- readRDS("Datafile/traj/TrajData_GW_s2019.rds")

traj_gw=rbind(traj_gw_18,traj_gw_19)

traj_gww_w=selectByDate(traj_gw,
                       start = "2018-12-27",
                       end="2019-01-25")

traj_gww_w

traj_gww_w
traj_gww_w$local=traj_gww_w$date+3600*8
traj_gww_w$Date=as.Date(traj_gww_w$local-3600*10)

traj_gww_w$Date=as.Date(traj_gww_w$Date)
traj_gww_w

env_gww$Date=as.Date(env_gww$Date)

traj_gww_w2 <- inner_join(traj_gww_w, env_gw[,-c(1:3)], by = "Date")
traj_gww_w2

trajPlot(traj_gww_w2,
         group = "Date",
         col = "turbo", lwd = 2)

traj_gww_w2_n=traj_gww_w2
traj_gww_w2_n$d3=traj_gww_w2_n$date-3600*10
traj_gww_w2_n$dn=as.character(traj_gww_w2_n$Date)
traj_gww_w2_n$ev=as.character(traj_gww_w2_n$Event)
traj_gww_w2_n$ev=factor(traj_gww_w2_n$ev, levels = c("Event","Normal","Non-event"))

mycol2=c("#BC3C29FF","#0072B5FF","#E18727FF","#EE4C97FF","royalblue4","#65463E","#704F70","#C03450","grey54")

trajMap(traj_gww_w2_n, 
        colour  = "dn",
        cols = mycol2,
        control = "ev",
        provider = "Stamen.Terrain"
        # provider = "CartoDB.Positron"
)

mycol_c=c("#FF5D00","royalblue4","#EBA91A","#7868E6","#638C80")


col_gw=c("royalblue4","#FF5D00","#638C80")
tiff("Figure/gww_ctraj_fin.tiff", width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
gww_clust <- trajCluster(traj_gww_w2_n, 
                         method = "angle", 
                         n.cluster = 3, 
                         #col = c("#e41a1c","royalblue4","#4daf4a"),
                         col = col_gw,
                         projection ="albers",
                         orientation = c(0,45,0),
                         map.cols ="grey45",
                         xlab="Longitude (°)",
                         ylab="Latitude (°)",
                         xlim=c(100,138),
                         ylim=c(30,52),
                         grid.col="black",
                         lwd = 6,
                         key=F
)
dev.off()

tiff("Figure/gww_ctraj_nolabel.tiff", width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
gww_clust_nolabel <- trajCluster_ms(traj_gww_w2_n, 
                                    method = "angle", 
                                    n.cluster = 3, 
                                    #col = c("#e41a1c","royalblue4","#4daf4a"),
                                    col = col_gw,
                                    projection ="albers",
                                    orientation = c(0,45,0),
                                    map.cols ="grey45",
                                    xlab="Longitude (°)",
                                    ylab="Latitude (°)",
                                    xlim=c(100,138),
                                    ylim=c(30,52),
                                    grid.col="black",
                                    #grid.col="transparent,
                                    lwd = 6,
                                    key=F
)
dev.off()

traj_gww_c=filter(gww_clust$data$traj, hour.inc == 0)
traj_gww_c$d3
is.Date(traj_gww_c$Date)
traj_gww_c$Freq=1
traj_gww_c$newd=as.factor(format(traj_gww_c$Date, "%b-%d"))
traj_gww_c$Event

tgw=unique(traj_gww_c[,c("Date","Event")])

tgw$col <- ifelse(tgw$Event == "Event", "red",
                 ifelse(tgw$Event == "Normal", "black","blue"))

vgw=as.vector(rev(tgw$col))

ggplot(traj_gww_c, aes(x=newd, y=Freq, fill=cluster, colour=cluster))+
  geom_bar(stat = "identity", position = position_fill())+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  scale_x_discrete(limits = rev(levels(traj_gww_c$newd)))+
  scale_fill_manual(values =col_gw)+
  scale_color_manual(values =col_gw)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=16, face="bold", color = "black", margin = unit(c(0.6,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=14, face="bold", color = "black"),
        #axis.text.x = element_text(size=14, face="bold", color = a),
        #axis.text.y = element_text(size=14,face="bold",color = "black"),
        axis.text.y = element_text(size=14,face="bold",color = vgw),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = "top"
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)+
  ggsave(filename("gww_traj_cluster"),height = 30, width = 20, units = "cm", dpi = 300, compression="lzw")

data_dend_gww=traj_gww_c[,c("Freq","cluster","newd")]
data_dend_gww2=dcast(data_dend_gww,newd~cluster,sum, value.var = "Freq")
data_dend_gww2=data_dend_gww2 %>% `row.names<-`(data_dend_gww2$newd)

dend_gww <- as.dendrogram(hclust(dist(data_dend_gww2)))
traj_gww_c$n2 <- factor(traj_gww_c$newd, levels = labels(dend_gww))

tgw=unique(traj_gww_c[,c("n2","Event")])
tgw$col <- ifelse(tgw$Event == "Event", "red",
                 ifelse(tgw$Event == "Normal", "black","blue"))
nv=data.frame(n2=labels(dend_gww))
nv$n=row.names(nv)
nv=nv %>% inner_join(tgw)
nv
vgw=as.vector((nv$col))

p1 <- ggplot(dend_gww, horiz = T)+
  scale_x_continuous(expand = c(0.015, 0.015))
p2 <- ggplot(traj_gww_c, aes(x=n2, y=Freq, fill=cluster), colour=NA, alpha=0.7)+
  geom_bar(stat = "identity", position = position_fill())+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  #scale_x_discrete(limits = rev(levels(traj_gww_c$n2)))+
  scale_fill_manual(values = col_gw)+
  scale_color_manual(values =col_gw)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=14, face="bold", color = "black"),
        #axis.text.y = element_text(size=14,face="bold",color = "black"),
        axis.text.y = element_text(size=14,face="bold",color = vgw),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = "right"
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)

plot_grid(p1, p2, align = "h")+
  ggsave(filename("gww_trajc_cluster_leg"),height = 30, width = 40, units = "cm", dpi = 300, compression="lzw")


dend_gww <- data_dend_gww2 %>% dist %>% hclust %>% as.dendrogram %>% set("labels_cex", c(0))

p1 <- ggplot(dend_gww, horiz = T)+
  scale_x_continuous(expand = c(0.015, 0.015))+
  theme(plot.margin = unit(c(0.2,-3.2,0.2,0.2),"cm"))

p2 <- ggplot(traj_gww_c, aes(x=n2, y=Freq, fill=cluster),colour=NA, alpha=0.7)+
  geom_bar(stat = "identity", position = position_fill(),alpha=0.8)+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  #scale_x_discrete(limits = rev(levels(traj_gww_c$n2)))+
  scale_fill_manual(values =col_gw)+
  scale_color_manual(values =col_gw)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=14, face="bold", color = "black"),
        #axis.text.y = element_text(size=14,face="bold",color = "black"),
        axis.text.y = element_text(size=14,face="bold",color = vgw),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = "NULL"
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)

plot_grid(p1, p2, align = "h")+
  ggsave(filename("gww_trajc_cluster_fin"),height = 30, width = 40, units = "cm", dpi = 300, compression="lzw")


##Beijing (Summer)=====
traj_bjs_w=selectByDate(traj_bj,
                        start = "2019-08-05",
                        end="2019-08-23")
traj_bjs_w
traj_bjs_w$local=traj_bjs_w$date+3600*8
traj_bjs_w$Date=as.Date(traj_bjs_w$local-3600*10)

traj_bjs_w$Date=as.Date(traj_bjs_w$Date)
traj_bjs_w

env_bj$Date=as.Date(env_bj$Date)

traj_bjs_w2 <- inner_join(traj_bjs_w, env_bj[,-c(1:3)], by = "Date")
traj_bjs_w2

trajPlot(traj_bjs_w2,
         group = "Date",
         col = "turbo", lwd = 2)

traj_bjs_w2_n=traj_bjs_w2
traj_bjs_w2_n$d3=traj_bjs_w2_n$date-3600*10
traj_bjs_w2_n$dn=as.character(traj_bjs_w2_n$Date)
traj_bjs_w2_n$ev=as.character(traj_bjs_w2_n$Event)
traj_bjs_w2_n$ev=factor(traj_bjs_w2_n$ev, levels = c("Event","Normal","Non-event"))

mycol2=c("#BC3C29FF","#0072B5FF","#E18727FF","#EE4C97FF","royalblue4","#65463E","#704F70","#C03450","grey54")

trajMap(traj_bjs_w2_n, 
        colour  = "dn",
        cols = mycol2,
        control = "ev",
        provider = "Stamen.Terrain"
        # provider = "CartoDB.Positron"
)

col_bs=c("royalblue4","#638C80","#FF5D00")
tiff("Figure/bjs_ctraj_fin.tiff", width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
bjs_clust <- trajCluster(traj_bjs_w2_n, 
                         method = "angle", 
                         n.cluster = 3, 
                         #col = c("#e41a1c","royalblue4","#4daf4a"),
                         col = col_bs,
                         projection ="albers",
                         orientation = c(0,45,0),
                         map.cols ="grey45",
                         xlab="Longitude (°)",
                         ylab="Latitude (°)",
                         xlim=c(90,130),
                         ylim=c(33,55),
                         grid.col="black",
                         lwd = 6,
                         key=F
)
dev.off()

tiff("Figure/bjs_ctraj_nolabel.tiff", width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
bjs_clust_nolabel <- trajCluster_ms(traj_bjs_w2_n, 
                                    method = "angle", 
                                    n.cluster = 3, 
                                    #col = c("#e41a1c","royalblue4","#4daf4a"),
                                    col = col_bs,
                                    projection ="albers",
                                    orientation = c(0,45,0),
                                    map.cols ="grey45",
                                    xlab="Longitude (°)",
                                    ylab="Latitude (°)",
                                    xlim=c(90,130),
                                    ylim=c(33,55),
                                    grid.col="black",
                                    #grid.col="transparent,
                                    lwd = 6,
                                    key=F
)
dev.off()

traj_bjs_c=filter(bjs_clust$data$traj, hour.inc == 0)
traj_bjs_c$d3
is.Date(traj_bjs_c$Date)
is.Date(traj_bjs_c$date)
traj_bjs_c$Freq=1
traj_bjs_c$newd=as.factor(format(traj_bjs_c$Date, "%b-%d"))
traj_bjs_c$Event

tbs=unique(traj_bjs_c[,c("Date","Event")])

tbs$col <- ifelse(tbs$Event == "Event", "red",
                 ifelse(tbs$Event == "Normal", "black","blue"))

vbs=as.vector(rev(tbs$col))

ggplot(traj_bjs_c, aes(x=newd, y=Freq, fill=cluster), colour=NA, alpha=0.7)+
  geom_bar(stat = "identity", position = position_fill())+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  scale_x_discrete(limits = rev(levels(traj_bjs_c$newd)))+
  scale_fill_manual(values =c("#FF5D00","royalblue4","#EBA91A","#7868E6","#638C80"))+
  scale_color_manual(values =c("#FF5D00","royalblue4","#EBA91A","#7868E6","#638C80"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=16, face="bold", color = "black", margin = unit(c(0.6,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=14, face="bold", color = "black"),
        #axis.text.x = element_text(size=14, face="bold", color = a),
        #axis.text.y = element_text(size=14,face="bold",color = "black"),
        axis.text.y = element_text(size=14,face="bold",color = ta),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = "top"
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)+
  ggsave(filename("bjs_traj_cluster"),height = 30, width = 20, units = "cm", dpi = 300, compression="lzw")


data_dend_bjs=traj_bjs_c[,c("Freq","cluster","newd")]
data_dend_bjs2=dcast(data_dend_bjs,newd~cluster,sum, value.var = "Freq")
data_dend_bjs2=data_dend_bjs2 %>% `row.names<-`(data_dend_bjs2$newd)

dend_bjs <- as.dendrogram(hclust(dist(data_dend_bjs2)))

traj_bjs_c$n2 <- factor(traj_bjs_c$newd, levels = labels(dend_bjs))
tbs=unique(traj_bjs_c[,c("n2","Event")])
tbs$col <- ifelse(tbs$Event == "Event", "red",
                 ifelse(tbs$Event == "Normal", "black","blue"))
nv=data.frame(n2=labels(dend_bjs))
nv$n=row.names(nv)
nv=nv %>% inner_join(tbs)
nv
cbs=as.vector((nv$col))

p1 <- ggplot(dend_bjs, horiz = T)+
  scale_x_continuous(expand = c(0.015, 0.015))

p2 <- ggplot(traj_bjs_c, aes(x=n2, y=Freq, fill=cluster), colour=NA, alpha=0.7)+
  geom_bar(stat = "identity", position = position_fill())+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  #scale_x_discrete(limits = rev(levels(traj_bjs_c$n2)))+
  scale_fill_manual(values =col_bs)+
  scale_color_manual(values =col_bs)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=14, face="bold", color = "black"),
        #axis.text.y = element_text(size=14,face="bold",color = "black"),
        axis.text.y = element_text(size=14,face="bold",color = cbs),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = "right"
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)

plot_grid(p1, p2, align = "h")+
  ggsave(filename("bjs_trajc_cluster_leg"),height = 30, width = 40, units = "cm", dpi = 300, compression="lzw")

dend_bjs <- data_dend_bjs2 %>% dist %>% hclust %>% as.dendrogram %>% set("labels_cex", c(0))

p1 <- ggplot(dend_bjs, horiz = T)+
  scale_x_continuous(expand = c(0.035, 0.035))+
  theme(plot.margin = unit(c(0.2,-3.2,0.2,0.2),"cm"))

p2 <- ggplot(traj_bjs_c, aes(x=n2, y=Freq, fill=cluster),colour=NA, alpha=0.7)+
  geom_bar(stat = "identity", position = position_fill(),alpha=0.8)+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  #scale_x_discrete(limits = rev(levels(traj_bjs_c$n2)))+
  scale_fill_manual(values =col_bs)+
  scale_color_manual(values =col_bs)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=14, face="bold", color = "black"),
        #axis.text.y = element_text(size=14,face="bold",color = "black"),
        axis.text.y = element_text(size=14,face="bold",color = cbs),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = "NULL"
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)

plot_grid(p1, p2, align = "h")+
  ggsave(filename("bjs_trajc_cluster_fin2"),height = 30, width = 40, units = "cm", dpi = 300, compression="lzw")

##Gwangju (Summer)=====
env_gw=subset(env,env$Group=="Gwangju")

traj_gw_18 <- readRDS("Datafile/traj/TrajData_GW_w2018.rds")
traj_gw_19 <- readRDS("Datafile/traj/TrajData_GW_s2019.rds")

traj_gw=rbind(traj_gw_18,traj_gw_19)

traj_gws_w=selectByDate(traj_gw,
                        start = "2019-08-05",
                        end="2019-08-23")

traj_gws_w

traj_gws_w
traj_gws_w$local=traj_gws_w$date+3600*8
traj_gws_w$Date=as.Date(traj_gws_w$local-3600*10)

traj_gws_w$Date=as.Date(traj_gws_w$Date)
traj_gws_w

env_gw$Date=as.Date(env_gw$Date)

traj_gws_w2 <- inner_join(traj_gws_w, env_gw[,-c(1:3)], by = "Date")
traj_gws_w2

trajPlot(traj_gws_w2,
         group = "Date",
         col = "turbo", lwd = 2)

traj_gws_w2_n=traj_gws_w2
traj_gws_w2_n$d3=traj_gws_w2_n$date-3600*10
traj_gws_w2_n$dn=as.character(traj_gws_w2_n$Date)
traj_gws_w2_n$ev=as.character(traj_gws_w2_n$Event)
traj_gws_w2_n$ev=factor(traj_gws_w2_n$ev, levels = c("Event","Normal","Non-event"))

mycol2=c("#BC3C29FF","#0072B5FF","#E18727FF","#EE4C97FF","royalblue4","#65463E","#704F70","#C03450","grey54")

trajMap(traj_gws_w2_n, 
        colour  = "dn",
        cols = mycol2,
        control = "ev",
        provider = "Stamen.Terrain"
        # provider = "CartoDB.Positron"
)

mycol_c=c("#BC3C29FF","royalblue4","#E18727FF","#65463E","#704F70")


col_gs=c("#FF5D00","royalblue4","#638C80","#7868E6")
tiff("Figure/gws_ctraj_fin.tiff", width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
gws_clust <- trajCluster(traj_gws_w2_n, 
                         method = "angle", 
                         n.cluster = 4, 
                         #col = c("#e41a1c","royalblue4","#4daf4a"),
                         col = col_gs,
                         projection ="albers",
                         orientation = c(0,45,0),
                         map.cols ="grey45",
                         xlab="Longitude (°)",
                         ylab="Latitude (°)",
                         xlim=c(100,138),
                         ylim=c(30,52),
                         grid.col="black",
                         lwd = 6,
                         key=F
)
dev.off()

tiff("Figure/gws_ctraj_nolabel.tiff", width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
gws_clust_nolabel <- trajCluster_ms(traj_gws_w2_n, 
                                    method = "angle", 
                                    n.cluster = 4, 
                                    #col = c("#e41a1c","royalblue4","#4daf4a"),
                                    col = col_gs,
                                    projection ="albers",
                                    orientation = c(0,45,0),
                                    map.cols ="grey45",
                                    xlab="Longitude (°)",
                                    ylab="Latitude (°)",
                                    xlim=c(100,138),
                                    ylim=c(30,52),
                                    grid.col="black",
                                    #grid.col="transparent,
                                    lwd = 6,
                                    key=F
)
dev.off()

traj_gws_c=filter(gws_clust$data$traj, hour.inc == 0)
traj_gws_c$d3
is.Date(traj_gws_c$Date)
is.Date(traj_gws_c$date)
traj_gws_c$Freq=1
traj_gws_c$newd=as.factor(format(traj_gws_c$Date, "%b-%d"))
traj_gws_c$Event

tt=unique(traj_gws_c[,c("Date","Event")])

tt$col <- ifelse(tt$Event == "Event", "red",
                 ifelse(tt$Event == "Normal", "black","blue"))

ta=as.vector(rev(tt$col))

traj_gws_c$cluster=factor(traj_gws_c$cluster, levels = c("C1","C2","C3","C4"))

ggplot(traj_gws_c, aes(x=newd, y=Freq, fill=cluster, colour=cluster))+
  geom_bar(stat = "identity", position = position_fill())+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  scale_x_discrete(limits = rev(levels(traj_gws_c$newd)))+
  scale_fill_manual(values =col_gs)+
  scale_color_manual(values =col_gs)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=16, face="bold", color = "black", margin = unit(c(0.6,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=14, face="bold", color = "black"),
        #axis.text.x = element_text(size=14, face="bold", color = a),
        #axis.text.y = element_text(size=14,face="bold",color = "black"),
        axis.text.y = element_text(size=14,face="bold",color = ta),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = "top"
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)+
  ggsave(filename("gws_traj_cluster"),height = 30, width = 20, units = "cm", dpi = 300, compression="lzw")

data_dend_gws=traj_gws_c[,c("Freq","cluster","newd")]
data_dend_gws2=dcast(data_dend_gws,newd~cluster,sum, value.var = "Freq")
data_dend_gws2=data_dend_gws2 %>% `row.names<-`(data_dend_gws2$newd)

dend_gws <- as.dendrogram(hclust(dist(data_dend_gws2)))

traj_gws_c$n2 <- factor(traj_gws_c$newd, levels = labels(dend_gws))
ts=unique(traj_gws_c[,c("n2","Event")])
ts$col <- ifelse(tt$Event == "Event", "red",
                 ifelse(tt$Event == "Normal", "black","blue"))

nv=data.frame(n2=labels(dend_gws))
nv$n=row.names(nv)
nv=nv %>% inner_join(ts)
nv
cs=as.vector((nv$col))

p1 <- ggplot(dend_gws, horiz = T)+
  scale_x_continuous(expand = c(0.015, 0.015))
p2 <- ggplot(traj_gws_c, aes(x=n2, y=Freq, fill=cluster, colour=cluster))+
  geom_bar(stat = "identity", position = position_fill())+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  #scale_x_discrete(limits = rev(levels(traj_gws_c$n2)))+
  scale_fill_manual(values =col_gs)+
  scale_color_manual(values =col_gs)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=14, face="bold", color = "black"),
        #axis.text.y = element_text(size=14,face="bold",color = "black"),
        axis.text.y = element_text(size=14,face="bold",color = cs),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = "right"
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)

plot_grid(p1, p2, align = "h")+
  ggsave(filename("gws_trajc_cluster_leg"),height = 30, width = 40, units = "cm", dpi = 300, compression="lzw")

dend_gws <- data_dend_gws2 %>% dist %>% hclust %>% as.dendrogram %>% set("labels_cex", c(0))

p1 <- ggplot(dend_gws, horiz = T)+
  scale_x_continuous(expand = c(0.035, 0.035))+
  theme(plot.margin = unit(c(0.2,-3.2,0.2,0.2),"cm"))
p2 <- ggplot(traj_gws_c, aes(x=n2, y=Freq, fill=cluster),colour=NA, alpha=0.7)+
  geom_bar(stat = "identity", position = position_fill(reverse = T),alpha=0.8)+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  #scale_x_discrete(limits = rev(levels(traj_gws_c$n2)))+
  scale_fill_manual(values =col_gs)+
  scale_color_manual(values =col_gs)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=14, face="bold", color = "black"),
        #axis.text.y = element_text(size=14,face="bold",color = "black"),
        axis.text.y = element_text(size=14,face="bold",color = cs),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = "NULL"
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)

plot_grid(p1, p2, align = "h")+
  ggsave(filename("gws_trajc_cluster_fin2"),height = 30, width = 40, units = "cm", dpi = 300, compression="lzw")




cl_dis=fread("Datafile/Cluster_distribution.csv")

cl_dis_m=melt(cl_dis, id.vars = c("Group","Season"))
cl_dis_m$Group=factor(cl_dis_m$Group, levels = c("Beijing","Gwangju"))
cl_dis_m$Season=factor(cl_dis_m$Season, levels = c("Winter","Summer"))

c("#FF5D00","royalblue4","#EBA91A","#7868E6","#638C80")
p2 <- ggplot(traj_gws_c, aes(x=n2, y=Freq, fill=cluster),colour=NA, alpha=0.7)+
  geom_bar(stat = "identity", position = position_fill(),alpha=0.8)+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  #scale_x_discrete(limits = rev(levels(traj_gws_c$n2)))+
  scale_fill_manual(values =c("#FF5D00","#7868E6","royalblue4","#EBA91A","#638C80"))+
  scale_color_manual(values =c("#FF5D00","#7868E6","royalblue4","#EBA91A","#638C80"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=14, face="bold", color = "black"),
        axis.text.y = element_text(size=14,face="bold",color = "black"),
        #axis.text.y = element_text(size=14,face="bold",color = cs),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = "NULL"
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)

ggplot(cl_dis_m, aes(x=Group, y=value/100, fill=variable))+
  geom_bar(stat = "identity", position = position_fill(reverse = T),alpha=0.8, width = 0.75)+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  scale_fill_manual(values =c("#FF5D00","royalblue4","#EBA91A","#7868E6","#638C80"))+
  scale_color_manual(values =c("#FF5D00","royalblue4","#EBA91A","#7868E6","#638C80"))+
  facet_rep_wrap(.~Season, repeat.tick.labels = T, ncol=2)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black",family = "Arial", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size=14, face="bold", color = "black",family = "Arial"),
        axis.text.y = element_text(size=14,face="bold",color = "black",family = "Arial"),
        #axis.text.y = element_text(size=14,face="bold",color = cs),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text = element_text(size=18,face=2, color = "black",family = "Arial"),
        legend.title = element_text(size=16,face="bold",color = "black",family = "Arial"),
        legend.text = element_text(size=14,face="bold",color = "black",family = "Arial"),
        legend.position = "NULL"
  )+
  guides(fill=guide_legend(title = "Cluster"), col=F)+
  ggsave(filename("Cluster_proportion"),height = 15, width = 20, units = "cm", dpi = 300, compression="lzw")


env=fread("Datafile/Envi_2019.csv")
env

cl_env=env

cl_env$`SOC`=cl_env$WSOCnbb
cl_env$`POC`=cl_env$OC-cl_env$SOC

cl_env$SOCp=cl_env$WSOCnbb/cl_env$OC*100
cl_env$POCp=100-cl_env$SOCp

cl_env_sel=cl_env[,c("Group","Season","Cluster","Event","PM2.5","T","RH","WS","POC","SOC","NH4+","NO3-","SO42-",
                     "O3","NO2","SO2")]
cl_env_sel_m=melt(cl_env_sel,id.vars = c("Group","Season","Cluster","Event"),na.rm = F)

cl_env_sel_m$varlab=factor(cl_env_sel_m$variable, levels = c("WS","T","RH","PM2.5","POC","SOC","NH4+","NO3-","SO42-",
                                                             "O3","NO2","SO2"),
                           labels = c(expression(bold("Wind speed (m/s)")),expression(bold("Temperature (℃)")),expression(bold("%RH")),
                                      expression(bold("PM"["2.5"]~"("*"\u03bcg/"*m^"3"*")")),expression(bold("POC"~"("*"\u03bcg/"*m^"3"*")")),
                                      expression(bold("SOC"~"("*"\u03bcg/"*m^"3"*")")),
                                      expression(bold("NH"["4"]^"+"~"("*"\u03bcg/"*m^"3"*")")),
                                      expression(bold("NO"["3"]^"-"~"("*"\u03bcg/"*m^"3"*")")),expression(bold("SO"["4"]^"2-"~"("*"\u03bcg/"*m^"3"*")")),
                                      expression(bold("O"["3"]~"(ppb)")),expression(bold("NO"["2"]~"(ppb)")),expression(bold("SO"["2"]~"(ppb)")))
                           )

cl_env_sel_m_w=subset(cl_env_sel_m,cl_env_sel_m$Season=="Winter")


shading_b <- data.frame(min = 1.133, max=1.264, Group="Beijing")
shading_g <- data.frame(min = 1.133+1, max=1.264+1, Group="Gwangju")

shading=rbind(shading_g,shading_b)
c("royalblue4","#FF5D00","#638C80","#7868E6")


cl_env_sel_m_w$val2=ifelse(cl_env_sel_m_w$Group=="Gwangju",
                           ifelse(cl_env_sel_m_w$variable=="WS",cl_env_sel_m_w$value*1,cl_env_sel_m_w$value),cl_env_sel_m_w$value)

ggplot()+
  stat_boxplot(data=cl_env_sel_m_w, aes(x=Group,y=val2, fill=Cluster),geom="errorbar",position = position_dodge(preserve = "total", width = 0.8)
               ,width=0.4)+
  geom_boxplot(data=cl_env_sel_m_w, aes(x=Group,y=val2, fill=Cluster), position = position_dodge(preserve = "total", width = 0.8),
               outlier.color = NA)+
#  geom_rect(data=shading, aes(xmin=min, xmax=max, ymin=-Inf, ymax=Inf), fill="white")+
  facet_rep_wrap(varlab~., scales = "free_y",ncol=3, repeat.tick.labels = T, labeller=label_parsed,strip.position="left")+
  scale_fill_manual(values =c("royalblue4","#FF5D00","#638C80","grey50"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black",family = "Arial", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size=20, face="bold", color = "black",family = "Arial"),
        axis.text.y = element_text(size=20,face="bold",color = "black",family = "Arial"),
        strip.placement = "outside",
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text = element_text(size=22,face=2, color = "black",family = "Arial"),
        legend.key.height = unit(1.8, "cm"),
        legend.key.width = unit(1.8, "cm"),
        legend.title = element_text(size=22,face="bold",color = "black",family = "Arial"),
        legend.text = element_text(size=20,face="bold",color = "black",family = "Arial"),
        legend.position = "bottom"
  )+
  guides(fill=guide_legend(title = "Cluster",ncol=6))+
  ggsave(filename("envi_bycluster_winter"),height = 45, width = 50, units = "cm", dpi = 300, compression="lzw")


cl_env_sel_m_s=subset(cl_env_sel_m,cl_env_sel_m$Season=="Summer")
cl_env_sel_m_s$Cluster=factor(cl_env_sel_m_s$Cluster, levels = c("C1","C2","C3","C4","Mixed"))

ggplot(data=cl_env_sel_m_s, aes(x=Group,y=value, fill=Cluster))+
  stat_boxplot(data=cl_env_sel_m_s, aes(x=Group,y=value, fill=Cluster),geom="errorbar",position = position_dodge(preserve = "single", width = 0.8)
               ,width=0.4)+
  geom_boxplot(data=cl_env_sel_m_s, aes(x=Group,y=value, fill=Cluster), position = position_dodge(preserve = "single", width = 0.8),
               outlier.color = NA)+
  #geom_rect(data=shading, aes(xmin=min, xmax=max, ymin=-Inf, ymax=Inf), fill="white")+
  facet_rep_wrap(varlab~., scales = "free_y",ncol=3, repeat.tick.labels = T, labeller=label_parsed,strip.position="left")+
  scale_fill_manual(values =c("royalblue4","#FF5D00","#638C80","#7868E6","grey50"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black",family = "Arial", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size=20, face="bold", color = "black",family = "Arial"),
        axis.text.y = element_text(size=20,face="bold",color = "black",family = "Arial"),
        strip.placement = "outside",
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text = element_text(size=22,face=2, color = "black",family = "Arial"),
        legend.key.height = unit(1.8, "cm"),
        legend.key.width = unit(1.8, "cm"),
        legend.title = element_text(size=22,face="bold",color = "black",family = "Arial"),
        legend.text = element_text(size=20,face="bold",color = "black",family = "Arial"),
        legend.position = "bottom"
  )+
  guides(fill=guide_legend(title = "Cluster",ncol=6))+
  ggsave(filename("envi_bycluster_summer"),height = 45, width = 50, units = "cm", dpi = 300, compression="lzw")

