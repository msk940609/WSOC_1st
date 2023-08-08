
envi_traj=envi_1st

envi_traj$Grouplab=factor(envi_traj$Group, levels = c("Ulan","Beijing","Seosan","Seoul","Noto"),
                                          labels = c("UT","BJ","SS","SE","NT"))

#env_se=subset(env_2d,env_2d$group=="Seoul")
envi_traj$day=as.Date(envi_traj$Date)
##UT=====
envi_ut=subset(envi_traj,envi_traj$Grouplab=="UT")
dim(envi_ut)
traj_ut_20 <- readRDS("Backup/traj/TrajData_ul_2020.rds")
traj_ut_21 <- readRDS("Backup/traj/TrajData_ul_2021.rds")
traj_ut=rbind(traj_ut_20,traj_ut_21)

traj_ut
traj_ut_sel=subset(traj_ut,traj_ut$hour.inc>-73)

traj_ut_sel$local=traj_ut_sel$date+3600*8
traj_ut_sel$day=as.Date(traj_ut_sel$local-3600*10)

traj_ut_sel2 <- inner_join(traj_ut_sel, envi_ut[,-c(1:3,5)], by = "day")
traj_ut_sel2

trajPlot(traj_ut_sel2,
         group = "day",
         col = "turbo", lwd = 2)

traj_ut_n=traj_ut_sel2
traj_ut_n$d3=traj_ut_n$date-3600*10
traj_ut_n$dn=as.character(traj_ut_n$day)

trajMap(traj_ut_n, 
        colour  = "dn",
        cols = mycol2,
        control = "dn",
        provider = "Stamen.Terrain"
)


##bj=====
envi_bj=subset(envi_traj,envi_traj$Grouplab=="BJ")
dim(envi_bj)
traj_bj_20 <- readRDS("Backup/traj/TrajData_bj_2020.rds")
traj_bj_21 <- readRDS("Backup/traj/TrajData_bj_2021.rds")
traj_bj=rbind(traj_bj_20,traj_bj_21)

traj_bj
traj_bj_sel=subset(traj_bj,traj_bj$hour.inc>-73)

traj_bj_sel$local=traj_bj_sel$date+3600*8
traj_bj_sel$day=as.Date(traj_bj_sel$local-3600*10)

traj_bj_sel2 <- inner_join(traj_bj_sel, envi_bj[,-c(1:3,5)], by = "day")
traj_bj_sel2

trajPlot(traj_bj_sel2,
         group = "day",
         col = "turbo", lwd = 2)

traj_bj_n=traj_bj_sel2
traj_bj_n$d3=traj_bj_n$date-3600*10
traj_bj_n$dn=as.character(traj_bj_n$day)


trajMap(traj_bj_n, 
        colour  = "dn",
        cols = mycol2,
        control = "dn",
        provider = "Stamen.Terrain"
)

trajLevelMap(traj_bj_n, statistic = "frequency",
             provider = "Stamen.Terrain",
             lat.inc = 0.5,
             lon.inc = 0.5,
)

trajLevel(traj_bj_n,
          pollutant = "PM2.5",
          statistic =  "sqtba",
          map.fill = FALSE,
          cols = "default",
          lat.inc = 0.5,
          lon.inc = 0.5,
          projection ="albers",
          orientation = c(0,45,0),
          xlim=c(70,140),
          ylim=c(33,53)
)



##ss=====
envi_ss=subset(envi_traj,envi_traj$Grouplab=="SS")
dim(envi_ss)
traj_ss_20 <- readRDS("Backup/traj/TrajData_ss_2020.rds")
traj_ss_21 <- readRDS("Backup/traj/TrajData_ss_2021.rds")
traj_ss=rbind(traj_ss_20,traj_ss_21)

traj_ss
traj_ss_sel=subset(traj_ss,traj_ss$hour.inc>-73)

traj_ss_sel$local=traj_ss_sel$date+3600*8
traj_ss_sel$day=as.Date(traj_ss_sel$local-3600*10)

traj_ss_sel2 <- inner_join(traj_ss_sel, envi_ss[,-c(1:3,5)], by = "day")
traj_ss_sel2

traj_ss_n=traj_ss_sel2
traj_ss_n$d3=traj_ss_n$date-3600*10
traj_ss_n$dn=as.character(traj_ss_n$day)

trajMap(traj_ss_n, 
        colour  = "dn",
        cols = mycol2,
        control = "dn",
        provider = "Stamen.Terrain"
)


trajLevelMap(traj_ss_n, statistic = "frequency")

traj_ss_n


trajLevel(traj_ss_n,
          pollutant = "PM2.5",
          statistic =  "sqtba",
          map.fill = FALSE,
          cols = "default",
          lat.inc = 0.5,
          lon.inc = 0.5,
          projection ="albers",
          orientation = c(0,45,0),
          xlim=c(70,140),
          ylim=c(33,53)
)

trajLevelMap(traj_ss_n, statistic = "frequency",
             provider = "Stamen.Terrain",
             lat.inc = 0.5,
             lon.inc = 0.5,
             )

trajLevelMap(ss_clust$data$traj, statistic = "frequency",
             provider = "Stamen.Terrain"
)


##se=====
envi_se=subset(envi_traj,envi_traj$Grouplab=="SE")
dim(envi_se)
traj_se_20 <- readRDS("Backup/traj/TrajData_se_2020.rds")
traj_se_21 <- readRDS("Backup/traj/TrajData_se_2021.rds")
traj_se=rbind(traj_se_20,traj_se_21)

traj_se
traj_se_sel=subset(traj_se,traj_se$hour.inc>-73)

traj_se_sel$local=traj_se_sel$date+3600*8
traj_se_sel$day=as.Date(traj_se_sel$local-3600*10)

traj_se_sel2 <- inner_join(traj_se_sel, envi_se[,-c(1:3,5)], by = "day")
traj_se_sel2


traj_se_n=traj_se_sel2
traj_se_n$d3=traj_se_n$date-3600*10
traj_se_n$dn=as.character(traj_se_n$day)

trajMap(traj_se_n, 
        colour  = "dn",
        cols = mycol2,
        control = "dn",
        provider = "Stamen.Terrain"
)

trajLevelMap(traj_se_n, statistic = "frequency",
             provider = "Stamen.Terrain",
             lat.inc = 0.5,
             lon.inc = 0.5,
)

trajLevel(traj_se_n,
          pollutant = "PM2.5",
          statistic =  "sqtba",
          map.fill = FALSE,
          cols = "default",
          lat.inc = 0.5,
          lon.inc = 0.5,
          projection ="albers",
          orientation = c(0,45,0),
          xlim=c(70,140),
          ylim=c(33,53)
)

##nt=====
envi_nt=subset(envi_traj,envi_traj$Grouplab=="NT")
dim(envi_nt)
traj_nt_20 <- readRDS("Backup/traj/TrajData_nt_2020.rds")
traj_nt_21 <- readRDS("Backup/traj/TrajData_nt_2021.rds")
traj_nt=rbind(traj_nt_20,traj_nt_21)

traj_nt
traj_nt_sel=subset(traj_nt,traj_nt$hour.inc>-73)

traj_nt_sel$local=traj_nt_sel$date+3600*8
traj_nt_sel$day=as.Date(traj_nt_sel$local-3600*10)

traj_nt_sel2 <- inner_join(traj_nt_sel, envi_nt[,-c(1:3,5)], by = "day")
traj_nt_sel2

traj_nt_n=traj_nt_sel2
traj_nt_n$d3=traj_nt_n$date-3600*10
traj_nt_n$dn=as.character(traj_nt_n$day)

trajMap(traj_nt_n, 
        colour  = "dn",
        cols = mycol2,
        control = "dn",
        provider = "Stamen.Terrain"
)

trajLevelMap(traj_nt_n, statistic = "frequency",
             provider = "Stamen.Terrain",
             lat.inc = 0.5,
             lon.inc = 0.5,
)

trajLevel(traj_nt_n,
          pollutant = "PM2.5",
          statistic =  "sqtba",
          map.fill = FALSE,
          cols = "default",
          lat.inc = 0.5,
          lon.inc = 0.5,
          projection ="albers",
          orientation = c(0,45,0),
          xlim=c(70,140),
          ylim=c(33,53)
)



for (i in 2:5) {
  
  tiff(paste0("Figure/ut_ctraj_n",i,"_euclid.tiff"), width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
  ut_clust <- trajCluster(traj_ut_n, 
                          # method = "angle", 
                           method = "euclid", 
                           n.cluster = i, 
                           col = c("#BC3C29FF","royalblue4","#E18727FF","#638C80","#7868E6"),
                           projection ="albers",
                           orientation = c(0,45,0),
                           map.cols ="grey45",
                           xlab="Longitude (°)",
                           ylab="Latitude (°)",
                           xlim=c(70,140),
                           ylim=c(33,53)
  )
  dev.off()
  
  
  tiff(paste0("Figure/bj_ctraj_n",i,"_euclid.tiff"), width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
  bj_clust <- trajCluster(traj_bj_n, 
                          #method = "angle", 
                           method = "euclid", 
                          n.cluster = i, 
                          col = c("#BC3C29FF","royalblue4","#E18727FF","#638C80","#7868E6"),
                          projection ="albers",
                          orientation = c(0,45,0),
                          map.cols ="grey45",
                          xlab="Longitude (°)",
                          ylab="Latitude (°)",
                          xlim=c(70,140),
                          ylim=c(33,53)
  )
  dev.off()
  
  
  tiff(paste0("Figure/ss_ctraj_n",i,"_euclid.tiff"), width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
  ss_clust <- trajCluster(traj_ss_n, 
                          #method = "angle", 
                          method = "euclid", 
                          n.cluster = i, 
                          col = c("#BC3C29FF","royalblue4","#E18727FF","#638C80","#7868E6"),
                          projection ="albers",
                          orientation = c(0,45,0),
                          map.cols ="grey45",
                          xlab="Longitude (°)",
                          ylab="Latitude (°)",
                          xlim=c(70,140),
                          ylim=c(33,53)
  )
  dev.off()
  
  tiff(paste0("Figure/se_ctraj_n",i,"_euclid.tiff"), width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
  se_clust <- trajCluster(traj_se_n, 
                          #method = "angle", 
                          method = "euclid", 
                          n.cluster = i, 
                          col = c("#BC3C29FF","royalblue4","#E18727FF","#638C80","#7868E6"),
                          projection ="albers",
                          orientation = c(0,45,0),
                          map.cols ="grey45",
                          xlab="Longitude (°)",
                          ylab="Latitude (°)",
                          xlim=c(70,140),
                          ylim=c(33,53)
  )
  dev.off()
  
  
  tiff(paste0("Figure/nt_ctraj_n",i,"_euclid.tiff"), width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
  nt_clust <- trajCluster(traj_nt_n, 
                          #method = "angle", 
                          method = "euclid", 
                          n.cluster = i, 
                          col = c("#BC3C29FF","royalblue4","#E18727FF","#638C80","#7868E6"),
                          projection ="albers",
                          orientation = c(0,45,0),
                          map.cols ="grey45",
                          xlab="Longitude (°)",
                          ylab="Latitude (°)",
                          xlim=c(70,140),
                          ylim=c(33,53)
  )
  dev.off()
}

c("#FF5D00","#7868E6","#638C80")
tiff(paste0("Figure/ut_ctraj_n","_euclid_fin.tiff"), width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
ut_clust_fin <- trajCluster(traj_ut_n, 
                        # method = "angle", 
                        method = "euclid", 
                        n.cluster = 3, 
                        col = c("#FF5D00","#7868E6","#638C80"),
                        projection ="albers",
                        orientation = c(0,45,0),
                        map.cols ="grey45",
                        xlab="Longitude (°)",
                        ylab="Latitude (°)",
                        xlim=c(70,140),
                        ylim=c(33,53)
)
dev.off()

tiff(paste0("Figure/bj_ctraj_n","_euclid_fin.tiff"), width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
bj_clust_fin <- trajCluster(traj_bj_n, 
                        #method = "angle", 
                        method = "euclid", 
                        n.cluster = 3, 
                        col = c("#FF5D00","#7868E6","#638C80"),
                        projection ="albers",
                        orientation = c(0,45,0),
                        map.cols ="grey45",
                        xlab="Longitude (°)",
                        ylab="Latitude (°)",
                        xlim=c(70,140),
                        ylim=c(33,53)
)
dev.off()

tiff(paste0("Figure/ss_ctraj_n","_euclid_fin.tiff"), width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
ss_clust_fin <- trajCluster(traj_ss_n, 
                        method = "angle", 
                        #method = "euclid", 
                        n.cluster = 3, 
                        col = c("#FF5D00","#7868E6","#638C80"),
                        projection ="albers",
                        orientation = c(0,45,0),
                        map.cols ="grey45",
                        xlab="Longitude (°)",
                        ylab="Latitude (°)",
                        xlim=c(70,140),
                        ylim=c(33,53)
)
dev.off()


tiff(paste0("Figure/se_ctraj_n","_euclid_fin.tiff"), width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
se_clust_fin <- trajCluster(traj_se_n, 
                        method = "angle", 
                        #method = "euclid", 
                        n.cluster = 2, 
                        col = c("#FF5D00","#7868E6"),
                        projection ="albers",
                        orientation = c(0,45,0),
                        map.cols ="grey45",
                        xlab="Longitude (°)",
                        ylab="Latitude (°)",
                        xlim=c(70,140),
                        ylim=c(33,53)
)
dev.off()

tiff(paste0("Figure/nt_ctraj_n","_euclid_fin.tiff"), width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
nt_clust_fin <- trajCluster(traj_nt_n, 
                        method = "angle", 
                        #method = "euclid", 
                        n.cluster = 3, 
                        col = c("#FF5D00","#7868E6","#638C80"),
                        projection ="albers",
                        orientation = c(0,45,0),
                        map.cols ="grey45",
                        xlab="Longitude (°)",
                        ylab="Latitude (°)",
                        xlim=c(70,140),
                        ylim=c(33,53)
)
dev.off()

traj_ut_n
traj_bj_n
traj_ss_n
traj_se_n
traj_nt_n

ut_clust_fin
bj_clust_fin
ss_clust_fin
se_clust_fin
nt_clust_fin

traj_ut_n_fin <- left_join(traj_ut_n, 
                  filter(ut_clust_fin$data$traj[,c("hour.inc","date","cluster")], hour.inc == 0), 
                  by = "date")

traj_bj_n_fin <- left_join(traj_bj_n, 
                           filter(bj_clust_fin$data$traj[,c("hour.inc","date","cluster")], hour.inc == 0), 
                           by = "date")

traj_ss_n_fin <- left_join(traj_ss_n, 
                           filter(ss_clust_fin$data$traj[,c("hour.inc","date","cluster")], hour.inc == 0), 
                           by = "date")

traj_se_n_fin <- left_join(traj_se_n, 
                           filter(se_clust_fin$data$traj[,c("hour.inc","date","cluster")], hour.inc == 0), 
                           by = "date")

traj_nt_n_fin <- left_join(traj_nt_n, 
                           filter(nt_clust_fin$data$traj[,c("hour.inc","date","cluster")], hour.inc == 0), 
                           by = "date")


timeProp(traj_ut_n_fin, pollutant = "PM2.5", 
         avg.time = "day", proportion = "cluster",
         cols = "Set2", 
         key.position = "top", key.columns = 3)

timeProp(traj_bj_n_fin, pollutant = "PM2.5", 
         avg.time = "day", proportion = "cluster",
         cols = "Set2", 
         key.position = "top", key.columns = 3)

timeProp(traj_ss_n_fin, pollutant = "PM2.5", 
         avg.time = "day", proportion = "cluster",
         cols = "Set2", 
         key.position = "top", key.columns = 3)

timeProp(traj_se_n_fin, pollutant = "PM2.5", 
         avg.time = "day", proportion = "cluster",
         cols = "Set2", 
         key.position = "top", key.columns = 2)

timeProp(traj_nt_n_fin, pollutant = "PM2.5", 
         avg.time = "day", proportion = "cluster",
         cols = "Set2", 
         key.position = "top", key.columns = 3)

lims <- as.POSIXct(strptime(c("2020-12-15 6:00","2021-01-14 12:00"), format = "%Y-%m-%d %H:%M"))

gg_traj_ut=unique(traj_ut_n_fin[,c("Grouplab","cluster","day","PM2.5","local")])
gg_traj_ut$Freq=0.125
gg_traj_ut$weight=gg_traj_ut$Freq*gg_traj_ut$PM2.5

gg_traj_ut=gg_traj_ut %>% 
  mutate(date2 = as.POSIXct(gg_traj_ut$day, format = '%Y-%m-%d %H:%M'))

traj_ut_trend <- ggplot(gg_traj_ut, aes(x=date2, y=weight, fill=cluster))+
  geom_bar(stat = "identity", position = position_stack(reverse = T))+
  scale_fill_manual(values =c("#FF5D00","#7868E6","#638C80"))+
  scale_x_datetime('',
                   limits = lims,
                   date_breaks = '2 day',
                   date_labels = "%b-%d", expand = c(0.02,0.02))+
  scale_y_continuous(name = expression(bold("PM"["2.5"]~"("*"\u03bcg/"*m^"3"*")")),expand = c(0.05,0.05),
                     limits = c(0,190))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.1,0.3,0.1,0.05),"cm"),
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
        legend.text = element_text(size = 18, colour = "black",family = "Arial",margin = unit(c(0.1,0.3,0.1,0.3),"cm"), hjust = 0.5,vjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = "NULL")+
  xlab("")
traj_ut_trend

###bj==========
gg_traj_bj=unique(traj_bj_n_fin[,c("Grouplab","cluster","day","PM2.5","local")])
gg_traj_bj$Freq=0.125
gg_traj_bj$weight=gg_traj_bj$Freq*gg_traj_bj$PM2.5

gg_traj_bj=gg_traj_bj %>% 
  mutate(date2 = as.POSIXct(gg_traj_bj$day, format = '%Y-%m-%d %H:%M'))

traj_bj_trend <- ggplot(gg_traj_bj, aes(x=date2, y=weight, fill=cluster))+
  geom_bar(stat = "identity", position = position_stack())+
  scale_fill_manual(values =c("#FF5D00","#7868E6","#638C80"))+
  scale_x_datetime('',
                   limits = lims,
                   date_breaks = '2 day',
                   date_labels = "%b-%d", expand = c(0.02,0.02))+
  scale_y_continuous(name = expression(bold("PM"["2.5"]~"("*"\u03bcg/"*m^"3"*")")),expand = c(0.05,0.05))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.1,0.3,0.1,0.05),"cm"),
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
        legend.text = element_text(size = 18, colour = "black",family = "Arial",margin = unit(c(0.1,0.3,0.1,0.3),"cm"), hjust = 0.5,vjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = "NULL")+
  xlab("")

traj_bj_trend

###ss====

gg_traj_ss=unique(traj_ss_n_fin[,c("Grouplab","cluster","day","PM2.5","local")])
gg_traj_ss$Freq=0.125
gg_traj_ss$weight=gg_traj_ss$Freq*gg_traj_ss$PM2.5

gg_traj_ss=gg_traj_ss %>% 
  mutate(date2 = as.POSIXct(gg_traj_ss$day, format = '%Y-%m-%d %H:%M'))

traj_ss_trend <- ggplot(gg_traj_ss, aes(x=date2, y=weight, fill=cluster))+
  geom_bar(stat = "identity", position = position_stack())+
  scale_fill_manual(values =c("#FF5D00","#7868E6","#638C80"))+
  scale_x_datetime('',
                   limits = lims,
                   date_breaks = '2 day',
                   date_labels = "%b-%d", expand = c(0.02,0.02))+
  scale_y_continuous(name = expression(bold("PM"["2.5"]~"("*"\u03bcg/"*m^"3"*")")),expand = c(0.05,0.05))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.1,0.3,0.1,0.05),"cm"),
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
        legend.text = element_text(size = 18, colour = "black",family = "Arial",margin = unit(c(0.1,0.3,0.1,0.3),"cm"), hjust = 0.5,vjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = "NULL")+
  xlab("")

traj_ss_trend

##se====

gg_traj_se=unique(traj_se_n_fin[,c("Grouplab","cluster","day","PM2.5","local")])
gg_traj_se$Freq=0.125
gg_traj_se$weight=gg_traj_se$Freq*gg_traj_se$PM2.5

gg_traj_se=gg_traj_se %>% 
  mutate(date2 = as.POSIXct(gg_traj_se$day, format = '%Y-%m-%d %H:%M'))

traj_se_trend <- ggplot(gg_traj_se, aes(x=date2, y=weight, fill=cluster))+
  geom_bar(stat = "identity", position = position_stack())+
  scale_fill_manual(values =c("#FF5D00","#7868E6","#638C80"))+
  scale_x_datetime('',
                   limits = lims,
                   date_breaks = '2 day',
                   date_labels = "%b-%d", expand = c(0.02,0.02))+
  scale_y_continuous(name = expression(bold("PM"["2.5"]~"("*"\u03bcg/"*m^"3"*")")),expand = c(0.05,0.05))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.1,0.3,0.1,0.05),"cm"),
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
        legend.text = element_text(size = 18, colour = "black",family = "Arial",margin = unit(c(0.1,0.3,0.1,0.3),"cm"), hjust = 0.5,vjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = "NULL")+
  xlab("")

traj_se_trend

###nt====

gg_traj_nt=unique(traj_nt_n_fin[,c("Grouplab","cluster","day","PM2.5","local")])
gg_traj_nt$Freq=0.125
gg_traj_nt$weight=gg_traj_nt$Freq*gg_traj_nt$PM2.5

gg_traj_nt=gg_traj_nt %>% 
  mutate(date2 = as.POSIXct(gg_traj_nt$day, format = '%Y-%m-%d %H:%M'))

traj_nt_trend <- ggplot(gg_traj_nt, aes(x=date2, y=weight, fill=cluster))+
  geom_bar(stat = "identity", position = position_stack())+
  scale_fill_manual(values =c("#FF5D00","#7868E6","#638C80"))+
  scale_x_datetime('',
                   limits = lims,
                   date_breaks = '2 day',
                   date_labels = "%b-%d", expand = c(0.02,0.02))+
  scale_y_continuous(name = expression(bold("PM"["2.5"]~"("*"\u03bcg/"*m^"3"*")")),expand = c(0.05,0.05))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 20, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.1,0.3,0.1,0.05),"cm"),
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
        legend.text = element_text(size = 18, colour = "black",family = "Arial",margin = unit(c(0.1,0.3,0.1,0.3),"cm"), hjust = 0.5,vjust = 0.5,face=2),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 0.1,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = "NULL")+
  xlab("")

traj_nt_trend

traj_pmtrend_all<- traj_ut_trend/traj_bj_trend/traj_ss_trend/traj_se_trend/traj_nt_trend
traj_pmtrend_all+ggsave(filename("traj_pm_trend_all"),height = 40, width = 35, units = "cm", dpi = 300, compression="lzw")




traj_ut_n
traj_bj_n
traj_ss_n
traj_se_n
traj_nt_n

traj_winter_n=rbind(traj_nt_n,traj_se_n,traj_ss_n,traj_bj_n,traj_ut_n)

table(traj_winter_n$site)

trajLevelMap(traj_ut_n,
             pollutant = "PM2.5",
             statistic = "sqtba",
             lat.inc = 0.5,
             lon.inc = 0.5,
)

trajLevelMap(traj_bj_n,
             pollutant = "PM2.5",
             statistic = "sqtba",
             lat.inc = 0.5,
             lon.inc = 0.5,
)

trajLevelMap(traj_ss_n,
             pollutant = "PM2.5",
             statistic = "sqtba",
             lat.inc = 0.5,
             lon.inc = 0.5,
)

trajLevelMap(traj_se_n,
             pollutant = "PM2.5",
             statistic = "sqtba",
             lat.inc = 0.5,
             lon.inc = 0.5,
)

trajLevelMap(traj_nt_n,
             pollutant = "PM2.5",
             statistic = "sqtba",
             lat.inc = 0.5,
             lon.inc = 0.5,
)


trajLevelMap(traj_winter_n,
             pollutant = "PM2.5",
             statistic = "cwt",
             lat.inc = 0.5,
             lon.inc = 0.5,
             )

trajLevelMap(traj_winter_n,
             pollutant = "PM2.5",
             statistic = "pscf",
             provider = "Stamen.Terrain",
             lat.inc = 0.5,
             lon.inc = 0.5,
)


trajLevelMap(traj_winter_n,
             pollutant = "PM2.5",
             statistic = "sqtba",
             .combine = "Grouplab",
             lat.inc = 0.5,
             lon.inc = 0.5,
)

traj_winter_n=rbind(traj_nt_n,traj_se_n,traj_ss_n,traj_bj_n,traj_ut_n)
traj_winter_n2=rbind(traj_se_n,traj_ss_n,traj_bj_n,traj_ut_n)

trajLevelMap(traj_winter_n2,
             pollutant = "PM2.5",
             statistic = "sqtba",
             .combine = "Grouplab",
             lat.inc = 0.5,
             lon.inc = 0.5,
             provider = "Stamen.Terrain",
)

trajLevelMap(
  traj_winter_n,
  pollutant = "PM2.5",
  statistic = "cwt",
  smooth = F,
  col = "increment",
  provider = "Stamen.Terrain"
)

trajLevelMap(
  traj_winter_n,
  pollutant = "PM2.5",
  statistic = "pscf",
  smooth = TRUE,
  col = "increment",
  provider = "Stamen.Terrain"
)



trajLevelMap(traj_data, statistic = "frequency")




leaflet() %>%
  addTiles() %>%
  addTrajPaths(data = uk,
               color = "blue",
               group = "London, UK", 
               opacity = .25) %>%
  addMarkers(data = dplyr::slice_head(uk, n = 1),
             lat = ~lat, lng = ~lon,
             group = "London, UK", label = "UK") %>%
  addTrajPaths(data = france,
               color = "red",
               group = "Paris, France", 
               opacity = .25) %>%
  addMarkers(data = dplyr::slice_head(france, n = 1), 
             lat = ~lat, lng = ~lon,
             group = "Paris, France", label = "FR") %>%
  addLayersControl(overlayGroups = c("Paris, France", "London, UK"))

