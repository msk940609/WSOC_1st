dall_sel

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
frd_match_all=frd_match_all %>% left_join()

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


