cor_1st_sep_ul2_soc
cor_1st_sep_bj2_soc
cor_1st_sep_ss2_soc
cor_1st_sep_sul2_soc
cor_1st_sep_nt2_soc


soc_cor_1st=rbind(cor_1st_sep_ul2_soc,cor_1st_sep_bj2_soc,cor_1st_sep_ss2_soc,cor_1st_sep_sul2_soc,cor_1st_sep_nt2_soc)
soc_cor_1st
soc_cor_all=rbind(soc_cor_1st)

soc_cor_all

soc_cor_all_pos=subset(soc_cor_all,soc_cor_all$value>0)
soc_cor_all_pos
soc_cor_all_pos$pd=ifelse(soc_cor_all_pos$ss=="Winter","1st",
                          ifelse(soc_cor_all_pos$ss=="Summer","2nd","3rd"))
frd_merge

frd_soc=frd_merge
frd_soc=frd_soc %>% left_join(soc_cor_all_pos[,c("Group","Formula","pd","Type")])

frd_soc_sel=subset(frd_soc,frd_soc$Type=="SOC")
frd_soc_sel

soc_dis_inty=aggregate(frd_soc_sel$Bromo.Inty, by=list(Group=frd_soc_sel$Group,
                                                  Pd=frd_soc_sel$pd, No=frd_soc_sel$No,AIclass=frd_soc_sel$AIClass),sum)

soc_dis_inty_tot=aggregate(frd_soc_sel$Bromo.Inty, by=list(Group=frd_soc_sel$Group,
                                                  Pd=frd_soc_sel$pd, No=frd_soc_sel$No),sum) %>% `colnames<-`(c("Group","Pd","No","Tot"))

soc_dis_inty=soc_dis_inty %>% left_join(soc_dis_inty_tot)
soc_dis_inty$rel=soc_dis_inty$x/soc_dis_inty$Tot*100
soc_dis_inty

soc_dis_inty_m=melt(soc_dis_inty[,c("Group","Pd","No","AIclass","rel")], id.vars=c("Group","Pd","No","AIclass")) %>% 
  dcast(Group+Pd~AIclass, value.var = "value",mean) %>% 
  melt(id.vars=c("Group","Pd"))

soc_dis_inty_m$Pdlab=factor(soc_dis_inty_m$Pd,levels = c("1st","2nd","3rd"),
                     labels = c(expression(bold("1"^"st"~"(Winter)")),
                                expression(bold("2"^"nd"~"(Summer)")),
                                expression(bold("3"^"rd"~"(Spring)"))
                     ))


soc_dis_inty_m$Grouplab=factor(soc_dis_inty_m$Group, levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"),
                        labels = c(expression(bold("UB")),
                                   expression(bold("BJ")),
                                   expression(bold("SS")),
                                   expression(bold("SE")),
                                   expression(bold("NT"))
                        ))

soc_dis_inty_m$Class=factor(soc_dis_inty_m$variable, levels = c("Polycyclic condensed aromatics","Polyphenolic",
                                                         "Highly unsaturated and phenolic","Aliphatic compounds",
                                                         "Saturated compounds"))

soc_dis_inty_m

soc_dis_inty_m_1st=subset(soc_dis_inty_m,soc_dis_inty_m$Pd=="1st")
soc_dis_inty_m_2nd=subset(soc_dis_inty_m,soc_dis_inty_m$Pd=="2nd")
soc_dis_inty_m_3rd=subset(soc_dis_inty_m,soc_dis_inty_m$Pd=="3rd")

soc_dis_inty_m_1st=soc_dis_inty_m_1st[order(soc_dis_inty_m_1st$value),]

ggplot()+
  #geom_bar(data=soc_dis_m_1st,aes(x="",y=value, fill=Class), col="black",stat="identity", size=0.5,
  #         position = position_fill(reverse = T))+
  geom_bar(data=soc_dis_inty_m_1st,aes(x="",y=value, fill=Class), col="black",stat="identity", size=0.5,
           position = position_fill(reverse = T),width = 1)+
  #geom_text(data=soc_dis_m_1st2,aes(x="",y=pos/500,label = round(value, digits = 2)), position = position_stack(vjust = 0.5)) +
  coord_polar("y")+
  scale_fill_manual(values = rev(c("grey70","#B8CBB5","#9DB9D1","#F9D38A","#958CC5")))+
  facet_grid2(Pdlab~Grouplab,independent = F, switch = "y",labeller = label_parsed )+
  theme_bw()+
  theme(
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
  guides(fill=guide_legend(reverse = T))+
  ggsave("Figure/230419pie_soc_1st_inty_weight.png",height = 15, width = 50, units = "cm", dpi = 300)


soc_dis_inty_m_2nd=soc_dis_inty_m_2nd[order(soc_dis_inty_m_2nd$value),]

ggplot()+
  geom_bar(data=soc_dis_inty_m_2nd,aes(x="",y=value, fill=Class), col="black",stat="identity", size=0.5,
           position = position_fill(reverse = T))+
  coord_polar("y")+
  scale_fill_manual(values = rev(c("grey70","#B8CBB5","#9DB9D1","#F9D38A","#958CC5")))+
  facet_grid2(Pdlab~Grouplab,independent = F, switch = "y",labeller = label_parsed )+
  theme_bw()+
  theme(
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
  guides(fill=guide_legend(reverse = T))+
  ggsave("Figure/230419pie_soc_2nd_inty_weight.png",height = 15, width = 50, units = "cm", dpi = 300)


soc_dis_inty_m_3rd=soc_dis_inty_m_3rd[order(soc_dis_inty_m_3rd$value),]

ggplot()+
  geom_bar(data=soc_dis_inty_m_3rd,aes(x="",y=value, fill=Class), col="black",stat="identity", size=0.5,
           position = position_fill(reverse = T))+
  coord_polar("y")+
  scale_fill_manual(values = rev(c("grey70","#B8CBB5","#9DB9D1","#F9D38A","#958CC5")))+
  facet_grid2(Pdlab~Grouplab,independent = F, switch = "y",labeller = label_parsed )+
  theme_bw()+
  theme(
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
  guides(fill=guide_legend(reverse = T))+
  ggsave("Figure/230419pie_soc_3rd_inty_weight.png",height = 15, width = 50, units = "cm", dpi = 300)


##Freq=====

soc_dis=aggregate(frd_soc_sel$Freq, by=list(Group=frd_soc_sel$Group,
                                                  Pd=frd_soc_sel$pd, No=frd_soc_sel$No,AIclass=frd_soc_sel$AIClass),sum)

soc_dis_tot=aggregate(frd_soc_sel$Freq, by=list(Group=frd_soc_sel$Group,
                                                      Pd=frd_soc_sel$pd, No=frd_soc_sel$No),sum) %>% `colnames<-`(c("Group","Pd","No","Tot"))

soc_dis=soc_dis %>% left_join(soc_dis_tot)
soc_dis$rel=soc_dis$x/soc_dis$Tot*100
soc_dis

soc_dis_m=melt(soc_dis[,c("Group","Pd","No","AIclass","rel")], id.vars=c("Group","Pd","No","AIclass")) %>% 
  dcast(Group+Pd~AIclass, value.var = "value",mean) %>% 
  melt(id.vars=c("Group","Pd"))

soc_dis_m$Pdlab=factor(soc_dis_m$Pd,levels = c("1st","2nd","3rd"),
                       labels = c(expression(bold("1"^"st"~"(Winter)")),
                                  expression(bold("2"^"nd"~"(Summer)")),
                                  expression(bold("3"^"rd"~"(Spring)"))
                       ))


soc_dis_m$Grouplab=factor(soc_dis_m$Group, levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"),
                          labels = c(expression(bold("UB")),
                                     expression(bold("BJ")),
                                     expression(bold("SS")),
                                     expression(bold("SE")),
                                     expression(bold("NT"))
                          ))

soc_dis_m$Class=factor(soc_dis_m$variable, levels = c("Polycyclic condensed aromatics","Polyphenolic",
                                                      "Highly unsaturated and phenolic","Aliphatic compounds",
                                                      "Saturated compounds"))

soc_dis_m
soc_dis_m_1st=subset(soc_dis_m,soc_dis_m$Pd=="1st")
soc_dis_m_2nd=subset(soc_dis_m,soc_dis_m$Pd=="2nd")
soc_dis_m_3rd=subset(soc_dis_m,soc_dis_m$Pd=="3rd")

soc_dis_m_1st=soc_dis_m_1st[order(soc_dis_m_1st$value),]


ggplot()+
  geom_bar(data=soc_dis_m_1st,aes(x="",y=value, fill=Class),col="black",stat="identity", size=0.5,
           position = position_fill(reverse = T))+
  coord_polar("y")+
  #scale_color_manual(values =c("black","black","black","black","black"))+
  scale_fill_manual(values = rev(c("grey70","#B8CBB5","#9DB9D1","#F9D38A","#958CC5")))+
  #scale_color_manual(values = rev(c("grey70","#B8CBB5","#9DB9D1","#F9D38A","#958CC5")))+
  facet_grid2(Pdlab~Grouplab,independent = F, switch = "y",labeller = label_parsed )+
  theme_bw()+
  theme(
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
  guides(fill=guide_legend(reverse = T))+
  ggsave("Figure/230419pie_soc_1st_freq_weight.tiff",height = 15, width = 50, units = "cm", dpi = 300)


soc_dis_m_2nd=soc_dis_m_2nd[order(soc_dis_m_2nd$value),]

  
ggplot()+
  geom_bar(data=soc_dis_m_2nd,aes(x="",y=value, fill=Class), col="black",stat="identity", size=0.5,
           position = position_fill(reverse = T))+
  coord_polar("y")+
  scale_fill_manual(values = rev(c("grey70","#B8CBB5","#9DB9D1","#F9D38A","#958CC5")))+
  facet_grid2(Pdlab~Grouplab,independent = F, switch = "y",labeller = label_parsed )+
  theme_bw()+
  theme(
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
  guides(fill=guide_legend(reverse = T))+
  ggsave("Figure/230419pie_soc_2nd_freq_weight.png",height = 15, width = 50, units = "cm", dpi = 300)


soc_dis_m_3rd=soc_dis_m_3rd[order(soc_dis_m_3rd$value),]

ggplot()+
  geom_bar(data=soc_dis_m_3rd,aes(x="",y=value, fill=Class), col="black",stat="identity", size=0.5,
           position = position_fill(reverse = T))+
  coord_polar("y")+
  scale_fill_manual(values = rev(c("grey70","#B8CBB5","#9DB9D1","#F9D38A","#958CC5")))+
  facet_grid2(Pdlab~Grouplab,independent = F, switch = "y",labeller = label_parsed )+
  theme_bw()+
  theme(
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
  guides(fill=guide_legend(reverse = T))+
  ggsave("Figure/230419pie_soc_3rd_freq_weight.png",height = 15, width = 50, units = "cm", dpi = 300)

