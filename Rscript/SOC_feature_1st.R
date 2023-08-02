cor_test_1st_raw=fread("Datafile/molecular_cor_raw_1st.csv")
cor_test_1st_raw
cor_test_1st=cor_test_1st_raw

cor_test_1st$Envi=ifelse(cor_test_1st_raw$Envi=="WSOCbb","POC","SOC")

frd_1st_re=ft_1st
frd_1st_re$Freq=1
frd_1st_re

frd_1st_re$id=frd_1st_re$Sample
frd_1st_re=frd_1st_re %>% separate(Sample, c("Proj","pd","smp"),sep = "_")
frd_1st_re=frd_1st_re %>% separate(smp, c("Group","No"), sep = "-")
frd_1st_re$Group=ifelse(frd_1st_re$Group=="Ulan","Ulaanbaatar",frd_1st_re$Group)
frd_1st_re$Freq=1

table(frd_1st_re$Group)
tt=aggregate(frd_1st_re$Freq,by=list(Group=frd_1st_re$Group, Formula=frd_1st_re$Formula),sum) %>% `colnames<-`(c("Group","Formula","cnt"))
min(tt$cnt)
frd_1st_re=frd_1st_re %>% left_join(tt)

fm_list_1st=unique(frd_1st_re[,c("Group","Formula","cnt")])

cor_test_1st
cor_test_1st=cor_test_1st %>% inner_join(fm_list_1st)
#cor_test_1st_raw=cor_test_1st

cor_test_1st=subset(cor_test_1st,cor_test_1st$cnt>4)
table(cor_test_1st$Envi)

CHEMICAL_ELEMENTS = c("C","H","N","O","S")
molecularFormula <- unique(cor_test_1st$Formula)

# extract numerical formulae
numericalFormula <- array(0, dim=c(length(molecularFormula), length(CHEMICAL_ELEMENTS)))
for (k in 1:length(molecularFormula)){
  #k=1
  formula <- molecularFormula[k]
  ge <- gregexpr("[A-Z]\\d*", formula, perl=TRUE)
  s_index <- ge[[1]]
  s_len <- attr(s_index, "match.length")
  for (i in 1:length(s_len)){
    #i=1
    token <- substr(formula, s_index[i], s_index[i] + s_len[i] - 1)
    element <- substr(token, 1, 1)
    if (grepl(element, "CHNOSP")) {
      idx = which(CHEMICAL_ELEMENTS %in% element)
      if (numericalFormula[k, idx] > 0) next  # for C13
      if (s_len[i] == 1) {
        numericalFormula[k, idx] = 1
      } else {
        numElement <- try(strtoi(substr(formula, s_index[i] + 1, s_index[i] + s_len[i] - 1)))
        if (class(numElement)=="integer"){
          numericalFormula[k, idx] = numElement
        } else {
          print(paste("[ERROR] an unknown chemical element found:", token, "in", formula))
        }
      }
    } else {
      print(paste("[ERROR] an unknown chemical element found:", element, "in", formula))
    }
  }
}


fm_1st=as.data.table(numericalFormula) %>% `colnames<-`(c("C","H","N","O","S"))
fm_1st$Formula=molecularFormula
fm_1st

fm_1st$C1=ifelse(fm_1st$C>0, "C","")
fm_1st$H1=ifelse(fm_1st$H>0, "H","")
fm_1st$O1=ifelse(fm_1st$O>0, "O","")
fm_1st$N1=ifelse(fm_1st$N>0, "N","")
fm_1st$S1=ifelse(fm_1st$S>0, "S","")

fm_1st=fm_1st %>% unite("Comp",c("C1","H1","O1","N1","S1"),sep = "")
fm_1st$Comp=ifelse(fm_1st$O==0, "Remainders",fm_1st$Comp)

cor_test_1st=cor_test_1st %>% inner_join(fm_1st)
cor_test_1st$O.C=cor_test_1st$O/cor_test_1st$C
cor_test_1st$H.C=cor_test_1st$H/cor_test_1st$C

cor_test_1st_sel=subset(cor_test_1st,cor_test_1st$p<0.05)
cor_test_1st_sel=subset(cor_test_1st_sel,abs(cor_test_1st_sel$rho)>0)
cor_test_1st_sel

cor_vk_1st_sel=cor_test_1st_sel %>% inner_join(fm_1st)

#cor_vk_1st_sel$rho=ifelse(cor_vk_1st_sel$Envi=="POC",
#                          ifelse(cor_vk_1st_sel$Comp!="CHO",cor_vk_1st_sel$rho*0.5,cor_vk_1st_sel$rho),cor_vk_1st_sel$rho)

cor_vk_1st_sel
cor_1st_soc=cor_vk_1st_sel %>% filter(Envi%in%c("POC","SOC"))

cor_1st_soc_sep=dcast(cor_1st_soc,Group+Formula+O.C+H.C+Comp~Envi,value.var = c("rho","p"),sum) %>%
  `colnames<-`(c("Group","Formula","O.C","H.C","Comp","rho_POC","rho_SOC","p_POC","p_SOC"))

cor_1st_soc_sep$Type=ifelse(abs(cor_1st_soc_sep$rho_POC)>abs(cor_1st_soc_sep$rho_SOC),"POC","SOC")
table(cor_1st_soc_sep$Type)

cor_1st_soc_sep$variable=ifelse(cor_1st_soc_sep$Type=="POC","POC","SOC")
cor_1st_soc_sep$value=ifelse(cor_1st_soc_sep$Type=="POC",cor_1st_soc_sep$rho_POC,cor_1st_soc_sep$rho_SOC)
cor_1st_soc_sep$p=ifelse(cor_1st_soc_sep$Type=="POC",cor_1st_soc_sep$p_POC,cor_1st_soc_sep$p_SOC)

cor_1st_soc_sep_sel=cor_1st_soc_sep[,c("Group","Comp","Formula","O.C","H.C","Type","variable","value","p")]
cor_1st_soc_sep_sel=subset(cor_1st_soc_sep_sel,abs(cor_1st_soc_sep_sel$value)>0.1)
cor_1st_soc_sep_sel

cor_1st_soc_sep_sel$Grouplab=factor(cor_1st_soc_sep_sel$Group,levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"),
                                    labels = c("UB","BJ","SS","SE","NT"))

cor_1st_soc_sep_sel$varlab=factor(cor_1st_soc_sep_sel$variable,levels = c("POC","SOC"))

cor_1st_soc_sep_sel$ss="Winter"
cor_1st_soc_sep_sel$sslab=factor(cor_1st_soc_sep_sel$ss,levels = "Winter")


###UB==================
cor_1st_sep_ul=subset(cor_1st_soc_sep_sel,cor_1st_soc_sep_sel$Grouplab=="UB")
cor_1st_sep_ul
table(cor_1st_sep_ul$Type)

cor_1st_sep_ul
cor_1st_sep_ul=cor_1st_sep_ul[order(cor_1st_sep_ul$value),]

cor_test_1st ##include insig
cor_test_1st_ul=subset(cor_test_1st,cor_test_1st$Group=="Ulaanbaatar") 
cor_1st_sep_ul
  
cor_test_1st_ul_grey=cor_test_1st_ul %>% filter(!Formula%in%c(unique(cor_1st_sep_ul$Formula)))
unique(cor_test_1st_ul_grey$Formula)

cor_1st_sep_ul2=subset(cor_1st_sep_ul,abs(cor_1st_sep_ul$value)>0.4)

ggplot()+
  geom_point(data=cor_test_1st_ul_grey, aes(x=O.C, y=H.C),col="grey70",size=2.5)+
  geom_point(data=cor_1st_sep_ul, aes(x=O.C, y=H.C,col=value),size=3.5)+
  facet_rep_grid(Grouplab~varlab, repeat.tick.labels = "all")+
  #facet_grid(Grouplab~varlab, labeller = label_parsed )+
  scale_x_continuous(name = "O/C",expand = c(0.01,0.01),limits = c(-0.01,1.0), breaks = round(seq(0,2.1,0.1),1),
                     labels =insert_minor_2(c("0","0.2","0.4","0.6","0.8","1.0","1.2","1.4","1.6","1.8","2.0","2.2"),2))+
  scale_y_continuous(name = "H/C",expand = c(0.01,0.01), limits = c(0.0,2.2),breaks = round(seq(0.0,2.5,0.1),1),
                     labels =insert_minor_1(c("0","0.5","1.0","1.5","2.0","2.5"),4))+
  #scale_color_gdradientn(colors=(matlab.like(40)[6:34]),breaks = c(-0.5, 0, 0.5), limits = c(-0.75,0.93))+
  #scale_color_gradientn(colors=(topo.colors(20)[4:17]))+
  scale_color_gradientn(colors=c(topo.colors(40)[1:38]),breaks = c(-0.8,-0.4, 0, 0.4,0.8), limits = c(-1,1))+
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
        #legend.position = "right",
        #legend.direction = "vertical",
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.background = element_blank(),
  )+
  #guides(col=guide_legend(title = "",ncol = 2,override.aes = list(shape=21,size=7, fill=c("#BC3C29FF","#EFC000FF","#008B45FF","#7876B1FF"),alpha=1)), size="none")+
  guides(col=guide_colorbar(title = expression(bolditalic("ρ")),ticks.colour = "black", ticks.linewidth = 1.5,
                            barwidth = 25, barheight = 2.5))+
  ggsave(filename("vk_cor_ul_1st_all_rho0.3"),height = 20, width = 45, units = "cm", dpi = 300)

cor_1st_sep_ul2_soc=subset(cor_1st_sep_ul2,cor_1st_sep_ul2$Type=="SOC")
cor_1st_sep_ul2_soc

table(cor_1st_sep_ul2_soc$Type)

ggplot()+
  geom_hline(yintercept = 1.5,lty=2,size=1)+
  geom_abline(slope = -1, intercept = 1.1,lty=2,size=1)+
  geom_abline(slope = -0.76, intercept = 0.75, lty=2,size=1)+
  geom_point(data=cor_test_1st_ul_grey, aes(x=O.C, y=H.C),col="grey70",size=2.5)+
  geom_point(data=cor_1st_sep_ul2_soc, aes(x=O.C, y=H.C,col=value),size=3.5)+
  facet_rep_grid(.~Grouplab, repeat.tick.labels = "all")+
  #facet_grid(Grouplab~varlab, labeller = label_parsed )+
  scale_x_continuous(name = "O/C",expand = c(0.01,0.01),limits = c(-0.01,1.0), breaks = round(seq(0,2.1,0.1),1),
                     labels =insert_minor_2(c("0","0.2","0.4","0.6","0.8","1.0","1.2","1.4","1.6","1.8","2.0","2.2"),2))+
  scale_y_continuous(name = "H/C",expand = c(0.01,0.01), limits = c(0.0,2.2),breaks = round(seq(0.0,2.5,0.1),1),
                     labels =insert_minor_1(c("0","0.5","1.0","1.5","2.0","2.5"),4))+
  #scale_color_gdradientn(colors=(matlab.like(40)[6:34]),breaks = c(-0.5, 0, 0.5), limits = c(-0.75,0.93))+
  #scale_color_gradientn(colors=(topo.colors(20)[4:17]))+
  scale_color_gradientn(colors=c(topo.colors(40)[1:38]))+
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
        #legend.position = "right",
        #legend.direction = "vertical",
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.background = element_blank(),
  )+
  #guides(col=guide_legend(title = "",ncol = 2,override.aes = list(shape=21,size=7, fill=c("#BC3C29FF","#EFC000FF","#008B45FF","#7876B1FF"),alpha=1)), size="none")+
  guides(col=guide_colorbar(title = expression(bolditalic("ρ")),ticks.colour = "black", ticks.linewidth = 1.5,
                            barwidth = 25, barheight = 2.5))+
  ggsave(filename("vk_cor_ul_1st_SOC_rho0.3"),height = 25, width = 20, units = "cm", dpi = 300)


##beijing=====
cor_1st_sep_bj=subset(cor_1st_soc_sep_sel,cor_1st_soc_sep_sel$Grouplab=="BJ")
cor_1st_sep_bj
table(cor_1st_sep_bj$Type)

cor_1st_sep_bj
cor_1st_sep_bj=cor_1st_sep_bj[order(cor_1st_sep_bj$value),]

cor_test_1st ##include insig
cor_test_1st_bj=subset(cor_test_1st,cor_test_1st$Group=="Beijing") 
cor_1st_sep_bj

cor_test_1st_bj_grey=cor_test_1st_bj %>% filter(!Formula%in%c(unique(cor_1st_sep_bj$Formula)))
unique(cor_test_1st_bj_grey$Formula)

cor_1st_sep_bj2=subset(cor_1st_sep_bj,abs(cor_1st_sep_bj$value)>0.4)

ggplot()+
  geom_point(data=cor_test_1st_bj_grey, aes(x=O.C, y=H.C),col="grey70",size=2.5)+
  geom_point(data=cor_1st_sep_bj, aes(x=O.C, y=H.C,col=value),size=3.5)+
  facet_rep_grid(Grouplab~varlab, repeat.tick.labels = "all")+
  #facet_grid(Grouplab~varlab, labeller = label_parsed )+
  scale_x_continuous(name = "O/C",expand = c(0.01,0.01),limits = c(-0.01,1.0), breaks = round(seq(0,2.1,0.1),1),
                     labels =insert_minor_2(c("0","0.2","0.4","0.6","0.8","1.0","1.2","1.4","1.6","1.8","2.0","2.2"),2))+
  scale_y_continuous(name = "H/C",expand = c(0.01,0.01), limits = c(0.0,2.2),breaks = round(seq(0.0,2.5,0.1),1),
                     labels =insert_minor_1(c("0","0.5","1.0","1.5","2.0","2.5"),4))+
  #scale_color_gdradientn(colors=(matlab.like(40)[6:34]),breaks = c(-0.5, 0, 0.5), limits = c(-0.75,0.93))+
  #scale_color_gradientn(colors=(topo.colors(20)[4:17]))+
  scale_color_gradientn(colors=c(topo.colors(40)[1:38]),breaks = c(-0.8,-0.4, 0, 0.4,0.8), limits = c(-1,1))+
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
  #guides(col=guide_legend(title = "",ncol = 2,override.aes = list(shape=21,size=7, fill=c("#BC3C29FF","#EFC000FF","#008B45FF","#7876B1FF"),alpha=1)), size="none")+
  guides(col=guide_colorbar(title = expression(bolditalic("ρ")),ticks.colour = "black", ticks.linewidth = 1.5,
                            barwidth = 2.5, barheight = 25))+
  ggsave(filename("vk_cor_bj_1st_all_rho0.3"),height = 20, width = 45, units = "cm", dpi = 300)

cor_1st_sep_bj2_soc=subset(cor_1st_sep_bj2,cor_1st_sep_bj2$Type=="SOC")
cor_1st_sep_bj2_soc

table(cor_1st_sep_bj2_soc$Type)

ggplot()+
  geom_hline(yintercept = 1.5,lty=2,size=1)+
  geom_abline(slope = -1, intercept = 1.1,lty=2,size=1)+
  geom_abline(slope = -0.76, intercept = 0.75, lty=2,size=1)+
  geom_point(data=cor_test_1st_bj_grey, aes(x=O.C, y=H.C),col="grey70",size=2.5)+
  geom_point(data=cor_1st_sep_bj2_soc, aes(x=O.C, y=H.C,col=value),size=3.5)+
  facet_rep_grid(.~Grouplab, repeat.tick.labels = "all")+
  #facet_grid(Grouplab~varlab, labeller = label_parsed )+
  scale_x_continuous(name = "O/C",expand = c(0.01,0.01),limits = c(-0.01,1.0), breaks = round(seq(0,2.1,0.1),1),
                     labels =insert_minor_2(c("0","0.2","0.4","0.6","0.8","1.0","1.2","1.4","1.6","1.8","2.0","2.2"),2))+
  scale_y_continuous(name = "H/C",expand = c(0.01,0.01), limits = c(0.0,2.2),breaks = round(seq(0.0,2.5,0.1),1),
                     labels =insert_minor_1(c("0","0.5","1.0","1.5","2.0","2.5"),4))+
  #scale_color_gdradientn(colors=(matlab.like(40)[6:34]),breaks = c(-0.5, 0, 0.5), limits = c(-0.75,0.93))+
  #scale_color_gradientn(colors=(topo.colors(20)[4:17]))+
  scale_color_gradientn(colors=c(topo.colors(40)[1:38]))+
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
        #legend.position = "right",
        #legend.direction = "vertical",
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.background = element_blank(),
  )+
  #guides(col=guide_legend(title = "",ncol = 2,override.aes = list(shape=21,size=7, fill=c("#BC3C29FF","#EFC000FF","#008B45FF","#7876B1FF"),alpha=1)), size="none")+
  guides(col=guide_colorbar(title = expression(bolditalic("ρ")),ticks.colour = "black", ticks.linewidth = 1.5,
                            barwidth = 25, barheight = 2.5))+
  ggsave(filename("vk_cor_bj_1st_SOC_rho0.3"),height = 25, width = 20, units = "cm", dpi = 300)


##Seosan=====
cor_1st_sep_ss=subset(cor_1st_soc_sep_sel,cor_1st_soc_sep_sel$Grouplab=="SS")
cor_1st_sep_ss
table(cor_1st_sep_ss$Type)

cor_1st_sep_ss
cor_1st_sep_ss=cor_1st_sep_ss[order(cor_1st_sep_ss$value),]

cor_test_1st ##include insig
cor_test_1st_ss=subset(cor_test_1st,cor_test_1st$Group=="Seosan") 
cor_1st_sep_ss

cor_test_1st_ss_grey=cor_test_1st_ss %>% filter(!Formula%in%c(unique(cor_1st_sep_ss$Formula)))
unique(cor_test_1st_ss_grey$Formula)

cor_1st_sep_ss2=subset(cor_1st_sep_ss,abs(cor_1st_sep_ss$value)>0.3)

ggplot()+
  geom_point(data=cor_test_1st_ss_grey, aes(x=O.C, y=H.C),col="grey70",size=2.5)+
  geom_point(data=cor_1st_sep_ss, aes(x=O.C, y=H.C,col=value),size=3.5)+
  facet_rep_grid(Grouplab~varlab, repeat.tick.labels = "all")+
  #facet_grid(Grouplab~varlab, labeller = label_parsed )+
  scale_x_continuous(name = "O/C",expand = c(0.01,0.01),limits = c(-0.01,1.0), breaks = round(seq(0,2.1,0.1),1),
                     labels =insert_minor_2(c("0","0.2","0.4","0.6","0.8","1.0","1.2","1.4","1.6","1.8","2.0","2.2"),2))+
  scale_y_continuous(name = "H/C",expand = c(0.01,0.01), limits = c(0.0,2.2),breaks = round(seq(0.0,2.5,0.1),1),
                     labels =insert_minor_1(c("0","0.5","1.0","1.5","2.0","2.5"),4))+
  #scale_color_gdradientn(colors=(matlab.like(40)[6:34]),breaks = c(-0.5, 0, 0.5), limits = c(-0.75,0.93))+
  #scale_color_gradientn(colors=(topo.colors(20)[4:17]))+
  scale_color_gradientn(colors=c(topo.colors(40)[1:38]),breaks = c(-0.8,-0.4, 0, 0.4,0.8), limits = c(-1,1))+
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
  #guides(col=guide_legend(title = "",ncol = 2,override.aes = list(shape=21,size=7, fill=c("#BC3C29FF","#EFC000FF","#008B45FF","#7876B1FF"),alpha=1)), size="none")+
  guides(col=guide_colorbar(title = expression(bolditalic("ρ")),ticks.colour = "black", ticks.linewidth = 1.5,
                            barwidth = 2.5, barheight = 25))+
  ggsave(filename("vk_cor_ss_1st_all_rho0.3"),height = 20, width = 45, units = "cm", dpi = 300)

cor_1st_sep_ss2_soc=subset(cor_1st_sep_ss2,cor_1st_sep_ss2$Type=="SOC")
cor_1st_sep_ss2_soc

table(cor_1st_sep_ss2_soc$Type)

ggplot()+
  geom_hline(yintercept = 1.5,lty=2,size=1)+
  geom_abline(slope = -1, intercept = 1.1,lty=2,size=1)+
  geom_abline(slope = -0.76, intercept = 0.75, lty=2,size=1)+
  geom_point(data=cor_test_1st_ss_grey, aes(x=O.C, y=H.C),col="grey70",size=2.5)+
  geom_point(data=cor_1st_sep_ss2_soc, aes(x=O.C, y=H.C,col=value),size=3.5)+
  facet_rep_grid(.~Grouplab, repeat.tick.labels = "all")+
  #facet_grid(Grouplab~varlab, labeller = label_parsed )+
  scale_x_continuous(name = "O/C",expand = c(0.01,0.01),limits = c(-0.01,1.0), breaks = round(seq(0,2.1,0.1),1),
                     labels =insert_minor_2(c("0","0.2","0.4","0.6","0.8","1.0","1.2","1.4","1.6","1.8","2.0","2.2"),2))+
  scale_y_continuous(name = "H/C",expand = c(0.01,0.01), limits = c(0.0,2.2),breaks = round(seq(0.0,2.5,0.1),1),
                     labels =insert_minor_1(c("0","0.5","1.0","1.5","2.0","2.5"),4))+
  #scale_color_gdradientn(colors=(matlab.like(40)[6:34]),breaks = c(-0.5, 0, 0.5), limits = c(-0.75,0.93))+
  #scale_color_gradientn(colors=(topo.colors(20)[4:17]))+
  scale_color_gradientn(colors=c(topo.colors(40)[1:38]))+
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
        #legend.position = "right",
        #legend.direction = "vertical",
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.background = element_blank(),
  )+
  #guides(col=guide_legend(title = "",ncol = 2,override.aes = list(shape=21,size=7, fill=c("#BC3C29FF","#EFC000FF","#008B45FF","#7876B1FF"),alpha=1)), size="none")+
  guides(col=guide_colorbar(title = expression(bolditalic("ρ")),ticks.colour = "black", ticks.linewidth = 1.5,
                            barwidth = 25, barheight = 2.5))+
  ggsave(filename("vk_cor_ss_1st_SOC_rho0.3"),height = 25, width = 20, units = "cm", dpi = 300)


##Seoul=====
cor_1st_sep_sul=subset(cor_1st_soc_sep_sel,cor_1st_soc_sep_sel$Grouplab=="SE")
cor_1st_sep_sul
table(cor_1st_sep_sul$Type)

cor_1st_sep_sul
cor_1st_sep_sul=cor_1st_sep_sul[order(cor_1st_sep_sul$value),]

cor_test_1st ##include insig
cor_test_1st_sul=subset(cor_test_1st,cor_test_1st$Group=="Seoul") 
cor_1st_sep_sul

cor_test_1st_sul_grey=cor_test_1st_sul %>% filter(!Formula%in%c(unique(cor_1st_sep_sul$Formula)))
unique(cor_test_1st_sul_grey$Formula)

cor_1st_sep_sul2=subset(cor_1st_sep_sul,abs(cor_1st_sep_sul$value)>0.3)

ggplot()+
  geom_point(data=cor_test_1st_sul_grey, aes(x=O.C, y=H.C),col="grey70",size=2.5)+
  geom_point(data=cor_1st_sep_sul, aes(x=O.C, y=H.C,col=value),size=3.5)+
  facet_rep_grid(Grouplab~varlab, repeat.tick.labels = "all")+
  #facet_grid(Grouplab~varlab, labeller = label_parsed )+
  scale_x_continuous(name = "O/C",expand = c(0.01,0.01),limits = c(-0.01,1.0), breaks = round(seq(0,2.1,0.1),1),
                     labels =insert_minor_2(c("0","0.2","0.4","0.6","0.8","1.0","1.2","1.4","1.6","1.8","2.0","2.2"),2))+
  scale_y_continuous(name = "H/C",expand = c(0.01,0.01), limits = c(0.0,2.2),breaks = round(seq(0.0,2.5,0.1),1),
                     labels =insert_minor_1(c("0","0.5","1.0","1.5","2.0","2.5"),4))+
  #scale_color_gdradientn(colors=(matlab.like(40)[6:34]),breaks = c(-0.5, 0, 0.5), limits = c(-0.75,0.93))+
  #scale_color_gradientn(colors=(topo.colors(20)[4:17]))+
  scale_color_gradientn(colors=c(topo.colors(40)[1:38]),breaks = c(-0.8,-0.4, 0, 0.4,0.8), limits = c(-1,1))+
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
  #guides(col=guide_legend(title = "",ncol = 2,override.aes = list(shape=21,size=7, fill=c("#BC3C29FF","#EFC000FF","#008B45FF","#7876B1FF"),alpha=1)), size="none")+
  guides(col=guide_colorbar(title = expression(bolditalic("ρ")),ticks.colour = "black", ticks.linewidth = 1.5,
                            barwidth = 2.5, barheight = 25))+
  ggsave(filename("vk_cor_sul_1st_all_rho0.3"),height = 20, width = 45, units = "cm", dpi = 300)

cor_1st_sep_sul2_soc=subset(cor_1st_sep_sul2,cor_1st_sep_sul2$Type=="SOC")
cor_1st_sep_sul2_soc

table(cor_1st_sep_sul2_soc$Type)

ggplot()+
  geom_hline(yintercept = 1.5,lty=2,size=1)+
  geom_abline(slope = -1, intercept = 1.1,lty=2,size=1)+
  geom_abline(slope = -0.76, intercept = 0.75, lty=2,size=1)+
  geom_point(data=cor_test_1st_sul_grey, aes(x=O.C, y=H.C),col="grey70",size=2.5)+
  geom_point(data=cor_1st_sep_sul2_soc, aes(x=O.C, y=H.C,col=value),size=3.5)+
  facet_rep_grid(.~Grouplab, repeat.tick.labels = "all")+
  #facet_grid(Grouplab~varlab, labeller = label_parsed )+
  scale_x_continuous(name = "O/C",expand = c(0.01,0.01),limits = c(-0.01,1.0), breaks = round(seq(0,2.1,0.1),1),
                     labels =insert_minor_2(c("0","0.2","0.4","0.6","0.8","1.0","1.2","1.4","1.6","1.8","2.0","2.2"),2))+
  scale_y_continuous(name = "H/C",expand = c(0.01,0.01), limits = c(0.0,2.2),breaks = round(seq(0.0,2.5,0.1),1),
                     labels =insert_minor_1(c("0","0.5","1.0","1.5","2.0","2.5"),4))+
  #scale_color_gdradientn(colors=(matlab.like(40)[6:34]),breaks = c(-0.5, 0, 0.5), limits = c(-0.75,0.93))+
  #scale_color_gradientn(colors=(topo.colors(20)[4:17]))+
  scale_color_gradientn(colors=c(topo.colors(40)[1:38]))+
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
        #legend.position = "right",
        #legend.direction = "vertical",
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.background = element_blank(),
  )+
  #guides(col=guide_legend(title = "",ncol = 2,override.aes = list(shape=21,size=7, fill=c("#BC3C29FF","#EFC000FF","#008B45FF","#7876B1FF"),alpha=1)), size="none")+
  guides(col=guide_colorbar(title = expression(bolditalic("ρ")),ticks.colour = "black", ticks.linewidth = 1.5,
                            barwidth = 25, barheight = 2.5))+
  ggsave(filename("vk_cor_sul_1st_SOC_rho0.3"),height = 25, width = 20, units = "cm", dpi = 300)


##Noto=====
cor_1st_sep_nt=subset(cor_1st_soc_sep_sel,cor_1st_soc_sep_sel$Grouplab=="NT")
cor_1st_sep_nt
table(cor_1st_sep_nt$Type)

cor_1st_sep_nt
cor_1st_sep_nt=cor_1st_sep_nt[order(cor_1st_sep_nt$value),]

cor_test_1st ##include insig
cor_test_1st_nt=subset(cor_test_1st,cor_test_1st$Group=="Noto") 
cor_1st_sep_nt

cor_test_1st_nt_grey=cor_test_1st_nt %>% filter(!Formula%in%c(unique(cor_1st_sep_nt$Formula)))
unique(cor_test_1st_nt_grey$Formula)

cor_1st_sep_nt2=subset(cor_1st_sep_nt,abs(cor_1st_sep_nt$value)>0.3)

ggplot()+
  geom_point(data=cor_test_1st_nt_grey, aes(x=O.C, y=H.C),col="grey70",size=2.5)+
  geom_point(data=cor_1st_sep_nt, aes(x=O.C, y=H.C,col=value),size=3.5)+
  facet_rep_grid(Grouplab~varlab, repeat.tick.labels = "all")+
  #facet_grid(Grouplab~varlab, labeller = label_parsed )+
  scale_x_continuous(name = "O/C",expand = c(0.01,0.01),limits = c(-0.01,1.0), breaks = round(seq(0,2.1,0.1),1),
                     labels =insert_minor_2(c("0","0.2","0.4","0.6","0.8","1.0","1.2","1.4","1.6","1.8","2.0","2.2"),2))+
  scale_y_continuous(name = "H/C",expand = c(0.01,0.01), limits = c(0.0,2.2),breaks = round(seq(0.0,2.5,0.1),1),
                     labels =insert_minor_1(c("0","0.5","1.0","1.5","2.0","2.5"),4))+
  #scale_color_gdradientn(colors=(matlab.like(40)[6:34]),breaks = c(-0.5, 0, 0.5), limits = c(-0.75,0.93))+
  #scale_color_gradientn(colors=(topo.colors(20)[4:17]))+
  scale_color_gradientn(colors=c(topo.colors(40)[1:38]),breaks = c(-0.8,-0.4, 0, 0.4,0.8), limits = c(-1,1))+
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
  #guides(col=guide_legend(title = "",ncol = 2,override.aes = list(shape=21,size=7, fill=c("#BC3C29FF","#EFC000FF","#008B45FF","#7876B1FF"),alpha=1)), size="none")+
  guides(col=guide_colorbar(title = expression(bolditalic("ρ")),ticks.colour = "black", ticks.linewidth = 1.5,
                            barwidth = 2.5, barheight = 25))+
  ggsave(filename("vk_cor_nt_1st_all_rho0.3"),height = 20, width = 45, units = "cm", dpi = 300)

cor_1st_sep_nt2_soc=subset(cor_1st_sep_nt2,cor_1st_sep_nt2$Type=="SOC")
cor_1st_sep_nt2_soc

table(cor_1st_sep_nt2_soc$Type)

ggplot()+
  geom_hline(yintercept = 1.5,lty=2,size=1)+
  geom_abline(slope = -1, intercept = 1.1,lty=2,size=1)+
  geom_abline(slope = -0.76, intercept = 0.75, lty=2,size=1)+
  geom_point(data=cor_test_1st_nt_grey, aes(x=O.C, y=H.C),col="grey70",size=2.5)+
  geom_point(data=cor_1st_sep_nt2_soc, aes(x=O.C, y=H.C,col=value),size=3.5)+
  facet_rep_grid(.~Grouplab, repeat.tick.labels = "all")+
  #facet_grid(Grouplab~varlab, labeller = label_parsed )+
  scale_x_continuous(name = "O/C",expand = c(0.01,0.01),limits = c(-0.01,1.0), breaks = round(seq(0,2.1,0.1),1),
                     labels =insert_minor_2(c("0","0.2","0.4","0.6","0.8","1.0","1.2","1.4","1.6","1.8","2.0","2.2"),2))+
  scale_y_continuous(name = "H/C",expand = c(0.01,0.01), limits = c(0.0,2.2),breaks = round(seq(0.0,2.5,0.1),1),
                     labels =insert_minor_1(c("0","0.5","1.0","1.5","2.0","2.5"),4))+
  #scale_color_gdradientn(colors=(matlab.like(40)[6:34]),breaks = c(-0.5, 0, 0.5), limits = c(-0.75,0.93))+
  #scale_color_gradientn(colors=(topo.colors(20)[4:17]))+
  scale_color_gradientn(colors=c(topo.colors(40)[1:38]))+
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
        #legend.position = "right",
        #legend.direction = "vertical",
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.background = element_blank(),
  )+
  #guides(col=guide_legend(title = "",ncol = 2,override.aes = list(shape=21,size=7, fill=c("#BC3C29FF","#EFC000FF","#008B45FF","#7876B1FF"),alpha=1)), size="none")+
  guides(col=guide_colorbar(title = expression(bolditalic("ρ")),ticks.colour = "black", ticks.linewidth = 1.5,
                            barwidth = 25, barheight = 2.5))+
  ggsave(filename("vk_cor_nt_1st_SOC_rho0.3"),height = 25, width = 20, units = "cm", dpi = 300)


cor_all=rbind(cor_1st_sep_ul2_soc,cor_1st_sep_bj2_soc,cor_1st_sep_ss2_soc,cor_1st_sep_sul2_soc,cor_1st_sep_nt2_soc)
frd_merge=subset(frd_merge,frd_merge$cnt>4)


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

vk_frd_soc=vk_frd %>% left_join(cor_all[,c("Group","Formula","Type","value","p")])
vk_frd_soc$Type2=ifelse(is.na(vk_frd_soc$Type),"WSOC",vk_frd_soc$Type)
vk_frd_soc_sel=subset(vk_frd_soc,vk_frd_soc$Type2=="SOC")

vk_frd_soc_sel_pos=subset(vk_frd_soc_sel,vk_frd_soc_sel$value>0)

table(vk_frd_soc_sel_pos$Group)

cnt2_soc=melt(table(vk_frd_soc_sel_pos$Group))

cnt2_soc$Grouplab=factor(cnt2_soc$Var1,
                     levels = c("Ulaanbaatar","Beijing","Seosan","Seoul","Noto"),
                     labels = c("UB","BJ","SS","SE","NT"))

cnt2_soc$pd="1st"
cnt2_soc$pdlab=factor(cnt2_soc$pd,
                  levels = c("1st","2nd","3rd"),
                  labels = c("1st (Winter)","2nd (Summer)","3rd (Spring)"))

vk_frd_soc_sel=vk_frd_soc_sel[order(vk_frd_soc_sel$value,decreasing = F),]

ggplot()+
  geom_hline(yintercept = 1.5,lty=2,size=1)+
  geom_abline(slope = -1, intercept = 1.1,lty=2,size=1)+
  geom_abline(slope = -0.76, intercept = 0.75, lty=2,size=1)+
  geom_point(data=vk_frd_soc, aes(x=O.C, y=H.C),col="grey70",size=1.2)+
  geom_point(data=vk_frd_soc_sel, aes(x=O.C, y=H.C,col=value),size=2)+
  geom_text(data=cnt2_soc, aes(x=0.15, y=0.15, label=paste0("italic('n')=","=",value)),size=9, parse = TRUE)+
  facet_rep_wrap(.~Grouplab, repeat.tick.labels = "all", ncol=3)+
  scale_x_continuous(name = "O/C",expand = c(0.01,0.01),limits = c(-0.01,1.0), breaks = round(seq(0,2.1,0.1),1),
                     labels =insert_minor_2(c("0","0.2","0.4","0.6","0.8","1.0","1.2","1.4","1.6","1.8","2.0","2.2"),2))+
  scale_y_continuous(name = "H/C",expand = c(0.01,0.01), limits = c(0.0,2.2),breaks = round(seq(0.0,2.5,0.1),1),
                     labels =insert_minor_1(c("0","0.5","1.0","1.5","2.0","2.5"),4))+
  #scale_fill_manual(values = (c("#BC3C29FF","#EFC000FF","#008B45FF","#7876B1FF","grey50")),guide="none")+
  scale_color_gradientn(colors=c(topo.colors(40)[1:38]))+
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
        #legend.position = "right",
        legend.direction = "vertical",
        legend.position = c(0.72,0.235),
        #legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.background = element_blank(),
  )+
  guides(col=guide_colorbar(title = expression(bolditalic("ρ")),ticks.colour = "black", ticks.linewidth = 1.5,
                            barwidth = 2.5, barheight = 25))+
  ggsave(filename("vk_all_soc"),height = 34, width = 48, units = "cm", dpi = 300, compression="lzw")





