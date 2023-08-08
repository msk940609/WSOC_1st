
ft_1st=fread("Datafile/FRIEND_1st.csv")

frd_merge=rbind(ft_1st)
frd_merge$id=frd_merge$Sample
frd_merge=frd_merge %>% separate(Sample, c("Proj","pd","smp"),sep = "_")
frd_merge=frd_merge %>% separate(smp, c("Group","No"), sep = "-")
frd_merge$Group=ifelse(frd_merge$Group=="Ulan","Ulaanbaatar",frd_merge$Group)
frd_merge$Freq=1

#fwrite(frd_merge, file = "Datafile/FRIEND_all.csv")
#frd_merge$samp=paste(frd_merge$Group,frd_merge$pd, sep="_")
#tt=as.data.frame(unique(frd_merge[,c("id","Group","pd","No")]))
#tt$Freq=1
#ttt=aggregate(tt$Freq, by=list(Group=tt$Group, pd=tt$pd),sum)

tt=aggregate(frd_merge$Freq,by=list(Group=frd_merge$Group,pd=frd_merge$pd, Formula=frd_merge$Formula),sum) %>% `colnames<-`(c("Group","pd","Formula","cnt"))

frd_merge=frd_merge %>% inner_join(tt)
#frd_merge_raw=frd_merge
min(frd_merge$cnt)

frd_merge_sel=subset(frd_merge,frd_merge$cnt>4)

frd_merge
frd_merge$lipid=ifelse(frd_merge$`O.C`>=0&frd_merge$`O.C`<0.3,
                       ifelse(frd_merge$`H.C`>1.5&frd_merge$`H.C`<2.0, "Lipids",""),"")
frd_merge$protein=ifelse(frd_merge$`O.C`>=0.3&frd_merge$`O.C`<=0.67,
                         ifelse(frd_merge$`H.C`>1.5&frd_merge$`H.C`<2.2,
                                ifelse(frd_merge$`N.C`>=0.05,"Proteins","") ,""),"")
frd_merge$carbo=ifelse(frd_merge$`O.C`>0.67&frd_merge$`O.C`<2.0,
                       ifelse(frd_merge$`H.C`>1.5&frd_merge$`H.C`<2.3, "Carbohydrates",""),"")
frd_merge$Unhydro=ifelse(frd_merge$`O.C`>0&frd_merge$`O.C`<0.1,
                         ifelse(frd_merge$`H.C`>=0.7&frd_merge$`H.C`<=1.5, "Unsaturated hydrocarbons",""),"")
frd_merge$lignin=ifelse(frd_merge$`O.C`>=0.1&frd_merge$`O.C`<=0.67,
                        ifelse(frd_merge$`H.C`>=0.7&frd_merge$`H.C`<=1.5, 
                               ifelse(frd_merge$AI<0.67, "Lignins",""),""),"")
frd_merge$tannin=ifelse(frd_merge$`O.C`>0.67&frd_merge$`O.C`<2.0,
                        ifelse(frd_merge$`H.C`>=0.5&frd_merge$`H.C`<=1.5, 
                               ifelse(frd_merge$AI<0.67, "Tannins",""),""),"")
frd_merge$conaro=ifelse(frd_merge$`O.C`>=0&frd_merge$`O.C`<=0.67,
                        ifelse(frd_merge$`H.C`>=0.2&frd_merge$`H.C`<0.67, 
                               ifelse(frd_merge$AI>=0.67, "Condensed aromatics",""),""),"")

frd_merge=frd_merge %>%  unite("Molecularclass",c("lipid","protein","carbo","Unhydro","lignin","tannin","conaro"), sep = "")
frd_merge$Molecularclass=ifelse(frd_merge$Molecularclass=="","Unassigned", frd_merge$Molecularclass) ###colum of molecular class


frd_merge$DBEAI=1+frd_merge$`C.`-0.5*frd_merge$`O.`-frd_merge$`S.`-0.5*frd_merge$`N.`-0.5*frd_merge$`H.`
frd_merge$CAI=frd_merge$`C.`-0.5*frd_merge$`O.`-frd_merge$`S.`-frd_merge$`N.`
frd_merge$AI=ifelse(frd_merge$DBEAI/frd_merge$CAI<0,0,frd_merge$DBEAI/frd_merge$CAI)

frd_merge$PCA=ifelse(frd_merge$AI>0.66, "Polycyclic condensed aromatics","")
frd_merge$aroma=ifelse(frd_merge$AI>0.5,ifelse(frd_merge$AI<=0.66,"Polyphenolic",""),"")
frd_merge$humic=ifelse(frd_merge$AI<=0.5,
                       ifelse(frd_merge$`H.C`<1.5,"Highly unsaturated and phenolic",""),"")
frd_merge$aliphatic=ifelse(frd_merge$`H.C`<2.0&frd_merge$`H.C`>=1.5,
                           ifelse(frd_merge$AI<=0.5,"Aliphatic compounds",""),"")

frd_merge=frd_merge %>%  unite("AIClass",c("PCA","aroma","humic","aliphatic"), sep = "")

frd_merge$AIClass=ifelse(frd_merge$AIClass=="","Saturated compounds", frd_merge$AIClass) ###colum of molecular class
table(frd_merge$AIClass)

frd_merge$AIClass=factor(frd_merge$AIClass, levels = c("Polycyclic condensed aromatics","Polyphenolic","Highly unsaturated and phenolic",
                                                       "Aliphatic compounds","Saturated compounds"))

frd_merge=frd_merge %>% mutate(Comp= gsub('[[:digit:]]', '', Formula))
frd_merge$Comp=ifelse(frd_merge$O.==0,"Remainders",frd_merge$Comp)

frd_merge$Comp=factor(frd_merge$Comp,levels = c("CHO","CHNO","CHOS","CHNOS","Remainders"),
                      labels = c("CHO","CHON","CHOS","CHONS","Remainders"))
frd_merge

fm_list_all=unique(frd_merge[,c("Formula","AIClass","Comp")])

frd_fm_intyavg=melt(frd_merge[,c("Group","pd","No","Formula","Bromo.Inty")]) %>% 
  dcast(pd+Group~Formula, value.var = "value", mean) %>% 
  melt(id.vars=c("Group","pd"),na.rm = T) %>% `colnames<-`(c("Group","pd","Formula","Inty"))

head(frd_fm_intyavg)

fm_list_all2=frd_fm_intyavg %>% left_join(fm_list_all)
fm_list_all2

frd_merge$Bromo.Inty=ifelse(frd_merge$pd=="1st",frd_merge$Bromo.Inty*10000,frd_merge$Bromo.Inty)
frd_merge
#frd_merge_raw=frd_merge
min(frd_merge$cnt)

