soc_cor_1st=rbind(cor_1st_sep_ul2_soc,cor_1st_sep_bj2_soc,cor_1st_sep_ss2_soc,cor_1st_sep_sul2_soc,cor_1st_sep_nt2_soc)
soc_cor_1st_pos=subset(soc_cor_1st,soc_cor_1st$value>0)
table(soc_cor_1st_pos$Group)
table(soc_cor_1st_pos$Comp)

soc_cor_1st
frd_1st

fm_1st=unique(frd_1st[,c("Formula","C.","H.","N.","O.","S.","DBE","AI")])
soc_cor_1st_list=soc_cor_1st %>% left_join(fm_1st)
#fwrite(soc_cor_1st_list,file = "SOC_1st_info.csv")


frd_merge_1st=frd_merge_raw
min(frd_1st_re$cnt)

frd_1st_soc=frd_merge_1st %>% left_join(soc_cor_1st_pos[,c("Group","Formula","Type")])
frd_1st_soc_fin=subset(frd_1st_soc,frd_1st_soc$Type=="SOC")

#build transformation dataset (each sample)
frd_mole_1st=unique(frd_1st_soc_fin[,c("Calc.m.z","C.","H.","N.","O.","S.","Formula","Molecularclass","AIClass","Comp")]) %>% 
  `colnames<-`(c("Calc.m.z",c("C#","H#","N#","O#","S#","Formula","Molecular class","AI class","Comp")))
frd_mole_1st

frd_mole_1st_sel=frd_mole_1st[order(frd_mole_1st$Calc.m.z),]
#fwrite(frd_mole_1st_sel, file = "Datafile/frd_mole_1st.csv")

frd_1st_trans=frd_1st_soc_fin[,c("id","Calc.m.z","Bromo.Inty")] %>% 
  dcast(Calc.m.z~id,mean)

frd_1st_trans[is.na(frd_1st_trans)] <- 0
frd_1st_trans

frd_1st_trans2=frd_1st_trans
frd_1st_trans2=as.data.frame(frd_1st_trans2) %>% `rownames<-`(frd_1st_trans2$Calc.m.z)
frd_1st_trans2=frd_1st_trans2[order(frd_1st_trans2$Calc.m.z),]

frd_1st

##wsoc dataset=====
frd_merge_1st

frd_mole_1st=unique(frd_merge_1st[,c("Calc.m.z","C.","H.","N.","O.","S.","Formula","Molecularclass","AIClass","Comp")]) %>% 
  `colnames<-`(c("Calc.m.z",c("C#","H#","N#","O#","S#","Formula","Molecular class","AI class","Comp")))
frd_mole_1st

frd_mole_1st_sel=frd_mole_1st[order(frd_mole_1st$Calc.m.z),]
#fwrite(frd_mole_1st_sel, file = "Datafile/frd_mole_1st_wsoc.csv")


frd_1st_trans=frd_merge_1st[,c("id","Calc.m.z","Bromo.Inty")] %>% 
  dcast(Calc.m.z~id,mean)

frd_1st_trans[is.na(frd_1st_trans)] <- 0
frd_1st_trans

frd_1st_trans2=frd_1st_trans
frd_1st_trans2=as.data.frame(frd_1st_trans2) %>% `rownames<-`(frd_1st_trans2$Calc.m.z)
frd_1st_trans2=frd_1st_trans2[order(frd_1st_trans2$Calc.m.z),]

frd_1st_trans2
#fwrite(frd_1st_trans2,file = "Datafile/frd1st_data_fortrans_wsoc.csv")

data = read.csv(file = "Datafile/frd1st_data_fortrans_wsoc.csv", row.names = 1) ##mass * sample dataset

# Keeping data and mol-data seperate to ensure they are unaltered
mol = read.csv(file = "Datafile/frd_mole_1st_wsoc.csv", row.names = 1) ##mass * chemical information dataset
colnames(data) = paste("Sample_", colnames(data), sep="")

# Loading in transformations
trans.full =  read.csv("Datafile/Transformation_Database_potential.csv")
trans.full$Name = as.character(trans.full$Name)

# ############# #
#### Errors ####
# ############ #

# Checking row names consistency between molecular info and data
if(identical(x = row.names(data), y = row.names(mol)) == FALSE){
  stop("Something is incorrect: the mol. info and peak counts don't match")
}

# Checking to ensure ftmsRanalysis was run
if(length(which(mol$C13 == 1)) > 0){
  stop("Isotopic signatures weren't removed")
}

# Likely not necessary, but ensuring the data is presence/absence
if(max(data) > 1){
  print("Data was not presence/absence")
  data[data > 1] = 1
}

# Creating output directories
if(!dir.exists("Transformation Peak Comparisons")){
  dir.create("Transformation Peak Comparisons")
}

if(!dir.exists("Transformations per Peak")){
  dir.create("Transformations per Peak")
}


###########################################
### Running through the transformations ###
###########################################
Sample_Name = "WSOC_trans0.000005_ms" # This is a name that will be added to the output file (helps in tracking)

# pull out just the sample names
samples.to.process = colnames(data)

# error term
error.term = 0.000005

# matrix to hold total number of transformations for each sample
tot.trans = numeric()

# matrix to hold transformation profiles
profiles.of.trans = trans.full
head(profiles.of.trans)

for (current.sample in samples.to.process) {
  #current.sample=samples.to.process :sample file name
  print(date()) ##print date
  
  one.sample.matrix = cbind(as.numeric(as.character(row.names(data))), data[,which(colnames(data) == current.sample), drop = FALSE]) # "drop = FALSE" ensures that the row and column names remain associated with the data
  colnames(one.sample.matrix) = c("peak", colnames(one.sample.matrix[2]))
  # print(head(one.sample.matrix))
  
  one.sample.matrix %>% gather("sample", "value", -1)
  
  Sample_Peak_Mat <- one.sample.matrix %>% gather("sample", "value", -1) %>% filter(value > 0) %>% dplyr::select(sample, peak)  #sample, masslist dataframe 
  Distance_Results <- Sample_Peak_Mat %>% left_join(Sample_Peak_Mat, by = "sample") %>% filter(peak.x > peak.y) %>% mutate(Dist = peak.x - peak.y) %>% dplyr::select(sample, Dist,peak.x,peak.y)
  Distance_Results$Dist.plus = Distance_Results$Dist + error.term
  Distance_Results$Dist.minus = Distance_Results$Dist - error.term
  Distance_Results$Trans.name = -999
  head(Distance_Results)
  
  dist.unique = unique(Distance_Results[,'sample']) #unique samples (extract sample name)
  
  date()
  
  #counter = 1
  
  for (current.trans in unique(trans.full$Name)) { # note that for masses with multiple names, only the last name is going to be recorded
    
    mass.diff = trans.full$Mass[which(trans.full$Name == current.trans)] #select one transformation mass difference  
    if (length(mass.diff) > 1) { break() }
    Distance_Results$Trans.name[ which(Distance_Results$Dist.plus >= mass.diff & Distance_Results$Dist.minus <= mass.diff)  ] = current.trans ##replace -999 to transformation name 
    #print(c(counter,current.trans,mass.diff,length(mass.diff)))
    
    #counter = counter + 1
    
  }
  
  date()
  
  Distance_Results = Distance_Results[-which(Distance_Results$Trans.name == -999),] ##transformation false remove
  head(Distance_Results)
  
  # Creating directory if it doesn't exist, prior to writing the output file
  if(length(grep(Sample_Name,list.dirs("Transformation Peak Comparisons", recursive = F))) == 0){
    dir.create(paste("Transformation Peak Comparisons/", Sample_Name, sep=""))
    print("Directory created")
  }
  
  write.csv(Distance_Results,paste("Transformation Peak Comparisons/",Sample_Name,"/Peak.2.Peak_",dist.unique,".csv",sep=""),quote = F,row.names = F)
  
  # Alternative .csv writing
  # write.csv(Distance_Results,paste("Transformation Peak Comparisons/", "Peak.2.Peak_",dist.unique,".csv",sep=""),quote = F,row.names = F)
  
  # sum up the number of transformations and update the matrix
  tot.trans = rbind(tot.trans,c(dist.unique,nrow(Distance_Results)))
  
  # generate transformation profile for the sample
  trans.profile = as.data.frame(tapply(X = Distance_Results$Trans.name,INDEX = Distance_Results$Trans.name,FUN = 'length')); head(trans.profile)
  colnames(trans.profile) = dist.unique
  head(trans.profile)
  
  # update the profile matrix
  profiles.of.trans = merge(x = profiles.of.trans,y = trans.profile,by.x = "Name",by.y = 0,all.x = T)
  profiles.of.trans[is.na(profiles.of.trans[,dist.unique]),dist.unique] = 0
  head(profiles.of.trans)
  str(profiles.of.trans)
  
  # find the number of transformations each peak was associated with
  peak.stack = as.data.frame(c(Distance_Results$peak.x,Distance_Results$peak.y)); head(peak.stack)
  peak.profile = as.data.frame(tapply(X = peak.stack[,1],INDEX = peak.stack[,1],FUN = 'length' )); dim(peak.profile)
  colnames(peak.profile) = 'num.trans.involved.in'
  peak.profile$sample = dist.unique
  peak.profile$peak = row.names(peak.profile)
  head(peak.profile);
  
  # Creating directory if it doesn't exist, prior to writing the output file
  if(length(grep(Sample_Name,list.dirs("Transformations per Peak", recursive = F))) == 0){
    dir.create(paste("Transformations per Peak/", Sample_Name, sep=""))
    print("Directory created")
  }
  
  # Writing data to newly created directory
  write.csv(peak.profile,paste("Transformations per Peak/",Sample_Name,"/Num.Peak.Trans_",dist.unique,"ms.csv",sep=""),quote = F,row.names = F)
  # Alternative .csv writing
  # write.csv(peak.profile,paste("Transformations per Peak/", "Num.Peak.Trans_",dist.unique,".csv",sep=""),quote = F,row.names = F)
  print(dist.unique)
  print(date())
  
}

##number of trans=====
#flist=dir(path = "Transformations per Peak/trans0.000005_ms/", pattern = ".csv")
flist=dir(path = paste0("Backup/Transformations per Peak/",Sample_Name), pattern = ".csv")

dt=data.table()
for (i in 1:length(flist)) {
  #tmp=fread(paste0("Transformations per Peak/trans0.000005_ms/",flist[i]))
  tmp=fread(paste0("Backup/Transformations per Peak/",Sample_Name,"/",flist[i]))
  dt=rbind(dt, tmp)
}

dt
dall=dt
dall=dall %>% separate(sample, c("id","No"),sep = "\\.")
dall=dall %>% separate(id, c("Sam","Proj","Pd","Group"),sep = "_")
dall
dall_sel=dall[,c("num.trans.involved.in","Group","No","peak")]
dall_sel=dall_sel %>% `colnames<-`(c("Trans","Group","No", "m/z"))

ggplot(dall_sel, aes(x=Trans))+
  geom_histogram(binwidth = 1)+
  facet_rep_wrap(Group~.)+
  theme_bw()

tlist=dir(path = paste0("Backup/Transformation Peak Comparisons/",Sample_Name), pattern = ".csv")

tt=data.table()
for (i in 1:length(tlist)) {
  #i=1
  tmp=fread(paste0("Backup/Transformation Peak Comparisons/",Sample_Name,"/",tlist[i]))
  tt=rbind(tt, tmp)
}

tt
#tt2=tt[,c("Trans.name","Dist")]
#tt_trans=unique(tt2)
#fwrite(tt_trans, file = "tt_0.csv")

trans_merge=tt
trans_merge=trans_merge %>% separate(sample, c("id","No"),sep = "\\.")
trans_merge=trans_merge %>% separate(id, c("Sample1","Proj","Pd","Group"),sep = "_")

trans_merge$Sample=paste(trans_merge$Group,trans_merge$No,sep="_")
trans_merge2=unique(trans_merge[,c("Group","No","peak.x","peak.y","Trans.name","Sample")])
dim(trans_merge2)
trans_merge2

frd_merge_1st

trans_1st_info=frd_merge_1st[,c("Group","No","Formula","Bromo.Inty")]
trans_1st_info_m=dcast(trans_1st_info, Group+No~Formula, value.var = "Bromo.Inty",mean) %>% 
  melt(id.vars=c("Group","No"),na.rm = T)

head(trans_1st_info_m)

trans_1st_info_m$Group=ifelse(trans_1st_info_m$Group=="Ulaanbaatar",
                              "Ulan",trans_1st_info_m$Group)
trans_1st_info_m$Sample=paste(trans_1st_info_m$Group,trans_1st_info_m$No,sep = "_")

trans_1st_info_m=trans_1st_info_m %>% `colnames<-`(c("Group","No","Formula","Inty","Sample"))
trans_1st_info_m=trans_1st_info_m[,c("Sample","Group","No","Formula","Inty")]
trans_1st_info_m

trans_1st_all=dall_sel
#trans_all=trans_all %>% separate(sample, c("n","Depth","Trt","Time"))
trans_1st_all$Sample=paste(trans_1st_all$Group,trans_1st_all$No,sep = "_")

trans_1st_all=trans_1st_all %>% `colnames<-`(c("Trans","Group","No","Calc.m.z","Sample"))

frd_merge_1st
frd_merge_1st$Sample=paste(frd_merge_1st$Group,frd_merge_1st$No, sep = "_")
frd_merge_1st_trans=frd_merge_1st %>% left_join(trans_1st_all[,c("Sample", "Calc.m.z","Trans")])

dim(frd_1st_soc_fin)
dim(frd_1st_soc_trans)

frd_merge_1st_trans$Group=ifelse(frd_merge_1st_trans$Group=="Ulaanbaatar",
                                 "Ulan",frd_merge_1st_trans$Group)
table(frd_merge_1st_trans$Group)
frd_merge_1st_trans$Sample=paste(frd_merge_1st_trans$Group,frd_merge_1st_trans$No,sep = "_")
fm_info=unique(frd_merge_1st_trans[,c("Sample","Formula","Calc.m.z","AIClass")])

table(fm_info$Sample)
head(fm_info)

fm_info=fm_info %>% `colnames<-`(c("Sample","Formula","m/z","AIClass"))
trans_1st_info_m=trans_1st_info_m[,c("Sample","Formula","Inty")]

trans_1st_info_m=trans_1st_info_m %>% left_join(fm_info)
head(trans_1st_info_m)
table(trans_1st_info_m$Sample)
trans_1st_info_m=trans_1st_info_m %>% separate(Sample, c("Group","No"))

soc_cor_1st_pos$Group=ifelse(soc_cor_1st_pos$Group=="Ulaanbaatar","Ulan",soc_cor_1st_pos$Group)

trans_1st_info_m_soc=trans_1st_info_m %>% left_join(soc_cor_1st_pos[,c("Group","Formula","Type")])
trans_1st_info_m_soc
trans_1st_info_m_soc$Type2=ifelse(is.na(trans_1st_info_m_soc$Type),"WSOC","SOC")

table(trans_1st_info_m_soc$Type2)
length(unique(trans_1st_info_m_soc$Formula))
soc_cor_1st_pos

trans_1st_info_m_soc$Sample=paste(trans_1st_info_m_soc$Group,trans_1st_info_m_soc$No,sep = "_")

table(trans_1st_info_m_soc$Sample)
table(trans_merge_sel$Sample)
trans_1st_info_m_soc

trans_1st_info_x=trans_1st_info_m_soc[,c("Sample","Formula","Inty","m/z","AIClass","Type2")] %>% `colnames<-`(c("Sample","Formula_x","Inty_x","peak.x","AIClass_x","Type2_x"))
trans_1st_info_y=trans_1st_info_m_soc[,c("Sample","Formula","Inty","m/z","AIClass","Type2")] %>% `colnames<-`(c("Sample","Formula_y","Inty_y","peak.y","AIClass_y","Type2_y"))

head(trans_merge)
table(trans_1st_info_x$Type2_x)

trans_merge_sel=trans_merge[,c("Sample","Dist","peak.x","peak.y","Trans.name")]
trans_merge_sel

length(unique(trans_merge_sel$peak.x))
length(unique(trans_1st_info_x$peak.x))
length(unique(trans_merge_sel$peak.y))
length(unique(trans_1st_info_y$peak.y))

trans_merge_sel
trans_1st_info_x

trans_merge_x=trans_merge_sel %>% left_join(trans_1st_info_x)
table(trans_merge_x$Type2_x)
table(trans_merge_x$Sample)

trans_fin=trans_merge_x %>% left_join(trans_1st_info_y)
head(trans_fin)
dim(trans_fin)

table(trans_fin$Type2_y)

trans_fin=trans_fin[,c("Sample","Trans.name","Dist","peak.x","peak.y","Formula_x","Formula_y",
                       "AIClass_x","AIClass_y","Type2_x","Inty_x","Inty_y","Type2_y")]

#fwrite(trans_fin, file = "Datafile/WSOC_transbysample.csv")


trans_fin=fread("Datafile/WSOC_transbysample.csv")
trans_fin
table(trans_fin$Sample)
trans_fin$Transtype=paste(trans_fin$'Type2_x',trans_fin$'Type2_y',sep = "_")
table(trans_fin$Transtype)

trans_fin=trans_fin %>% filter(Transtype%in%c("WSOC_WSOC","WSOC_SOC","SOC_WSOC","SOC_SOC"))

##p2p data analysis=====
#fp2p=dir(path = "Transformation Peak Comparisons/trans0.000005_ms/", pattern = ".csv")
fp2p=dir(path = paste0("Backup//Transformation Peak Comparisons/",Sample_Name), pattern = ".csv")

dp2p=data.table()
for (i in 1:length(fp2p)) {
  
  tmp=fread(paste0("Backup/Transformation Peak Comparisons/",Sample_Name,"/",fp2p[i]))
  dp2p=rbind(dp2p, tmp)
  
}

dp2p_all=dp2p
dp2p_all
dp2p_all$Freq=1
dp2p_all

dp2p_all$pt=paste(dp2p_all$peak.x,dp2p_all$peak.y, sep="_")
dp2p_all

trans_fin

dim(dp2p_all)
dim(trans_fin)
table(trans_fin$Transtype)

trans_fin$Transtype=paste(trans_fin$'Type2_x',trans_fin$'Type2_y',sep = "_")
trans_fin$pt=paste(trans_fin$peak.x,trans_fin$peak.y, sep="_")

dp2p_all=dp2p_all %>% left_join(trans_fin[,c("pt","Transtype")])

trans_fin$Freq=1
head(trans_fin)
trsbyname=aggregate(trans_fin$Freq, by=list(Sample=trans_fin$Sample,Trans=trans_fin$Trans.name, Transtype=trans_fin$Transtype),sum)

#trsbyname=trsbyname %>% separate(Sample, c("Sample","Proj","Pd","Group","No"))
#trsbyname=trsbyname[,c("Group","No","Trans","x")]
#fwrite(trsbyname, file = "transbyname.csv")

table(trsbyname$Transtype)

trsbyname=trsbyname %>% separate(Sample, c("Group","No"))
trsbyname$Transtype=ifelse(trsbyname$Transtype=="SOC_WSOC","WSOC_SOC",trsbyname$Transtype)
trsbyname$Transtypelab=factor(trsbyname$Transtype, levels = c("WSOC_WSOC","WSOC_SOC","SOC_SOC"),
                           labels = c("WSOC-WSOC","WSOC-SOC","SOC-SOC"))

trsbyname
trans_mean_grp=aggregate(trsbyname$x, by=list(Group=trsbyname$Group,Trans=trsbyname$Trans, Transtype=trsbyname$Transtypelab), mean) %>% `colnames<-`(c("Group","Trans","Type","mean"))
trans_sd_grp=aggregate(trsbyname$x, by=list(Group=trsbyname$Group,Trans=trsbyname$Trans, Transtype=trsbyname$Transtypelab), sd) %>% `colnames<-`(c("Group","Trans","Type","sd"))

trans_mean_grp
trans_sd_grp

trans_mean_grp$Grouplab=factor(trans_mean_grp$Group, levels = c("Ulan","Beijing","Seosan","Seoul","Noto"),
                               labels = c("UT","BJ","SS","SE","NT"))

table(trans_mean_grp$Trans)
trans_mean_grp$tlab=factor(trans_mean_grp$Trans, levels = c("2H","C2H2O","CH2","C2H4","C2H6","C3H4","C3H6","C2H2",
                                                            "O","2O","3O","-2H+2O","H2O",
                                                            "CO2","CO (COOH-OH)","CH2O","C",
                                                            "NH","NH3-O","-NH3+2O","CH3N-O (CH3NH-OH)","CON-H (CONH-H2)","CN-H (CONH-H2O)",
                                                            "NO2-H","NO2-OH",
                                                            "SO2","SO3","SO4","SH2","S","SH-OH")
                           )

labs = c(expression(bold("-2H")),expression(bold("-C"["2"]*"H"["2"]*"O")),
         expression(bold("-CH"["2"])), expression(bold("-C"["2"]*"H"["4"])),expression(bold("-C"["2"]*"H"["6"])),
         expression(bold("-C"["3"]*"H"["4"])),expression(bold("-C"["3"]*"H"["6"])),expression(bold("-C"["2"]*"H"["2"])),
         expression(bold("+O")),expression(bold("+2O")),expression(bold("+3O")), expression(bold("-2H"*"+2O")), expression(bold("+H"["2"]*"O")),
         expression(bold("-C"*"O"["2"])), expression(bold("-COOH"*"+OH")), expression(bold("-C"*"H"["2"]*"O")),expression(bold("-C")),
         expression(bold("-NH")), expression(bold("-NH"["3"]*"+O")),expression(bold("-NH"["3"]*"+2O")),
         expression(bold("-CH"["3"]*"NH"*"+OH")),expression(bold("-CONH"*"+H"["2"])),expression(bold("-CONH"*"+H"["2"]*"O")),
         expression(bold("-NO"["2"]*"+H")),expression(bold("-NO"["2"]*"+OH")),
         expression(bold("-SO"["2"])),expression(bold("-SO"["3"])),expression(bold("-SO"["4"])),
         expression(bold("-SH"["2"])),expression(bold("-S")),expression(bold("-SH"*"+OH")))

trans_mean_grp$tlab2=factor(trans_mean_grp$Trans, levels = c("2H","C2H2O","CH2","C2H4","C2H6","C3H4","C3H6","C2H2",
                                                            "O","2O","3O","-2H+2O","H2O",
                                                            "CO2","CO (COOH-OH)","CH2O","C",
                                                            "NH","NH3-O","-NH3+2O","CH3N-O (CH3NH-OH)","CON-H (CONH-H2)","CN-H (CONH-H2O)",
                                                            "NO2-H","NO2-OH",
                                                            "SO2","SO3","SO4","SH2","S","SH-OH"),
                           labels = labs)

trans_mean_grp=trans_mean_grp[order(trans_mean_grp$tlab),]

trans_mean_grp
trans_mean_grp

trn_stat

ggplot(trans_mean_grp, aes(x=tlab, y=mean,col=Grouplab, group=Grouplab))+
  geom_segment(aes(x=tlab, xend=tlab,y=0, yend=2500), col="black")+
  #geom_hline(yintercept = 500, lty=2)+
  #geom_hline(yintercept = 1000, lty=2)+
  #geom_hline(yintercept = 1500, lty=2)+
  #geom_hline(yintercept = 2000, lty=2)+
  #annotate('text', x = 30.5, y = c(500,1000,1500,2000), label = c('500','1000','1500','2000'), size=6)+
  #geom_text(aes(x=tlab,y=2600,label=tlab2),angle=15, parse=T, col="black",size=4)+
  facet_wrap(.~Type)+
  geom_polygon(fill=NA, size=1.5)+
  geom_point(stat='identity', size=4) +
  scale_x_discrete(labels=labs)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_color_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  coord_polar(start = -pi/24*4)+
  theme_bw()+
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=0.1),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "bottom")+
  guides(col=guide_legend(title = "Sampling sites",
                          override.aes = list(fill=c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))))+
  ggsave(filename("soc_radar_trans"),height = 22, width = 60, units = "cm", dpi = 300, compression="lzw")



max(trans_mean_grp$mean)

ggplot(trans_mean_grp, aes(x=tlab, y=mean,col=Grouplab, group=Grouplab))+
  geom_segment(aes(x=tlab, xend=tlab,y=0, yend=450), col="black")+
  geom_hline(yintercept = 150, lty=2)+
 # geom_hline(yintercept = 300, lty=2)+
#  geom_hline(yintercept = 450, lty=2)+
  annotate('text', x = 30.5, y = c(150,300,450), label = c('150','300','450'), size=6)+
  geom_text(aes(x=tlab,y=180,label=tlab2),angle=15, parse=T, col="black",size=4)+
  geom_polygon(fill=NA, size=1.5)+
  geom_point(stat='identity', size=4) +
  scale_x_discrete(labels=labs)+
  scale_y_continuous(expand = c(0.02,0.02), limits = c(0,180))+
  scale_color_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  coord_polar(start = -pi/24*4,clip ="on")+
  theme_bw()+
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=0.1),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "bottom")+
  guides(col=guide_legend(title = "Sampling sites",
                          override.aes = list(fill=c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))))+
  ggsave(filename("soc_radar_trans_zoom"),height = 22, width = 20, units = "cm", dpi = 300, compression="lzw")


trans_mean_grp_interwsoc=subset(trans_mean_grp,trans_mean_grp$Type=="WSOC-WSOC")

ggplot(trans_mean_grp_interwsoc, aes(x=tlab, y=mean,col=Grouplab, group=Grouplab))+
  geom_segment(aes(x=tlab, xend=tlab,y=0, yend=2500), col="black")+
  #geom_hline(yintercept = 500, lty=2)+
  #geom_hline(yintercept = 1000, lty=2)+
  #geom_hline(yintercept = 1500, lty=2)+
  #geom_hline(yintercept = 2000, lty=2)+
  #annotate('text', x = 30.5, y = c(500,1000,1500,2000), label = c('500','1000','1500','2000'), size=6)+
  #geom_text(aes(x=tlab,y=2600,label=tlab2),angle=15, parse=T, col="black",size=4)+
  facet_wrap(.~Type)+
  geom_polygon(fill=NA, size=1.5)+
  geom_point(stat='identity', size=4) +
  scale_x_discrete(labels=labs)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_color_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  coord_polar(start = -pi/24*4)+
  theme_bw()+
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=0.1),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "bottom")+
  guides(col=guide_legend(title = "Sampling sites",
                          override.aes = list(fill=c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))))+
  ggsave(filename("soc_radar_trans_intrawsoc"),height = 22, width = 20, units = "cm", dpi = 300, compression="lzw")

trans_mean_grp_interc=subset(trans_mean_grp,trans_mean_grp$Type=="WSOC-SOC")

ggplot(trans_mean_grp_interc, aes(x=tlab, y=mean,col=Grouplab, group=Grouplab))+
  #geom_segment(aes(x=tlab, xend=tlab,y=0, yend=2500), col="black")+
  #geom_hline(yintercept = 500, lty=2)+
  #geom_hline(yintercept = 1000, lty=2)+
  #geom_hline(yintercept = 1500, lty=2)+
  #geom_hline(yintercept = 2000, lty=2)+
  #annotate('text', x = 30.5, y = c(500,1000,1500,2000), label = c('500','1000','1500','2000'), size=6)+
  #geom_text(aes(x=tlab,y=2600,label=tlab2),angle=15, parse=T, col="black",size=4)+
  facet_wrap(.~Type)+
  geom_polygon(fill=NA, size=1.5)+
  geom_point(stat='identity', size=4) +
  scale_x_discrete(labels=labs)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_color_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  coord_polar(start = -pi/24*4)+
  theme_bw()+
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=0.1),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "bottom")+
  #guides(col=guide_legend(title = "Sampling sites",
  #                        override.aes = list(fill=c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))))+
  ggsave(filename("soc_radar_trans_inter"),height = 22, width = 20, units = "cm", dpi = 300, compression="lzw")



trans_info=fread("Datafile/potential_trans_info.csv")
trans_info=trans_info %>% `colnames<-`(c("Trans","Mass","Trans_class"))
head(trans_mean_grp)

trans_mean_grp2=trans_mean_grp %>% left_join(trans_info)
trans_mean_grp2=trans_mean_grp2 %>% left_join(trans_sd_grp)


trn_stat=trn_stat %>% `colnames<-`(c("Tmp","stat","site","Trans","Grouplab"))

trans_mean_grp2=trans_mean_grp2 %>% left_join(trn_stat, by=c("Grouplab","Trans"))


trans_mean_grp2_dealkyl=subset(trans_mean_grp2,trans_mean_grp2$Trans_class=="Dealkyl group")

labs_dealkyl = c(expression(bold("-CH"["2"])), expression(bold("-C"["2"]*"H"["4"])),expression(bold("-C"["2"]*"H"["6"])),
         expression(bold("-C"["3"]*"H"["4"])),expression(bold("-C"["3"]*"H"["6"])),expression(bold("-C"["2"]*"H"["2"])))

ggplot(trans_mean_grp2_dealkyl, aes(x=tlab2,y=mean, fill=Grouplab))+
  geom_errorbar(aes(ymin=ifelse(mean-sd<0,1,mean-sd), ymax=mean+sd),position = position_dodge2(width = 0.25), size=0.2)+
  geom_bar(stat = "identity", position = position_dodge2(width = 0.25),)+
  geom_text( aes(x=tlab2,y=(mean+sd)*1.06, group=Grouplab, label=stat),position = position_dodge2(width = 0.9))+
  #scale_x_discrete(labels=labs_dealkyl)+
  scale_x_continuous(name="",breaks = c(1.2,2.2,3.2,4.2,5.2),
                     labels = labs_dealkyl,expand = c(0.02,0.02))+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_fill_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  facet_wrap(.~Trans_class)+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size=18, face = 2),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_text(size = 14, colour = "black"),
    axis.text.y = element_text(size=15, colour = "black",face = 2),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "NULL")+
  ggsave(filename("soc_trans_dealkyl"),height = 10, width = 15, units = "cm", dpi = 300, compression="lzw")

##oxygen
trans_mean_grp2_oxy=subset(trans_mean_grp2,trans_mean_grp2$Trans_class=="Oxygen addition")

labs_oxy = c(expression(bold("+O")),expression(bold("+2O")),expression(bold("+3O")), expression(bold("-2H"*"+2O")), expression(bold("+H"["2"]*"O")))

ggplot(trans_mean_grp2_oxy, aes(x=tlab2,y=mean, fill=Grouplab))+
  geom_errorbar(aes(ymin=ifelse(mean-sd<0,1,mean-sd), ymax=mean+sd),position = position_dodge2(width = 0.25), size=0.2)+
  geom_bar(stat = "identity", position = position_dodge2(width = 0.25),)+
  geom_text( aes(x=tlab2,y=(mean+sd)*1.06, group=Grouplab, label=stat),position = position_dodge2(width = 0.9))+
  scale_x_discrete(labels=labs_oxy)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_fill_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  facet_wrap(.~Trans_class)+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size=18, face = 2),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_text(size = 14, colour = "black"),
    axis.text.y = element_text(size=14, colour = "black",face = 2),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "NULL")+
  ggsave(filename("soc_trans_oxy"),height = 10, width = 15, units = "cm", dpi = 300, compression="lzw")
   
##carboxgen====
trans_mean_grp2_carbox=subset(trans_mean_grp2,trans_mean_grp2$Trans_class=="Reaction of carboxylic acid")

labs_carbox = c(expression(bold("-C"*"O"["2"])), expression(bold("-COOH"*"+OH")), expression(bold("-C"*"H"["2"]*"O")),expression(bold("-C")))

ggplot(trans_mean_grp2_carbox, aes(x=tlab2,y=mean, fill=Grouplab))+
  geom_errorbar(aes(ymin=ifelse(mean-sd<0,1,mean-sd), ymax=mean+sd),position = position_dodge2(width = 0.25), size=0.2)+
  geom_bar(stat = "identity", position = position_dodge2(width = 0.25),)+
  geom_text( aes(x=tlab2,y=(mean+sd)*1.06, group=Grouplab, label=stat),position = position_dodge2(width = 0.9))+
  scale_x_discrete(labels=labs_carbox)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_fill_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  facet_wrap(.~Trans_class)+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size=18, face = 2),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_text(size = 14, colour = "black"),
    axis.text.y = element_text(size=14, colour = "black",face = 2),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "NULL")+
  ggsave(filename("soc_trans_carbox"),height = 10, width = 15, units = "cm", dpi = 300, compression="lzw")

##amine====
trans_mean_grp2_amine=subset(trans_mean_grp2,trans_mean_grp2$Trans_class=="Reaction of amine")

labs_amine = c(expression(bold("-NH")), expression(bold("-NH"["3"]*"+O")),expression(bold("-NH"["3"]*"+2O")),
         expression(bold("-CH"["3"]*"NH"*"+OH")),expression(bold("-CONH"*"+H"["2"])),expression(bold("-CONH"*"+H"["2"]*"O")))

ggplot(trans_mean_grp2_amine, aes(x=tlab2,y=mean, fill=Grouplab))+
  geom_errorbar(aes(ymin=ifelse(mean-sd<0,1,mean-sd), ymax=mean+sd),position = position_dodge2(width = 0.25), size=0.2)+
  geom_bar(stat = "identity", position = position_dodge2(width = 0.25),)+
  geom_text( aes(x=tlab2,y=(mean+sd)*1.06, group=Grouplab, label=stat),position = position_dodge2(width = 0.9))+
  scale_x_discrete(labels=labs_amine)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_fill_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  facet_wrap(.~Trans_class)+
  theme_bw()+
  theme(
    plot.margin = unit(c(0.2,0.8,0.2,0.2),"cm"),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size=18, face = 2),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_text(size = 10, colour = "black", angle = 335, hjust = 0, vjust = 0.5),
    axis.text.y = element_text(size=14, colour = "black",face = 2),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "NULL")+
  ggsave(filename("soc_trans_amine"),height = 10, width = 15, units = "cm", dpi = 300, compression="lzw")

##reaction of NO2====
table(trans_mean_grp2$Trans_class)
trans_mean_grp2_nitro=subset(trans_mean_grp2,trans_mean_grp2$Trans_class=="Reaction of the nitro or nitroso")


labs_nitro = c(expression(bold("-NO"["2"]*"+H")),expression(bold("-NO"["2"]*"+OH")))

ggplot(trans_mean_grp2_nitro, aes(x=tlab2,y=mean, fill=Grouplab))+
  geom_errorbar(aes(ymin=ifelse(mean-sd<0,1,mean-sd), ymax=mean+sd),position = position_dodge2(width = 0.25), size=0.2)+
  geom_bar(stat = "identity", position = position_dodge2(width = 0.25),)+
  geom_text( aes(x=tlab2,y=(mean+sd)*1.06, group=Grouplab, label=stat),position = position_dodge2(width = 0.9))+
  scale_x_discrete(labels=labs_nitro)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_fill_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  facet_wrap(.~Trans_class)+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size=18, face = 2),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_text(size = 14, colour = "black"),
    axis.text.y = element_text(size=14, colour = "black",face = 2),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "NULL")+
  ggsave(filename("soc_trans_nitro"),height = 10, width = 15, units = "cm", dpi = 300, compression="lzw")

##surfur====
trans_mean_grp2_surfur=subset(trans_mean_grp2,trans_mean_grp2$Trans_class=="Reaction of sulfur")



labs_surfur = c(expression(bold("-SO"["2"])),expression(bold("-SO"["3"])),expression(bold("-SO"["4"])),
         expression(bold("-SH"["2"])),expression(bold("-S")),expression(bold("-SH"*"+OH")))

ggplot(trans_mean_grp2_surfur, aes(x=tlab2,y=mean, fill=Grouplab))+
  geom_errorbar(aes(ymin=ifelse(mean-sd<0,1,mean-sd), ymax=mean+sd),position = position_dodge2(width = 0.25), size=0.2)+
  geom_bar(stat = "identity", position = position_dodge2(width = 0.25),)+
  geom_text( aes(x=tlab2,y=(mean+sd)*1.06, group=Grouplab, label=stat),position = position_dodge2(width = 0.9))+
  scale_x_discrete(labels=labs_surfur)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_fill_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  facet_wrap(.~Trans_class)+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size=18, face = 2),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_text(size = 14, colour = "black"),
    axis.text.y = element_text(size=14, colour = "black",face = 2),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "NULL")+
  ggsave(filename("soc_trans_surfur"),height = 10, width = 15, units = "cm", dpi = 300, compression="lzw")

grp=unique(trsbyname$Group)
trn=unique(trsbyname$Trans)
trsbyname
trn_stat=data.table()
for (i in 1:length(trn)) {
#  i=1

  tmp=subset(trsbyname,trsbyname$Trans==trn[i])  
  
  kr=kruskal(tmp$x,tmp$Group,p.adj = "fdr",console = T)
  
  kt=kr$groups
  kt$site=rownames(kt)
  kt$trn=trn[i]
  
  trn_stat=rbind(trn_stat,kt)
  
}

trn_stat$Grouplab=factor(trn_stat$site, levels = c("Ulan","Beijing","Seosan","Seoul","Noto"),
                               labels = c("UT","BJ","SS","SE","NT"))
trn_stat

###event vs non-event====
env_1st=fread("Datafile/FRIEND_1st_envi_re.csv")
env_1st_ev=env_1st[,c("Group","No","Event")]
table(env_1st_ev$Group)
env_1st_ev$Group=ifelse(env_1st_ev$Group=="Ulaanbaatar","Ulan",env_1st_ev$Group)
trsbyname$No=as.numeric(trsbyname$No)
trsbyname_ev=trsbyname %>% left_join(env_1st_ev)

trans_ev_mean_grp=aggregate(trsbyname_ev$x, by=list(Group=trsbyname_ev$Group,trans=trsbyname_ev$Trans, Event=trsbyname_ev$Event), mean) %>% `colnames<-`(c("Group","Trans","Event","mean"))
trans_ev_sd_grp=aggregate(trsbyname_ev$x, by=list(Group=trsbyname_ev$Group,trans=trsbyname_ev$Trans, Event=trsbyname_ev$Event), sd) %>% `colnames<-`(c("Group","Trans","Event","sd"))

trans_ev_mean_grp
trans_ev_sd_grp

trans_ev_mean_grp$Grouplab=factor(trans_ev_mean_grp$Group, levels = c("Ulan","Beijing","Seosan","Seoul","Noto"),
                               labels = c("UT","BJ","SS","SE","NT"))

trans_ev_mean_grp_ev=trans_ev_mean_grp %>% filter(Event%in%c("Event","Non-event"))


table(trans_ev_mean_grp_ev$Trans)

trans_ev_mean_grp_ev$tlab=factor(trans_ev_mean_grp_ev$Trans, levels = c("2H","C2H2O","CH2","C2H4","C2H6","C3H4","C3H6","C2H2",
                                                            "O","2O","3O","-2H+2O","H2O",
                                                            "CO2","CO (COOH-OH)","CH2O","C",
                                                            "NH","NH3-O","-NH3+2O","CH3N-O (CH3NH-OH)","CON-H (CONH-H2)","CN-H (CONH-H2O)",
                                                            "NO2-H","NO2-OH",
                                                            "SO2","SO3","SO4","SH2","S","SH-OH")
)

trans_ev_mean_grp_ev
table(trans_ev_mean_grp_ev$tlab)

labs = c(expression(bold("-2H")),expression(bold("-C"["2"]*"H"["2"]*"O")),
         expression(bold("-CH"["2"])), expression(bold("-C"["2"]*"H"["4"])),expression(bold("-C"["2"]*"H"["6"])),
         expression(bold("-C"["3"]*"H"["4"])),expression(bold("-C"["3"]*"H"["6"])),expression(bold("-C"["2"]*"H"["2"])),
         expression(bold("+O")),expression(bold("+2O")),expression(bold("+3O")), expression(bold("-2H"*"+2O")), expression(bold("+H"["2"]*"O")),
         expression(bold("-C"*"O"["2"])), expression(bold("-COOH"*"+OH")), expression(bold("-C"*"H"["2"]*"O")),expression(bold("-C")),
         expression(bold("-NH")), expression(bold("-NH"["3"]*"+O")),expression(bold("-NH"["3"]*"+2O")),
         expression(bold("-CH"["3"]*"NH"*"+OH")),expression(bold("-CONH"*"+H"["2"])),expression(bold("-CONH"*"+H"["2"]*"O")),
         expression(bold("-NO"["2"]*"+H")),expression(bold("-NO"["2"]*"+OH")),
         expression(bold("-SO"["2"])),expression(bold("-SO"["3"])),expression(bold("-SO"["4"])),
         expression(bold("-SH"["2"])),expression(bold("-S")),expression(bold("-SH"*"+OH")))

trans_ev_mean_grp_ev$tlab2=factor(trans_ev_mean_grp_ev$Trans, levels = c("2H","C2H2O","CH2","C2H4","C2H6","C3H4","C3H6","C2H2",
                                                             "O","2O","3O","-2H+2O","H2O",
                                                             "CO2","CO (COOH-OH)","CH2O","C",
                                                             "NH","NH3-O","-NH3+2O","CH3N-O (CH3NH-OH)","CON-H (CONH-H2)","CN-H (CONH-H2O)",
                                                             "NO2-H","NO2-OH",
                                                             "SO2","SO3","SO4","SH2","S","SH-OH"),
                            labels = labs)

trans_ev_mean_grp_ev=trans_ev_mean_grp_ev[order(trans_ev_mean_grp_ev$tlab),]

trans_ev_mean_grp_ev
trn_stat
head(trans_ev_mean_grp_ev)

#ut====
trans_ev_mean_grp_ev_ut=subset(trans_ev_mean_grp_ev,trans_ev_mean_grp_ev$Grouplab=="UT")
ggplot(trans_ev_mean_grp_ev_ut, aes(x=tlab, y=mean,col=Event, group=Event))+
  geom_segment(aes(x=tlab, xend=tlab,y=0, yend=450), col="black")+
  geom_hline(yintercept = 150, lty=2)+
  geom_hline(yintercept = 300, lty=2)+
  geom_hline(yintercept = 450, lty=2)+
  annotate('text', x = 30.5, y = c(150,300,450), label = c('150','300','450'), size=6)+
  geom_text(aes(x=tlab,y=480,label=tlab2),angle=15, parse=T, col="black",size=4)+
  geom_polygon(fill=NA, size=1.5)+
  geom_point(stat='identity', size=4) +
  #scale_x_discrete(labels=labs)+
  scale_y_continuous(expand = c(0.02,0.02))+
  #scale_color_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  scale_color_manual(values = c("#F99417","grey70"))+
  coord_polar(start = -pi/24*4)+
  #facet_wrap(Grouplab~.)+
  theme_bw()+
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=0.1),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "bottom")+
 # guides(col=guide_legend(title = "Sampling sites",
  #                        override.aes = list(fill=c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))))+
  ggsave(filename("soc_radar_trans_ev_ut"),height = 22, width = 20, units = "cm", dpi = 300, compression="lzw")

#bj====
trans_ev_mean_grp_ev_bj=subset(trans_ev_mean_grp_ev,trans_ev_mean_grp_ev$Grouplab=="BJ")

ggplot(trans_ev_mean_grp_ev_bj, aes(x=tlab, y=mean,col=Event, group=Event))+
  geom_segment(aes(x=tlab, xend=tlab,y=0, yend=450), col="black")+
  geom_hline(yintercept = 150, lty=2)+
  geom_hline(yintercept = 300, lty=2)+
  geom_hline(yintercept = 450, lty=2)+
  annotate('text', x = 30.5, y = c(150,300,450), label = c('150','300','450'), size=6)+
  geom_text(aes(x=tlab,y=480,label=tlab2),angle=15, parse=T, col="black",size=4)+
  geom_polygon(fill=NA, size=1.5)+
  geom_point(stat='identity', size=4) +
  #scale_x_discrete(labels=labs)+
  scale_y_continuous(expand = c(0.02,0.02))+
  #scale_color_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  scale_color_manual(values = c("#F99417","grey70"))+
  coord_polar(start = -pi/24*4)+
  #facet_wrap(Grouplab~.)+
  theme_bw()+
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=0.1),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "bottom")+
  # guides(col=guide_legend(title = "Sampling sites",
  #                        override.aes = list(fill=c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))))+
  ggsave(filename("soc_radar_trans_ev_bj"),height = 22, width = 20, units = "cm", dpi = 300, compression="lzw")


#ss====
trans_ev_mean_grp_ev_ss=subset(trans_ev_mean_grp_ev,trans_ev_mean_grp_ev$Grouplab=="SS")

ggplot(trans_ev_mean_grp_ev_ss, aes(x=tlab, y=mean,col=Event, group=Event))+
  geom_segment(aes(x=tlab, xend=tlab,y=0, yend=450), col="black")+
  geom_hline(yintercept = 150, lty=2)+
  geom_hline(yintercept = 300, lty=2)+
  geom_hline(yintercept = 450, lty=2)+
  annotate('text', x = 30.5, y = c(150,300,450), label = c('150','300','450'), size=6)+
  geom_text(aes(x=tlab,y=480,label=tlab2),angle=15, parse=T, col="black",size=4)+
  geom_polygon(fill=NA, size=1.5)+
  geom_point(stat='identity', size=4) +
  #scale_x_discrete(labels=labs)+
  scale_y_continuous(expand = c(0.02,0.02))+
  #scale_color_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  scale_color_manual(values = c("#F99417","grey70"))+
  coord_polar(start = -pi/24*4)+
  #facet_wrap(Grouplab~.)+
  theme_bw()+
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=0.1),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "bottom")+
  # guides(col=guide_legend(title = "Sampling sites",
  #                        override.aes = list(fill=c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))))+
  ggsave(filename("soc_radar_trans_ev_ss"),height = 22, width = 20, units = "cm", dpi = 300, compression="lzw")

#se====
trans_ev_mean_grp_ev_se=subset(trans_ev_mean_grp_ev,trans_ev_mean_grp_ev$Grouplab=="SE")

ggplot(trans_ev_mean_grp_ev_se, aes(x=tlab, y=mean,col=Event, group=Event))+
  geom_segment(aes(x=tlab, xend=tlab,y=0, yend=450), col="black")+
  geom_hline(yintercept = 150, lty=2)+
  geom_hline(yintercept = 300, lty=2)+
  geom_hline(yintercept = 450, lty=2)+
  annotate('text', x = 30.5, y = c(150,300,450), label = c('150','300','450'), size=6)+
  geom_text(aes(x=tlab,y=480,label=tlab2),angle=15, parse=T, col="black",size=4)+
  geom_polygon(fill=NA, size=1.5)+
  geom_point(stat='identity', size=4) +
  #scale_x_discrete(labels=labs)+
  scale_y_continuous(expand = c(0.02,0.02))+
  #scale_color_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  scale_color_manual(values = c("#F99417","grey70"))+
  coord_polar(start = -pi/24*4)+
  #facet_wrap(Grouplab~.)+
  theme_bw()+
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=0.1),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "bottom")+
  # guides(col=guide_legend(title = "Sampling sites",
  #                        override.aes = list(fill=c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))))+
  ggsave(filename("soc_radar_trans_ev_se"),height = 22, width = 20, units = "cm", dpi = 300, compression="lzw")


#nt====
trans_ev_mean_grp_ev_nt=subset(trans_ev_mean_grp_ev,trans_ev_mean_grp_ev$Grouplab=="NT")

ggplot(trans_ev_mean_grp_ev_nt, aes(x=tlab, y=mean,col=Event, group=Event))+
  geom_segment(aes(x=tlab, xend=tlab,y=0, yend=150), col="black")+
  geom_hline(yintercept = 50, lty=2)+
  geom_hline(yintercept = 100, lty=2)+
  geom_hline(yintercept = 150, lty=2)+
  annotate('text', x = 30.5, y = c(50,100,150), label = c('50','100','150'), size=6)+
  geom_text(aes(x=tlab,y=160,label=tlab2),angle=15, parse=T, col="black",size=4)+
  geom_polygon(fill=NA, size=1.5)+
  geom_point(stat='identity', size=4) +
  #scale_x_discrete(labels=labs)+
  scale_y_continuous(expand = c(0.02,0.02))+
  #scale_color_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  scale_color_manual(values = c("#F99417","grey70"))+
  coord_polar(start = -pi/24*4)+
  #facet_wrap(Grouplab~.)+
  theme_bw()+
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=0.1),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "bottom")+
  # guides(col=guide_legend(title = "Sampling sites",
  #                        override.aes = list(fill=c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))))+
  ggsave(filename("soc_radar_trans_ev_nt"),height = 22, width = 20, units = "cm", dpi = 300, compression="lzw")


trans_info

trans_ev_mean_grp2=trans_ev_mean_grp %>% left_join(trans_info)
trans_ev_mean_grp2=trans_ev_mean_grp2 %>% left_join(trans_ev_sd_grp)

grp=unique(trsbyname$Group)
trn=unique(trsbyname$Trans)

trsbyname_ev_fin=trsbyname_ev %>% filter(Event%in%c("Event","Non-event"))
trsbyname_ev_fin

trn_stat_ev=data.table()
for (i in 1:length(trn)) {
  #i=1

  tmp=subset(trsbyname_ev_fin,trsbyname_ev_fin$Trans==trn[i])  
  
  for (j in 1:length(grp)) {
   # j=1
    tmp2=subset(tmp,tmp$Group==grp[j])
    
    tmp_ev=subset(tmp2,tmp2$Event=="Event")
    tmp_nev=subset(tmp2,tmp2$Event=="Non-event")
    
    if(length(unique(tmp_nev$x))<3){
      s1$p.value=1
    }else {
      s1=(shapiro.test(tmp_ev$x))
    }
    
    if(length(unique(tmp_nev$x))<3){
      s2$p.value=1
    }else {
      s2=(shapiro.test(tmp_nev$x))
    }
    
    #tt=t.test(tmp_ev$x,tmp_nev$x,var.equal = F)
    wt=wilcox.test(tmp_ev$x,tmp_nev$x,exact = F)
    
    new=data.table(Group=grp[j],Trans=trn[i],sp1=round(s1$p.value,3),sp2=round(s2$p.value,3),
                   #tt=round(tt$p.value,3),
                   wt=round(wt$p.value,3))
    
    trn_stat_ev=rbind(trn_stat_ev,new)
    
  }
  
}
#trn_stat_ev
trn_stat_tt=subset(trn_stat_ev,trn_stat_ev$wt<0.05)
trn_stat_tt

trn_stat$Grouplab=factor(trn_stat$site, levels = c("Ulan","Beijing","Seosan","Seoul","Noto"),
                               labels = c("UT","BJ","SS","SE","NT"))


trn_stat=trn_stat %>% `colnames<-`(c("Tmp","stat","site","trans_ev","Grouplab"))

trans_ev_mean_grp2=trans_ev_mean_grp2 %>% left_join(trn_stat, by=c("Grouplab","trans_ev"))


trans_ev_mean_grp2_dealkyl=subset(trans_ev_mean_grp2,trans_ev_mean_grp2$trans_ev_class=="Dealkyl group")

labs_dealkyl = c(expression(bold("-CH"["2"])), expression(bold("-C"["2"]*"H"["4"])),expression(bold("-C"["2"]*"H"["6"])),
                 expression(bold("-C"["3"]*"H"["4"])),expression(bold("-C"["3"]*"H"["6"])),expression(bold("-C"["2"]*"H"["2"])))

ggplot(trans_ev_mean_grp2_dealkyl, aes(x=tlab2,y=mean, fill=Grouplab))+
  geom_errorbar(aes(ymin=ifelse(mean-sd<0,1,mean-sd), ymax=mean+sd),position = position_dodge2(width = 0.25), size=0.2)+
  geom_bar(stat = "identity", position = position_dodge2(width = 0.25),)+
  geom_text( aes(x=tlab2,y=(mean+sd)*1.06, group=Grouplab, label=stat),position = position_dodge2(width = 0.9))+
  scale_x_discrete(labels=labs_dealkyl)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_fill_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  facet_wrap(.~trans_ev_class)+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size=18, face = 2),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_text(size = 14, colour = "black"),
    axis.text.y = element_text(size=15, colour = "black",face = 2),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "NULL")+
  ggsave(filename("soc_trans_ev_dealkyl"),height = 10, width = 15, units = "cm", dpi = 300, compression="lzw")

##oxygen
trans_ev_mean_grp2_oxy=subset(trans_ev_mean_grp2,trans_ev_mean_grp2$trans_ev_class=="Oxygen addition")

labs_oxy = c(expression(bold("+O")),expression(bold("+2O")),expression(bold("+3O")), expression(bold("-2H"*"+2O")), expression(bold("+H"["2"]*"O")))

ggplot(trans_ev_mean_grp2_oxy, aes(x=tlab2,y=mean, fill=Grouplab))+
  geom_errorbar(aes(ymin=ifelse(mean-sd<0,1,mean-sd), ymax=mean+sd),position = position_dodge2(width = 0.25), size=0.2)+
  geom_bar(stat = "identity", position = position_dodge2(width = 0.25),)+
  geom_text( aes(x=tlab2,y=(mean+sd)*1.06, group=Grouplab, label=stat),position = position_dodge2(width = 0.9))+
  scale_x_discrete(labels=labs_oxy)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_fill_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  facet_wrap(.~trans_ev_class)+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size=18, face = 2),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_text(size = 14, colour = "black"),
    axis.text.y = element_text(size=14, colour = "black",face = 2),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "NULL")+
  ggsave(filename("soc_trans_ev_oxy"),height = 10, width = 15, units = "cm", dpi = 300, compression="lzw")

##carboxgen====
trans_ev_mean_grp2_carbox=subset(trans_ev_mean_grp2,trans_ev_mean_grp2$trans_ev_class=="Reaction of carboxylic acid")

labs_carbox = c(expression(bold("-C"*"O"["2"])), expression(bold("-COOH"*"+OH")), expression(bold("-C"*"H"["2"]*"O")),expression(bold("-C")))

ggplot(trans_ev_mean_grp2_carbox, aes(x=tlab2,y=mean, fill=Grouplab))+
  geom_errorbar(aes(ymin=ifelse(mean-sd<0,1,mean-sd), ymax=mean+sd),position = position_dodge2(width = 0.25), size=0.2)+
  geom_bar(stat = "identity", position = position_dodge2(width = 0.25),)+
  geom_text( aes(x=tlab2,y=(mean+sd)*1.06, group=Grouplab, label=stat),position = position_dodge2(width = 0.9))+
  scale_x_discrete(labels=labs_carbox)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_fill_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  facet_wrap(.~trans_ev_class)+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size=18, face = 2),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_text(size = 14, colour = "black"),
    axis.text.y = element_text(size=14, colour = "black",face = 2),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "NULL")+
  ggsave(filename("soc_trans_ev_carbox"),height = 10, width = 15, units = "cm", dpi = 300, compression="lzw")

##amine====
trans_ev_mean_grp2_amine=subset(trans_ev_mean_grp2,trans_ev_mean_grp2$trans_ev_class=="Reaction of amine")

labs_amine = c(expression(bold("-NH")), expression(bold("-NH"["3"]*"+O")),expression(bold("-NH"["3"]*"+2O")),
               expression(bold("-CH"["3"]*"NH"*"+OH")),expression(bold("-CONH"*"+H"["2"])),expression(bold("-CONH"*"+H"["2"]*"O")))

ggplot(trans_ev_mean_grp2_amine, aes(x=tlab2,y=mean, fill=Grouplab))+
  geom_errorbar(aes(ymin=ifelse(mean-sd<0,1,mean-sd), ymax=mean+sd),position = position_dodge2(width = 0.25), size=0.2)+
  geom_bar(stat = "identity", position = position_dodge2(width = 0.25),)+
  geom_text( aes(x=tlab2,y=(mean+sd)*1.06, group=Grouplab, label=stat),position = position_dodge2(width = 0.9))+
  scale_x_discrete(labels=labs_amine)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_fill_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  facet_wrap(.~trans_ev_class)+
  theme_bw()+
  theme(
    plot.margin = unit(c(0.2,0.8,0.2,0.2),"cm"),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size=18, face = 2),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_text(size = 10, colour = "black", angle = 335, hjust = 0, vjust = 0.5),
    axis.text.y = element_text(size=14, colour = "black",face = 2),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "NULL")+
  ggsave(filename("soc_trans_ev_amine"),height = 10, width = 15, units = "cm", dpi = 300, compression="lzw")

##reaction of NO2====
table(trans_ev_mean_grp2$trans_ev_class)
trans_ev_mean_grp2_nitro=subset(trans_ev_mean_grp2,trans_ev_mean_grp2$trans_ev_class=="Reaction of the nitro or nitroso")


labs_nitro = c(expression(bold("-NO"["2"]*"+H")),expression(bold("-NO"["2"]*"+OH")))

ggplot(trans_ev_mean_grp2_nitro, aes(x=tlab2,y=mean, fill=Grouplab))+
  geom_errorbar(aes(ymin=ifelse(mean-sd<0,1,mean-sd), ymax=mean+sd),position = position_dodge2(width = 0.25), size=0.2)+
  geom_bar(stat = "identity", position = position_dodge2(width = 0.25),)+
  geom_text( aes(x=tlab2,y=(mean+sd)*1.06, group=Grouplab, label=stat),position = position_dodge2(width = 0.9))+
  scale_x_discrete(labels=labs_nitro)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_fill_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  facet_wrap(.~trans_ev_class)+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size=18, face = 2),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_text(size = 14, colour = "black"),
    axis.text.y = element_text(size=14, colour = "black",face = 2),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "NULL")+
  ggsave(filename("soc_trans_ev_nitro"),height = 10, width = 15, units = "cm", dpi = 300, compression="lzw")

##surfur====
trans_ev_mean_grp2_surfur=subset(trans_ev_mean_grp2,trans_ev_mean_grp2$trans_ev_class=="Reaction of sulfur")

labs_surfur = c(expression(bold("-NH")), expression(bold("-NH"["3"]*"+O")),expression(bold("-NH"["3"]*"+2O")),
                expression(bold("-CH"["3"]*"NH"*"+OH")),expression(bold("-CONH"*"+H"["2"])),expression(bold("-CONH"*"+H"["2"]*"O")))


labs_surfur = c(expression(bold("-SO"["2"])),expression(bold("-SO"["3"])),expression(bold("-SO"["4"])),
                expression(bold("-SH"["2"])),expression(bold("-S")),expression(bold("-SH"*"+OH")))

ggplot(trans_ev_mean_grp2_surfur, aes(x=tlab2,y=mean, fill=Grouplab))+
  geom_errorbar(aes(ymin=ifelse(mean-sd<0,1,mean-sd), ymax=mean+sd),position = position_dodge2(width = 0.25), size=0.2)+
  geom_bar(stat = "identity", position = position_dodge2(width = 0.25),)+
  geom_text( aes(x=tlab2,y=(mean+sd)*1.06, group=Grouplab, label=stat),position = position_dodge2(width = 0.9))+
  scale_x_discrete(labels=labs_surfur)+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_fill_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF","#F39B7FFF"))+
  facet_wrap(.~trans_ev_class)+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size=18, face = 2),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_text(size = 14, colour = "black"),
    axis.text.y = element_text(size=14, colour = "black",face = 2),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "NULL")+
  ggsave(filename("soc_trans_ev_surfur"),height = 10, width = 15, units = "cm", dpi = 300, compression="lzw")


