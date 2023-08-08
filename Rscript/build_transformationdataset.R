Sample_Name = "WSOC_trans0.000005_ms" # This is a name that will be added to the output file (helps in tracking)


soc_cor_1st=rbind(cor_1st_sep_ul2_soc,cor_1st_sep_bj2_soc,cor_1st_sep_ss2_soc,cor_1st_sep_sul2_soc,cor_1st_sep_nt2_soc)
soc_cor_1st_pos=subset(soc_cor_1st,soc_cor_1st$value>0)
table(soc_cor_1st_pos$Group)
table(soc_cor_1st_pos$Comp)

soc_cor_1st
frd_1st=frd_merge

fm_1st=unique(frd_1st[,c("Formula","C.","H.","N.","O.","S.","DBE","AI")])
soc_cor_1st_list=soc_cor_1st %>% left_join(fm_1st)
#fwrite(soc_cor_1st_list,file = "SOC_1st_info.csv")

frd_merge=subset(frd_merge,frd_merge$cnt>4)
frd_merge_1st=frd_merge
min(frd_merge_1st$cnt)

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

table(frd_merge_1st_trans$Group)
frd_merge_1st_trans$Group=ifelse(frd_merge_1st_trans$Group=="Ulaanbaatar",
                                 "Ulan",frd_merge_1st_trans$Group)

frd_merge_1st_trans$Sample=paste(frd_merge_1st_trans$Group,frd_merge_1st_trans$No,sep = "_")
fm_info=unique(frd_merge_1st_trans[,c("Sample","Formula","Calc.m.z","AIClass")])

table(fm_info$Sample)
head(fm_info)

fm_info=unique(fm_info[,c("Formula","Calc.m.z","AIClass")]) %>% `colnames<-`(c("Formula","m/z","AIClass"))
trans_1st_info_m=trans_1st_info_m[,c("Sample","Formula","Inty")]

trans_1st_info_m=trans_1st_info_m %>% left_join(fm_info[,c("Formula","m/z","AIClass")])
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

#fwrite(trans_fin, file = "Datafile/WSOC_transbysample_new.csv")

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
#fwrite(dp2p_all, file = "Datafile/trans_sample.csv")



