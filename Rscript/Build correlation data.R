#build correlation======
frd_merge

FT_m_3rd=melt(frd_3rd[,c("Sample","Formula","Bromo.Inty")],
          id.vars = c("Sample","Formula")) %>% 
  dcast(Sample~Formula, value.var = "value",sum)

FT_m_3rd[,1:23]

FT_envi_3rd=fread("Datafile/FRIEND_3rd_envi.csv")

FT_envi_3rd$WSOCbb=2.94*FT_envi_3rd$Levo/1000
FT_envi_3rd$WSOCnbb=FT_envi_3rd$WSOC-FT_envi_3rd$WSOCbb

FT_envi_3rd$WISOC=FT_envi_3rd$OC-FT_envi_3rd$WSOC

FT_envi_3rd$POC=FT_envi_3rd$WSOCbb+FT_envi_3rd$WISOC
FT_envi_3rd$SOC=FT_envi_3rd$WSOCnbb

FT_envi_3rd$`SOC(%)`=FT_envi_3rd$WSOCnbb/FT_envi_3rd$OC
FT_envi_3rd$`POC(%)`=1-FT_envi_3rd$`SOC(%)`

FT_envi_3rd
FT_envi_3rd$Sam=paste("FRIEND","3rd",FT_envi_3rd$Group,sep = "_")
FT_envi_3rd$Sample=paste(FT_envi_3rd$Sam,FT_envi_3rd$No,sep = "-")

FT_envi_sel=FT_envi_3rd[,c("Sample","Group","POC","SOC")]
FT_m_3rd[,1:23]

FT_envi_sel2=FT_envi_sel %>% inner_join(FT_m_3rd)
FT_envi_sel2[,1:23]

grp=unique(FT_envi_sel2$Group)

cor_temp=FT_envi_sel2
cor_temp[,1:23]

df=data.frame()
for (i in 1:length(grp)) {
  # i=1
  temp=as.data.frame(subset(cor_temp,cor_temp$Group==grp[i]))
  #temp[,1:23]
  for (j in 3:4) {
    #j=4
    for (k in 5:dim(temp)[2]) {
      #k=5  
      sum.j<-sum(temp[,j],na.rm = T)
      sum.k<-sum(temp[,k],na.rm = T)
      if(sum.j >0 & sum.k >0){
        test<-cor.test(temp[,j],temp[,k],method="spearman",na.action=na.rm,exact = F)
        rho<-test$estimate
        p.value<-test$p.value
      }
      
      if(sum.j <=0 | sum.k <= 0){
        rho<-0
        p.value<-1
      }	
      
      new=data.frame("Envi"=names(temp)[j],"Formula"=names(temp)[k],"rho"=round(rho,3),
                     "p"=round(p.value,4), "Group"=grp[i])
      
      df<-rbind(df,new)			
      
    }
    print(i/length(grp))
    
  }
  fwrite(df, file = "cortest.csv")
}

df
table(df$Envi)
#fwrite(df, file = "cortest.csv")
