dat0<-read.csv("/Users/xerxesseposo/Documents/E Co consultancy/Togo GIZ/priority disease identification/eligible.csv",header=T)
names(dat0)

dat1<-dat0[,c(16,17,18,20,21)]

## delete the no value cells in the data frame
dat1<-dat1[-which(dat1$Health.outcome==""),]

## unique number of eligible studies
length(unique(dat1$doi.link)) ## 179 studies

library(dplyr)

tab0<-as.data.frame(dat1 %>%
                      group_by(Health.outcome,Hazard.s,Population) %>%
                      summarise(n=n()))


##install.packages("ggalluvial")
library(ggalluvial)

tiff("/Users/xerxesseposo/Documents/E Co consultancy/Togo GIZ/priority disease identification/togotest.tif",width=80,height = 50,units="in",compression="lzw",res=300)
ggplot(tab0,
       aes(y=n,axis1=Hazard.s,axis2=Health.outcome,axis3=Population))+
  geom_alluvium(aes(fill=Hazard.s),width=1/12)+
  geom_stratum()+
  geom_text(stat="stratum",
            aes(label=after_stat(stratum),size=3))+
  scale_x_discrete(limits=c("Hazard","Health outcome","Population"),
                   expand = c(0.15,0.05))+
  theme_void()
dev.off()

## summarize the health outcome being identified
tab0a<-as.data.frame(dat1 %>%
                       group_by(Health.outcome) %>%
                       summarise(n=n()))

tab0a[order(tab0a$n,decreasing = T),]

## chart agains health outcome and hazard (checking the amount of evidence per combination)
tab3<-table(dat1$Health.outcome,dat1$Hazard.s)
tab3<-data.frame(tab3)


## locations

list0<-data.frame(dat0$Location)
list0a<-list()
for(i in 1:nrow(list0)){
  cat(i,"")
  list0a[[i]]<-rep(dat0[i,]$doi.link,length(unlist(strsplit(list0[i,],","))))
  
}

ref<-data.frame(ref=unlist(list0a))


list1<-data.frame(loc=unlist(strsplit(list0$dat0.Location,",")))
list1<-data.frame(loc=gsub(" ","",list1$loc))


list3<-cbind.data.frame(list1,ref)

## unique references

loc<-unique(list3$loc)


mat<-matrix(NA,length(loc),2)
for(i in seq(loc)){
  
  cat(i,"")
  sub<-list3[list3$loc==loc[i],]
  mat[i,1]<-loc[i]
  mat[i,2]<-length(unique(sub$ref))
  
}

mat<-as.data.frame(mat)
names(mat)<-c("loc","n")
mat$n<-as.numeric(mat$n)

write.csv(mat,"mattogo.csv")


## number of studies per location
mat[order(mat$n,decreasing = T),]


## togo-specific unique studies
mat[mat$loc=="Togo",]
