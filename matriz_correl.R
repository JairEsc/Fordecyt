library(readxl)
library(tibble)
install.packages("tibble")
library(foreign)
library("ggcorrplot")
base_contam<-read.dbf("JAIR/excels/Extract_raster_regiones.dbf")
base_enferm<- read_excel("~/JAIR/excels/indice_fordecyt.xlsx",sheet = "Promedios")
hacer<-function(base_enferm,contaminante,string){
  mat_NA <- matrix(NA, ncol = 2, nrow = 0)
  colnames(mat_NA)<-c("otraCVE",string)
  for (i in 1:length(base_enferm$CVE_GEO)){
    j<-which(contaminante$CVE_GEO==base_enferm$CVE_GEO[i])
    if(length(j)>0){
      mat_NA<-rbind(mat_NA,cbind(contaminante$CVE_GEO[[j]],contaminante[j,2]))
    }
    else{
      mat_NA<-rbind(mat_NA,rep(-1000,2))
    }
  }
  
  W<-cbind(base_enferm,mat_NA)
  return(subset(W,W[string]>0,select=c(-1,-20)))
}
strings<-character(0)
mat_correl<-matrix(NA, nrow = 17, ncol = 18)
colnames(mat_correl)<-colnames(subset(base_enferm,select=2:19))
for (i in 11:27){
  contaminante=subset(base_contam,select=c(5,i));
  string<-colnames(contaminante)[2]
  strings[i-10]<-string
  matriz<-hacer(base_enferm , contaminante , string)
  vector_correl<-cor(mapply(matriz,FUN=as.numeric),method = "spearman")[19,1:18]
  mat_correl[i-10,]<-vector_correl
}
rownames(mat_correl)<-strings
ggcorrplot(mat_correl,title = "Padecimientos vs contaminantes",lab = TRUE,legend.title = "Spearman Corr")
ggplot2::ggsave("matriz_corr.jpeg",plot=last_plot(),width = 25,height = 14)
