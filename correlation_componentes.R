install.packages("ggcorrplot")                      # Install ggcorrplot package
library("ggcorrplot")
library(readxl)
library()

i=1
base<-read_excel("JAIR/excels/indices de los componentes_contaminacion (Jesus_15_04_2021).xlsx", 
           sheet = as.character(i))
base<-subset(base,base$CVE_GEO>0)
ind<-rep(-1000,428)
base<-cbind(base,ind)
indice<-read_excel("JAIR/excels/INDICE_VULN_1.xlsx")

for (i in 1:426){
h<-which(base$CVE_GEO==indice$`CVE_GEO,N,10,0`[i]);
base$ind[h]<-indice$indice[i]
}
base<-base[-which(base$ind==-1000),]
ggcorrplot(cor(mapply(subbase,FUN = as.numeric),method = c("spearman")),lab = TRUE, title = paste('Correlacion ',str),) 
gen_plot<-function(j){
  str<-paste("componente",as.character(j))
  low<-2+(j-1)*5
  up<-6+(j-1)*5
  W<-cbind(base[low:up],base$ind)
  colnames(W)<-c("0-15","15-40","40-60","60-80",">80","indice")
  return(ggcorrplot(cor(mapply(W,FUN = as.numeric),method = c("spearman")),lab = TRUE, title = paste('Correlación ',str),type = "upper") )
  
}
A<-gen_plot(1)
B<-gen_plot(2)
C<-gen_plot(3)
D<-gen_plot(4)
E<-gen_plot(5)
F<-gen_plot(6)
G<-gen_plot(7)

plot_grid(A,B,C,D,E,F,G, labels = c("A","B","C","D","E","F","G"), ncol=3, nrow = 3)
ggplot2::ggsave("corr_3x3.jpeg",plot=last_plot(),width = 20,height = 20)
