library(readxl)
count <- read_excel("~/JAIR/excels/count.xlsx")
count_sincaballos=count;
count_sincaballos$`Sum_ALIMEN,N,10,0`<-count_sincaballos$`Sum_ALIMEN,N,10,0`+count_sincaballos$`Sum_ESTA_R,N,10,0`;
count_0<-subset(count_sincaballos,select=c(-1,-11,-23,-32,-22,-9,-35,-12,-14)) #quito columna 1;

W=data.frame(log(count_0[1]+1))
#logaritmizar
for(i in 2:26){
  W=cbind(W,log(count_0[i]+1))
}
library(writexl)
write_xlsx(x = data.frame(cbind(count[1],W)), path = "count_log.xlsx", col_names = TRUE)

W_0<-mapply(W,FUN=as.numeric)
pca<-prcomp(W_0,scale. = TRUE,retx = TRUE,center=TRUE)
summary(pca)

##extras
sd=pca$sdev
pr_var <- sd^2
prop_varex <- pr_var/sum(pr_var)
# Generating Cumulative point chart
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

rawLoadings     <- pca$rotation[,1:7] %*% diag(pca$sdev, 7, 7) ##numero de componentes
rotatedLoadings <- varimax(rawLoadings)$loadings
invLoadings     <- t(pracma::pinv(rotatedLoadings))
scores          <- scale(W_0) %*% invLoadings ##
rotatedLoadings ##con varimax