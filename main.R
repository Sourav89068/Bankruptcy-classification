########### Necessary Libraries #############
library(MASS)
library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
library(ggExtra)

##################### Data reading ##########
data <- read_excel("data.xlsx")
View(data)
summary(data)
data1=data.matrix(data)
#############################################

#################### Count of firms #############
ggplot(data,aes(x=factor(population),fill=factor(population)))+geom_bar()+scale_colour_manual(name="Firm Type",labels=c("Bankrupt Firms","Sound Firms"),values = c("Brown","Magenta"),aesthetics = c("fill"))+theme(axis.title.x=element_blank())+labs("Barplot of Firm Types")+geom_text(stat='count', aes(label=..count..), vjust=-.1)+labs(title = "Count of Firms")
#################################################

############### violin plot of x1 and x3 variables ###########
x1<-data[c(2,6)]
x1wp<-cbind(paste0("x",rep(1,46)),x1)
colnames(x1wp)<-c("variable","values","population")
x1wp

x3<-data[c(4,6)]
x3wp<-cbind(paste0("x",rep(3,46)),x3)
colnames(x3wp)<-c("variable","values","population")
x3wp
barx1x3<-rbind(x1wp,x3wp)
barx1x3$population[barx1x3$population==0]="Bankrupt";barx1x3$population[barx1x3$population==1]="Sound Firms"

#ggplot(barx1x3,aes(y=values,fill=factor(population)))+geom_boxplot()+facet_wrap(~variable)+scale_colour_manual(name="Firm Type",labels=c("Bankrupt Firms","Sound Firms"),values = c("Brown","Magenta"),aesthetics = c("fill"))+theme_bw()+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(title = bquote('Barplot of different firms for '~x[1]~ 'and' ~x[3]~ ''))

ggplot(barx1x3,aes(x=factor(population),y=values,fill=factor(population)))+geom_violin()+facet_wrap(~variable,scales = "free")+scale_colour_manual(name="Firm Type",labels=c("Bankrupt Firms","Sound Firms"),values = c("Brown","Magenta"),aesthetics = c("fill"))+theme_bw()+theme(axis.title.x=element_blank())+labs(title = bquote('Violinplot of different firms for '~x[1]~ 'and' ~x[3]~ ''),subtitle = bquote(~x[1]~ "=CashFlow/Total Debt\t\t" ~x[3]~ "=Current Assests/Current Liabilites"))+
  geom_boxplot(width=0.2,fill="white")

#####################################################


################# violin plot of x1 and x4 variable #######################
# x1 x4
x4<-data[c(5,6)]
x4wp<-cbind(paste0("x",rep(4,46)),x4)
colnames(x4wp)<-c("variable","values","population")
x4wp
barx1x4<-rbind(x1wp,x4wp)
barx1x4$population[barx1x4$population==0]="Bankrupt";barx1x4$population[barx1x4$population==1]="Sound Firms"
ggplot(barx1x4,aes(x=factor(population),y=values,fill=factor(population)))+geom_violin()+facet_wrap(~variable,scales = "free")+scale_colour_manual(name="Firm Type",labels=c("Bankrupt Firms","Sound Firms"),values = c("Brown","Magenta"),aesthetics = c("fill"))+theme_bw()+theme(axis.title.x=element_blank())+labs(title = bquote('Violinplot of different firms for '~x[1]~ 'and' ~x[4]~ ''),subtitle = bquote(~x[1]~ "=CashFlow/Total Debt\t\t\t" ~x[4]~ "=Current Assests/Net Sales"))+geom_boxplot(width=0.2,fill="white")
######################################################################
## Conclusion from these two plots x1 and x3 are better discriminator for the type of firms ###########3


############## Visualization of the data to see whether it is coming from normal distribution###########
x1_bar<-mean(data$x1)
x2_bar<-mean(data$x2)
x3_bar<-mean(data$x3)
x4_bar<-mean(data$x4)

d<-ggplot(data,aes(x1,x2))+geom_point(size=2,color="brown")+geom_point(size=3.5,x=x1_bar,y=x2_bar,color="blue",shape="x")+stat_ellipse(col="green3",type="norm")+theme_bw()
d<-ggMarginal(d, groupColour = F, groupFill = F,type="densigram",fill="brown")
e<-ggplot(data,aes(x1,x3))+geom_point(size=2,color="brown")+geom_point(size=3.5,x=x1_bar,y=x3_bar,color="blue",shape="x")+stat_ellipse(col="green3",type="norm")+theme_bw()
e<-ggMarginal(e, groupColour = F, groupFill = F,type="densigram",fill="brown")
f<-ggplot(data,aes(x1,x4))+geom_point(size=2,color="brown")+geom_point(size=3.5,x=x1_bar,y=x4_bar,color="blue",shape="x")+stat_ellipse(col="green3",type="norm")+theme_bw()
f<-ggMarginal(f, groupColour = F, groupFill = F,type="densigram",fill="brown")
grid.arrange(d,e,f,nrow=2,top="Scatter plot of the data pairs with x1 and their corresponding marginal plots")
################################################################################
###### All of them almost looks like bivariate normal data by observing normality in their marginal plots and normal sphere with 95 confidence and the mean is also in the middle of the data distribution############



##################### Mean of x1,x3 and x1,x4 ##########
meanx<-data%>%dplyr::select(x1,x2,x3,x4,population)%>%group_by(population)%>%summarise(meanx1=mean(x1),meanx2=mean(x2),meanx3=mean(x3),mean4=mean(x4))
meanx[c(2,4)][1,];meanx[c(2,4)][2,] ### x1_bar and x2_bar of x1 and x3 
meanx[c(2,5)][1,];meanx[c(2,5)][2,] ### x1_bar and x2_bar of x1 and x4
############# 
############  Variance covariance matrix of x1,x3 and x1,x4 ##############
varcov013<-data[data$population==0,][c(2,4)]
varcov113<-data[data$population==1,][c(2,4)]
cov(varcov013);cov(varcov113) #S1 S2 for x1 x3
varcov014<-data[data$population==0,][c(2,5)]
varcov114<-data[data$population==1,][c(2,5)]
cov(varcov014);cov(varcov114) #S1 S2 for x1 x4
#############################
#### for x1 x2 #########
mean0vecforx1x3<-meanx[1,c(2,4)];mean0vecforx1x3 ### x1bar
mean1vecforx1x3<-meanx[2,c(2,4)];mean1vecforx1x3 ### x2bar
for0varcov<-data[which(data$population==0),][c(2,4)];varcov0x1x3<-cov(for0varcov);varcov0x1x3 #S1
for1varcov<-data[which(data$population==1),][c(2,4)];varcov1x1x3<-cov(for1varcov);varcov1x1x3 #S2
##########################
#### for x1 x4 ############
mean0vecforx1x4<-meanx[1,c(2,5)];mean0vecforx1x4 ### x1bar
mean1vecforx1x4<-meanx[2,c(2,5)];mean1vecforx1x4 ### x2bar
for0varcov<-data[which(data$population==0),][c(2,5)];varcov0x1x4<-cov(for0varcov);varcov0x1x4 #S1
for1varcov<-data[which(data$population==1),][c(2,5)];varcov1x1x4<-cov(for1varcov);varcov1x1x4 #S2
###########################

############ Classification of firm type with x1 and x3 variable 
############################################
classify<-function(d,indices,newpt,p1=0.5,p2=0.5){
  X1<-d[d$population==0,][c(indices[1],indices[2])]
  X2<-d[d$population==1,][c(indices[1],indices[2])]
  x1br<-colMeans(X1)
  x2br<-colMeans(X2)
  s1pooled<-cov(X1)
  s2pooled<-cov(X2)
  k=(log(det(s1pooled)/det(s2pooled))+(t(x1br)%*%solve(s1pooled)%*%(x1br)-t(x2br)%*%solve(s2pooled)%*%(x2br)))*0.5
  l=-0.5*(t(newpt)%*%(solve(s1pooled)-solve(s2pooled))%*%newpt)+( (t(x1br)%*%solve(s1pooled)) - (t(x2br)%*%solve(s2pooled)) )%*%newpt
  r=log(p2/p1)
  if (l>=k+r) 
    return (0)
  else 
    return (1)
}

classes<-c()
for (i in 1:46){
  class<-classify(data,c(2,4),as.matrix(as.numeric(data[i,c(2,4)])))
  classes<-c(classes,class)
}

classes
data2<-data
data2$prediction<-classes

### for p1=p2=0.5 x1 x3 #####
ctmy3 <- table(data2$population, data2$prediction,dnn = c("Actual","Predicted"))
ctmy3 ######## Confusion matrix
sum(diag(prop.table(ctmy3))) ### efficiency for x1 and x3 
APERquad3<-1-sum(diag(prop.table(ctmy3)));APERquad3 #### APER for x1 and x3
data2$check<-(data2$population==data2$prediction) 
ggplot(data2,aes(x1,x3,size=check,shape=factor(prediction)))+geom_point(aes(col=factor(population)))+scale_colour_manual(name="Firm Type",labels=c("Bankrupt Firms","Sound Firms"),values = c("Brown","Magenta"),aesthetics = c("col"))+theme_bw()+labs(subtitle="for p1=p2=0.5",title="Distribution of predicted points on x1 vs x3 plane", color="Firm Type",shape="Firm Type",size="Classification")+scale_shape(solid = FALSE)+labs(y=expression(x[3]),x=expression(x[1]))+scale_colour_manual(name="Firm Type",labels=c("Bankrupt Firms","Sound Firms"),values = c("Brown","Magenta"),aesthetics = c("colour"))+scale_shape_discrete(name="Firm Type",labels=c("Bankrupt Firms","Sound Firms"))

##########################

############ for AER #############
ER<-c()
El<-c()


for (i in 1:46){
  dat<-data[-i,]
  classes1<-c()
  for (j in 1:45){
    class1<-classify(dat,c(2,4),as.matrix(as.numeric(dat[j,c(2,4)])))
    classes1<-c(classes1,class1)
  }
  er<-1-mean(dat$population==classes1)
  ER<-c(ER,er)
  
  
}

AER3<-sum(ER)/46; AER3  ### AER for x1 and x3
###########################

########################################
####################################

############################################
################ for x1 and x4
classes<-c()
for (i in 1:46){
  class<-classify(data,c(2,5),as.matrix(as.numeric(data[i,c(2,5)])))
  classes<-c(classes,class)
}



classes
data2<-data
data2$prediction<-classes


### for p1=p2=0.5 x1 x3 #####
ctmy4 <- table(data2$population, data2$prediction,dnn = c("Actual","Predicted"))
ctmy4
sum(diag(prop.table(ctmy4)))

APERquad4<-1-sum(diag(prop.table(ctmy4)));APERquad4
data2$check<-(data2$population==data2$prediction)
ggplot(data2,aes(x1,x4,size=check,shape=factor(prediction)))+geom_point(aes(col=factor(population)))+scale_colour_manual(name="Firm Type",labels=c("Bankrupt Firms","Sound Firms"),values = c("Brown","Magenta"),aesthetics = c("col"))+theme_bw()+labs(subtitle="for p1=p2=0.5",title="Distribution of predicted points on x1 vs x4 plane", color="Firm Type",shape="Firm Type",size="Classification")+scale_shape(solid = FALSE)+labs(y=expression(x[4]),x=expression(x[1]))+scale_colour_manual(name="Firm Type",labels=c("Bankrupt Firms","Sound Firms"),values = c("Brown","Magenta"),aesthetics = c("colour"))+scale_shape_discrete(name="Firm Type",labels=c("Bankrupt Firms","Sound Firms"))

##########################

############ for AER #############
ER<-c()
El<-c()


for (i in 1:46){
  dat<-data[-i,]
  classes1<-c()
  for (j in 1:45){
    class1<-classify(dat,c(2,5),as.matrix(as.numeric(dat[j,c(2,5)])))
    classes1<-c(classes1,class1)
  }
  er<-1-mean(dat$population==classes1)
  ER<-c(ER,er)
  
  
}

AER4<-sum(ER)/46; AER4



#### for p1=p2=1/2 APER and AER for x1,x3 and x1,x4 ##########
APERquad3;AER3
APERquad4;AER4
p5APERquad3<-APERquad3;p5AER3<-AER3
p5APERquad4<-APERquad4;p5AER4<-AER4
#########################

################################################################################################################
###############


#for p1=0.05,p2=0.95  APER and AER for x1,x3 and x1,x4

classify2<-function(d,indices,newpt,p1=0.05,p2=0.95){
  X1<-d[d$population==0,][c(indices[1],indices[2])]
  X2<-d[d$population==1,][c(indices[1],indices[2])]
  x1br<-colMeans(X1)
  x2br<-colMeans(X2)
  s1pooled<-cov(X1)
  s2pooled<-cov(X2)
  k=(log(det(s1pooled)/det(s2pooled))+(t(x1br)%*%solve(s1pooled)%*%(x1br)-t(x2br)%*%solve(s2pooled)%*%(x2br)))*0.5
  l=-0.5*(t(newpt)%*%(solve(s1pooled)-solve(s2pooled))%*%newpt)+( (t(x1br)%*%solve(s1pooled)) - (t(x2br)%*%solve(s2pooled)) )%*%newpt
  r=log(p2/p1)
  if (l>=k+r) 
    return (0)
  else 
    return (1)
}

classes<-c()
for (i in 1:46){
  class<-classify2(data,c(2,4),as.matrix(as.numeric(data[i,c(2,4)])))
  classes<-c(classes,class)
}

classes
data2<-data
data2$prediction<-classes

### for p1=0.05 and p2=0.95 x1 x3 #####
ctmy3 <- table(data2$population, data2$prediction,dnn = c("Actual","Predicted"))
ctmy3
sum(diag(prop.table(ctmy3)))
APERquad3<-1-sum(diag(prop.table(ctmy3)));APERquad3
data2$check<-(data2$population==data2$prediction)
ggplot(data2,aes(x1,x3,size=check,shape=factor(prediction)))+geom_point(aes(col=factor(population)))+scale_colour_manual(name="Firm Type",labels=c("Bankrupt Firms","Sound Firms"),values = c("Brown","Magenta"),aesthetics = c("col"))+theme_bw()+labs(subtitle="for p1=0.05, p2=0.95",title="Distribution of predicted points on x1 vs x3 plane", color="Firm Type",shape="Firm Type",size="Classification")+scale_shape(solid = FALSE)+labs(y=expression(x[3]),x=expression(x[1]))+scale_colour_manual(name="Firm Type",labels=c("Bankrupt Firms","Sound Firms"),values = c("Brown","Magenta"),aesthetics = c("colour"))+scale_shape_discrete(name="Firm Type",labels=c("Bankrupt Firms","Sound Firms"))

##########################

############ for AER #############
ER<-c()
El<-c()


for (i in 1:46){
  dat<-data[-i,]
  classes1<-c()
  for (j in 1:45){
    class1<-classify2(dat,c(2,4),as.matrix(as.numeric(dat[j,c(2,4)])))
    classes1<-c(classes1,class1)
  }
  er<-1-mean(dat$population==classes1)
  ER<-c(ER,er)
  
  
}

AER3<-sum(ER)/46; AER3
###########################

########################################
####################################

############################################
################ for x1 and x4
classes<-c()
for (i in 1:46){
  class<-classify2(data,c(2,5),as.matrix(as.numeric(data[i,c(2,5)])))
  classes<-c(classes,class)
}

classes
data2<-data
data2$prediction<-classes

### for p1=p2=0.5 x1 x3 #####
ctmy4 <- table(data2$population, data2$prediction,dnn = c("Actual","Predicted"))
ctmy4
sum(diag(prop.table(ctmy4)))
APERquad4<-1-sum(diag(prop.table(ctmy4)));APERquad4
data2$check<-(data2$population==data2$prediction)
ggplot(data2,aes(x1,x4,size=check,shape=factor(prediction)))+geom_point(aes(col=factor(population)))+scale_colour_manual(name="Firm Type",labels=c("Bankrupt Firms","Sound Firms"),values = c("Brown","Magenta"),aesthetics = c("col"))+theme_bw()+labs(subtitle="for p1=0.05, p2=0.95",title="Distribution of predicted points on x1 vs x4 plane", color="Firm Type",shape="Firm Type",size="Classification")+scale_shape(solid = FALSE)+labs(y=expression(x[4]),x=expression(x[1]))+scale_colour_manual(name="Firm Type",labels=c("Bankrupt Firms","Sound Firms"),values = c("Brown","Magenta"),aesthetics = c("colour"))+scale_shape_discrete(name="Firm Type",labels=c("Bankrupt Firms","Sound Firms"))

##########################

############ for AER #############
ER<-c()
El<-c()


for (i in 1:46){
  dat<-data[-i,]
  classes1<-c()
  for (j in 1:45){
    class1<-classify2(dat,c(2,5),as.matrix(as.numeric(dat[j,c(2,5)])))
    classes1<-c(classes1,class1)
  }
  er<-1-mean(dat$population==classes1)
  ER<-c(ER,er)
  
  
}

AER4<-sum(ER)/46; AER4



#### for p1=0.05,p2=0.95 APER and AER for x1,x3 and x1,x4 ##########
APERquad3;AER3
APERquad4;AER4
####
p5APERquad3;p5AER3#### for p1=p2=0.5
p5APERquad4;p5AER4#### for p1=p2=0.5



#### Variation of APER wrt priors ####
El<-c()
for (p in seq(0,1,length.out=50)){
  classes2<-c()
  for (i in 1:46){
    class2<-classify(data,c(2,4),as.matrix(as.numeric(data[i,c(2,4)])),p1=p,p2=1-p)
    classes2<-c(classes2,class2)
  }
  er<-1-mean(data$population==classes2)
  El<-c(El,er)
}


ggplot(mapping = aes(x=seq(0,1,length.out=50),y=El,color=El))+geom_point()+geom_line()+geom_hline(yintercept = p5APERquad3,color="red")+geom_vline(xintercept = 0.5,color="red")+
  geom_hline(yintercept = APERquad3,color="blue")+geom_vline(xintercept = 0.05,color="blue")+
  labs(title = "Variation of error with variation in priors",subtitle = "Modeled with variables x1 and x3",x="Prior of Bankrupt class (p1)",y="Model error",color="Model error")+
  annotate("text",x=0.05,y=APERquad3,label=paste("p1=0.05, APER=",signif(APERquad3,4)),color="blue",vjust=1,hjust=-0.2)+
  annotate("text",x=0.5,y=p5APERquad3,label=paste("p1=0.5, APER=",signif(p5APERquad3,4)),color="red",vjust=1,hjust=-0.2)




El2<-c()
for (p in seq(0,1,length.out=50)){
  classes3<-c()
  for (i in 1:46){
    class3<-classify(data,c(2,5),as.matrix(as.numeric(data[i,c(2,5)])),p1=p,p2=1-p)
    classes3<-c(classes3,class3)
  }
  er<-1-mean(data$population==classes3)
  El2<-c(El2,er)
}


ggplot(mapping = aes(x=seq(0,1,length.out=50),y=El2,color=El2))+geom_point()+geom_line()+geom_hline(yintercept = p5APERquad4,color="red")+geom_vline(xintercept = 0.5,color="red")+
  geom_hline(yintercept = APERquad4,color="blue")+geom_vline(xintercept = 0.05,color="blue")+
  labs(title = "Variation of error with variation in priors",subtitle = "Modeled with variables x1 and x4",x="Prior of Bankrupt class",y="Model error",color="Model error")+
  annotate("text",x=0.05,y=APERquad4,label=paste("p1=0.05, APER=", signif(APERquad4,4)),color="blue",vjust=1,hjust=-0.25)+
  annotate("text",x=0.5,y=p5APERquad4,label=paste("p1=0.5, APER=",signif(p5APERquad4,4)),color="red",vjust=1.2,hjust=-0.3)

