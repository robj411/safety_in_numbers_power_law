library(matrixStats)
library(latex2exp)
library(readstata13)
library(xlsx)
library(readODS)
library(tensorA)
library(dplyr)
library(tidyr)
library(MASS); 
library(splines)
# setwd('~/overflow_dropbox/safety_in_numbers_power_law/england_LAs/')

datasets <- readRDS('data/england_data.Rds')
ssg.la.base <- datasets[[1]]
ssg.la.road.base <- datasets[[2]]
ssg.reg.base <- datasets[[3]]
ssg.road.base <- datasets[[4]]
ssg.total.base <- datasets[[5]]
la_map <- readRDS('data/la_map.Rds')

######################################################################################

regions <- levels(ssg.reg.base$region)
modes <- levels(ssg.reg.base$strike_mode)
#ssg.la.base$cas_distance <- ssg.la.base$cas_distance*1e-9
#ssg.la.base$strike_distance <- ssg.la.base$strike_distance*1e-9
#ssg.reg.base$cas_distance <- ssg.reg.base$cas_distance*1e-9
#ssg.reg.base$strike_distance <- ssg.reg.base$strike_distance*1e-9

true_count <- pred_count <- pred_count_corrected <- cas_sin <- str_sin <- matrix(0,nrow=length(regions),ncol=length(modes)); 
betas <- betas3 <- matrix(0,ncol=2,nrow=length(modes))
country_pred <- matrix(0,ncol=3,nrow=length(modes)); 
sev <- c('Fatal','Serious')
intercepts <- list()
{pdf('englandParameters.pdf',width=9,height=6); par(mfrow=c(2,3),mar=c(5,5,0.5,0.5))
for(vehicle in 1:length(modes)){
  intercepts[[vehicle]] <- list()
  coefs <- matrix(0,ncol=2,nrow=length(regions))
  coefs_meso <- matrix(0,ncol=2,nrow=2)
  se.coefs <- matrix(0,ncol=2,nrow=length(regions))
  se.coefs_meso <- matrix(0,ncol=2,nrow=2)
  veh <- modes[vehicle]
  england_data <- england_data_corrected <- data.frame(cas_severity=character(),strike_mode=character(),year=integer(),region=character(),count=numeric(),population=numeric(),cas_distance=numeric(),strike_distance=numeric())#subset(ssg.reg.base,cas_severity%in%sev&strike_mode==veh)
  dataset <- subset(ssg.la.base,cas_severity%in%sev&strike_mode==veh)
  dataset$cas_distance_sum <- apply(dataset,1,function(x)sum(dataset$cas_distance[dataset$year==x[3]&dataset$region==x[8]&dataset$cas_severity==sev[1]]))
  dataset$strike_distance_sum <- apply(dataset,1,function(x)sum(dataset$strike_distance[dataset$year==x[3]&dataset$region==x[8]&dataset$cas_severity==sev[1]]))
  intercepts[[vehicle]][[1]] <- rep(0,length(regions))
  if(vehicle>1){
    for(i in 1:length(regions)){
      fit <- glm(count~log(cas_distance)+log(strike_distance),data=subset(ssg.la.base,region==regions[i]&cas_severity%in%sev&strike_mode==veh),family=poisson())
      coefs[i,] <- coefficients(fit)[2:3]
      intercepts[[vehicle]][[1]][i] <- coefficients(fit)[1]
      se.coefs[i,] <- summary(fit)$coefficient[2:3,2]
      cas_sin[i,vehicle] <- coefficients(fit)[2]
      str_sin[i,vehicle] <- coefficients(fit)[3]
      true_count[i,vehicle] <- sum(subset(ssg.reg.base,region==regions[i]&cas_severity%in%sev&strike_mode==veh)$count)
    }
    fit2 <- glm(count~I(log(cas_distance))+I(log(strike_distance)),data=dataset,family=poisson())
    intercepts[[vehicle]][[2]] <- coefficients(fit2)[1]
    coefs_meso[1,] <- betas[vehicle,] <- coefficients(fit2)[2:3]
    se.coefs_meso[1,] <- summary(fit2)$coefficient[2:3,2]
    fit3 <- glm(count~log(cas_distance)+log(strike_distance),data=subset(ssg.reg.base,cas_severity%in%sev&strike_mode==veh),family=poisson())
    coefs_meso[2,] <- betas3[vehicle,] <- coefficients(fit3)[2:3]
    intercepts[[vehicle]][[3]] <- coefficients(fit3)[1]
    se.coefs_meso[2,] <- summary(fit3)$coefficient[2:3,2]
    fit_corrected <- fit2
    fit_corrected$coefficients[2:3] <- c(1,1)
  }else{
    formula <- count~I(log(cas_distance))
    for(i in 1:length(regions)){
      fit <- glm(as.formula(formula),data=subset(ssg.la.base,region==regions[i]&cas_severity%in%sev&strike_mode==veh),family=poisson())
      coefs[i,] <- coefficients(fit)[2:1]
      se.coefs[i,] <- summary(fit)$coefficient[2:1,2]
      cas_sin[i,vehicle] <- coefficients(fit)[2]
      str_sin[i,vehicle] <- coefficients(fit)[1]
      pred_count[i,vehicle] <- sum(predict(fit,newdata=subset(ssg.reg.base,region==regions[i]&cas_severity%in%sev&strike_mode==veh),type='response'))
      true_count[i,vehicle] <- sum(subset(ssg.reg.base,region==regions[i]&cas_severity%in%sev&strike_mode==veh)$count)
    }
    fit2 <- glm(as.formula(formula),data=dataset,family=poisson())
    coefs_meso[1,] <- coefficients(fit2)[2:1]
    betas[vehicle,] <- coefficients(fit2)[2]
    se.coefs_meso[1,] <- summary(fit2)$coefficient[2:1,2]
    fit3 <- glm(as.formula(formula),data=subset(ssg.reg.base,cas_severity%in%sev&strike_mode==veh),family=poisson())
    coefs_meso[2,] <- coefficients(fit3)[2:1]
    se.coefs_meso[2,] <- summary(fit3)$coefficient[2:1,2]
    fit_corrected <- fit2
    fit_corrected$coefficients[2] <- 1
  }
  for(i in 1:length(regions)) {
    newdata <- subset(ssg.reg.base,region==regions[i]&cas_severity%in%sev&strike_mode==veh)
    newdata$cas_distance <- apply(newdata,1,function(x)sum(dataset$cas_distance[dataset$cas_severity==sev[1]&dataset$year==x[3]&dataset$region==x[4]]))
    if(vehicle==1) newdata$cas_distance <- apply(newdata,1,function(x)sum(dataset$cas_distance[dataset$cas_severity==sev[1]&dataset$year==x[3]&dataset$region==x[4]]))
    newdata$strike_distance <- apply(newdata,1,function(x)sum(dataset$strike_distance[dataset$cas_severity==sev[1]&dataset$year==x[3]&dataset$region==x[4]]))
    pred_count[i,vehicle] <- sum(predict(fit2,newdata=newdata,type='response'))
    england_data <- rbind(england_data,newdata)
    newdata <- subset(ssg.reg.base,region==regions[i]&cas_severity%in%sev&strike_mode==veh)
    newdata$cas_distance <- apply(newdata,1,function(x)sum(dataset$cas_distance[dataset$cas_severity==sev[1]&dataset$year==x[3]&dataset$region==x[4]]^coefs_meso[1,1]*
                                                             dataset$strike_distance[dataset$cas_severity==sev[1]&dataset$year==x[3]&dataset$region==x[4]]^coefs_meso[1,2]))
    if(vehicle==1) newdata$cas_distance <- apply(newdata,1,function(x)sum(dataset$cas_distance[dataset$cas_severity==sev[1]&dataset$year==x[3]&dataset$region==x[4]]^coefs_meso[1,1]))
    newdata$strike_distance <- 1
    england_data_corrected <- rbind(england_data_corrected,newdata)
    pred_count_corrected[i,vehicle] <- sum(predict(fit_corrected,newdata=newdata,type='response'))
  }
  country_pred[vehicle,1] <- sum(england_data$count)
  country_pred[vehicle,2] <- sum(predict(fit2,newdata=data.frame(
    cas_distance=apply(expand.grid(2005:2015,sev),1,function(x)sum(england_data$cas_distance[england_data$year==x[1]&england_data$cas_severity==x[2]])),
    strike_distance=apply(expand.grid(2005:2015,sev),1,function(x)sum(england_data$strike_distance[england_data$year==x[1]&england_data$cas_severity==x[2]]))),
    type='response'))
  country_pred[vehicle,3] <- sum(predict(fit_corrected,newdata=data.frame(
    cas_distance=apply(expand.grid(2005:2015,sev),1,function(x)sum(england_data_corrected$cas_distance[england_data_corrected$year==x[1]&england_data_corrected$cas_severity==x[2]])),
    strike_distance=1),
    type='response'))
  plot(coefs[,1],coefs[,2],frame=F,cex.main=1.5,xlab='Cyclist',ylab=ifelse(vehicle==1,'Intercept',veh),cex.lab=1.5,cex.axis=1.5)
  for(i in 1:dim(coefs)[1]) lines(x=rep(coefs[i,1],2),y=coefs[i,2]+c(se.coefs[i,2],-se.coefs[i,2]),col='gray')
  for(i in 1:dim(coefs)[1]) lines(y=rep(coefs[i,2],2),x=coefs[i,1]+c(se.coefs[i,1],-se.coefs[i,1]),col='gray')
  points(coefs_meso[,1],coefs_meso[,2],col=c('darkorange','turquoise'),pch=20,cex=2)
  lines(y=rep(coefs_meso[1,2],2),x=coefs_meso[1,1]+c(se.coefs_meso[1,1],-se.coefs_meso[1,1]),col='darkorange')
  lines(x=rep(coefs_meso[1,1],2),y=coefs_meso[1,2]+c(se.coefs_meso[1,2],-se.coefs_meso[1,2]),col='darkorange')
  lines(y=rep(coefs_meso[2,2],2),x=coefs_meso[2,1]+c(se.coefs_meso[2,1],-se.coefs_meso[2,1]),col='turquoise')
  lines(x=rep(coefs_meso[2,1],2),y=coefs_meso[2,2]+c(se.coefs_meso[2,2],-se.coefs_meso[2,2]),col='turquoise')
  if(vehicle!=1) lines(x=c(-5,6),y=c(6,-5),lty=2)
  abline(h=1,lty=2); abline(v=1,lty=2)
  if(vehicle==1) legend(x=-0.8,y=-40,bty='n',legend=c('Regions','Pooled','Summed'),col=c('black','darkorange','turquoise'),pch=c(1,20,20),cex=1.25)
}
}
dev.off()

{betas[1,] <- betas[1,]/2
x11(width=9);par(mar=c(5,5,1,1),fig = c(0,1,0,1));
  plot(rep(rowSums(betas),each=length(regions)),(as.vector(pred_count))-(as.vector(true_count)),frame=FALSE,pch=16,col='darkgray',ylim=range(c((as.vector(pred_count))-(as.vector(true_count)),country_pred[,2]-country_pred[,1])),
       xlab='Sum of SIN coefficients',ylab='Predicted count minus true count',cex.lab=1.5,cex.axis=1.5); 
  points(x=rowSums(betas),y=(country_pred[,2])-(country_pred[,1]),pch=20,cex=2,col='darkorange')
  text(x=rowSums(betas),y=c(-700,-1000,-2200,-1700,-1100,-1800),labels=modes,srt=90)
  #text(x=rowSums(betas),y=c(-130,-160,-110,-170,-120,-170),labels=modes,srt=90)
  abline(v=1,lty=2,col='darkgray'); abline(h=0,lty=2,col='darkgray'); 
  legend(x=1.2,y=-2500,legend=c('Country','Regions'),col=c('darkorange','darkgrey'),pch=c(16,20),pt.cex=c(1.5,1.25),bty='n',cex=1.25)
  par(fig = c(0.5,0.95,0.06, 0.5), new = T)  
  plot(rep(rowSums(betas),each=length(regions)),(as.vector(pred_count))-(as.vector(true_count)),pch=16,col='darkgray',ylim=range((country_pred[-3,2])-(country_pred[-3,1])),
    xlab='',ylab='',cex.axis=1,cex.lab=1,yaxt='n',xaxt='n'); 
  axis(1,line=0,tck=-0.01,mgp=c(3,0.3,0))
  axis(2,line=0,tck=-0.01,mgp=c(3,0.3,0)) 
  points(x=rowSums(betas)[-3],y=(country_pred[-3,2])-(country_pred[-3,1]),pch=20,cex=2,col='darkorange')
  abline(v=1,lty=2,col='darkgray'); abline(h=0,lty=2,col='darkgray'); 
  
  x11(width=9);par(mar=c(5,5,1,1),fig = c(0,1,0,1));
  plot(rep(rowSums(betas),each=length(regions)),(as.vector(pred_count_corrected))-(as.vector(true_count)),
       frame=FALSE,pch=16,col='darkgray',ylim=range(c((as.vector(pred_count_corrected))-(as.vector(true_count)),country_pred[,3]-country_pred[,1])),
       xlab='Sum of SIN coefficients',ylab='Predicted count minus true count',cex.lab=1.5,cex.axis=1.5); 
  points(x=rowSums(betas),y=(country_pred[,3])-(country_pred[,1]),pch=20,cex=2,col='darkorange')
  #text(x=rowSums(betas),y=c(-700,-1000,-2200,-1700,-1100,-1800),labels=modes,srt=90)
  text(x=rowSums(betas),y=c(-130,-160,-110,-170,-120,-170),labels=modes,srt=90)
  abline(v=1,lty=2,col='darkgray'); abline(h=0,lty=2,col='darkgray'); 
  legend(x=1.2,y=-2500,legend=c('Country','Regions'),col=c('darkorange','darkgrey'),pch=c(16,20),pt.cex=c(1.5,1.25),bty='n',cex=1.25)
}

roadlist <- list('A','B, C, Unclassified',c('A','B, C, Unclassified'))
for(k in 1:3){
  sev <- c('Fatal','Serious')
  x11(width=9,height=6); par(mfrow=c(2,3),mar=c(5,5,0.5,0.5))
  for(vehicle in 1:length(modes)){
    coefs <- matrix(0,ncol=2,nrow=length(regions)); coefs_meso <- matrix(0,ncol=2,nrow=2)
    se.coefs <- matrix(0,ncol=2,nrow=length(regions)); se.coefs_meso <- matrix(0,ncol=2,nrow=2)
    veh <- modes[vehicle]
    england_data <- data.frame(cas_severity=character(),strike_mode=character(),year=integer(),region=character(),count=numeric(),population=numeric(),cas_distance=numeric(),strike_distance=numeric())#subset(ssg.reg.base,cas_severity%in%sev&strike_mode==veh)
    dataset <- subset(ssg.la.road.base,cas_severity%in%sev&strike_mode==veh&roadtype%in%roadlist[[k]])
    dataset.reg <- subset(ssg.road.base,cas_severity%in%sev&strike_mode==veh&roadtype%in%roadlist[[k]])
    formula <- 'count~log(cas_distance)+log(strike_distance)'; formula2 <- 'count~I(log(cas_distance))'
    if(k==3) {formula <- 'count~log(cas_distance)+log(strike_distance)+roadtype-1'; formula2 <- 'count~I(log(cas_distance))+roadtype-1'}
    if(vehicle>1){
      for(i in 1:length(regions)){
        fit <- glm(as.formula(formula),data=subset(dataset,region==regions[i]),family=poisson())
        coefs[i,] <- coefficients(fit)[2:3-as.numeric(k==3)]
        se.coefs[i,] <- summary(fit)$coefficient[2:3-as.numeric(k==3),2]
        true_count[i,vehicle] <- sum(subset(dataset.reg,region==regions[i])$count)
      }
      fit2 <- glm(as.formula(formula),data=dataset,family=poisson())
      coefs_meso[1,] <- betas[vehicle,] <- coefficients(fit2)[2:3-as.numeric(k==3)]
      se.coefs_meso[1,] <- summary(fit2)$coefficient[2:3-as.numeric(k==3),2]
      fit3 <- glm(as.formula(formula),data=dataset.reg,family=poisson())
      coefs_meso[2,] <- coefficients(fit3)[2:3-as.numeric(k==3)]
      se.coefs_meso[2,] <- summary(fit3)$coefficient[2:3-as.numeric(k==3),2]
      fit2$coefficients[2:3-as.numeric(k==3)] <- c(1,1)
    }else{
      for(i in 1:length(regions)){
        fit <- glm(as.formula(formula2),data=subset(dataset,region==regions[i]),family=poisson())
        coefs[i,] <- coefficients(fit)[ifelse(k==3,1,2):ifelse(k==3,2,1)]
        se.coefs[i,] <- summary(fit)$coefficient[ifelse(k==3,1,2):ifelse(k==3,2,1),2]
        pred_count[i,vehicle] <- sum(predict(fit,newdata=subset(dataset.reg,region==regions[i]),type='response'))
        true_count[i,vehicle] <- sum(subset(dataset.reg,region==regions[i])$count)
      }
      fit2 <- glm(as.formula(formula2),data=dataset,family=poisson())
      coefs_meso[1,] <- coefficients(fit2)[ifelse(k==3,1,2):ifelse(k==3,2,1)]; betas[vehicle,] <- coefficients(fit2)[ifelse(k==3,1,2)]
      se.coefs_meso[1,] <- summary(fit2)$coefficient[ifelse(k==3,1,2):ifelse(k==3,2,1),2]
      fit3 <- glm(as.formula(formula2),data=dataset.reg,init.theta=2.5,control=glm.control(epsilon=1e-3,maxit=100))
      coefs_meso[2,] <- coefficients(fit3)[ifelse(k==3,1,2):ifelse(k==3,2,1)]
      se.coefs_meso[2,] <- summary(fit3)$coefficient[ifelse(k==3,1,2):ifelse(k==3,2,1),2]
      fit2$coefficients[ifelse(k==3,1,2)] <- 1
    }
    for(i in 1:length(regions)) {
      newdata <- subset(dataset.reg,region==regions[i])
      newdata$cas_distance <- apply(newdata,1,function(x)sum(dataset$cas_distance[dataset$cas_severity==sev[1]&dataset$roadtype==x[3]&dataset$year==x[4]&dataset$region==x[5]]^coefs_meso[1,1]*
                                                               dataset$strike_distance[dataset$cas_severity==sev[1]&dataset$roadtype==x[3]&dataset$year==x[4]&dataset$region==x[5]]^coefs_meso[1,2]))
      if(vehicle==1) newdata$cas_distance <- apply(newdata,1,function(x)sum(dataset$cas_distance[dataset$cas_severity==sev[1]&dataset$roadtype==x[3]&dataset$year==x[4]&dataset$region==x[5]]^coefs_meso[1,1]))
      newdata$strike_distance <- 1
      pred_count[i,vehicle] <- sum(predict(fit2,newdata=newdata,type='response'))
      england_data <- rbind(england_data,newdata)
    }
    country_pred[vehicle,1] <- sum(england_data$count)
    country_pred[vehicle,2] <- sum(predict(fit2,newdata=data.frame(
      roadtype=expand.grid(2005:2015,sev,roadlist[[k]])[,3],
      cas_distance=apply(expand.grid(2005:2015,sev,roadlist[[k]]),1,function(x)sum(england_data$cas_distance[england_data$year==x[1]&england_data$cas_severity==x[2]&england_data$roadtype==x[3]]*
                                                                                     england_data$strike_distance[england_data$year==x[1]&england_data$cas_severity==x[2]&england_data$roadtype==x[3]])),
      strike_distance=1),
      type='response'))
    plot(coefs[,1],coefs[,2],frame=F,cex.main=1.5,xlab='Cyclist',ylab=ifelse(vehicle==1,'Intercept',veh),cex.lab=1.5,cex.axis=1.5,ylim=range(c(coefs[,2],coefs_meso[,2])))
    for(i in 1:dim(coefs)[1]) lines(x=rep(coefs[i,1],2),y=coefs[i,2]+c(se.coefs[i,2],-se.coefs[i,2]),col='gray')
    for(i in 1:dim(coefs)[1]) lines(y=rep(coefs[i,2],2),x=coefs[i,1]+c(se.coefs[i,1],-se.coefs[i,1]),col='gray')
    points(coefs_meso[,1],coefs_meso[,2],col=c('darkorange','turquoise'),pch=20,cex=2)
    lines(y=rep(coefs_meso[1,2],2),x=coefs_meso[1,1]+c(se.coefs_meso[1,1],-se.coefs_meso[1,1]),col='darkorange')
    lines(x=rep(coefs_meso[1,1],2),y=coefs_meso[1,2]+c(se.coefs_meso[1,2],-se.coefs_meso[1,2]),col='darkorange')
    lines(y=rep(coefs_meso[2,2],2),x=coefs_meso[2,1]+c(se.coefs_meso[2,1],-se.coefs_meso[2,1]),col='turquoise')
    lines(x=rep(coefs_meso[2,1],2),y=coefs_meso[2,2]+c(se.coefs_meso[2,2],-se.coefs_meso[2,2]),col='turquoise')
    abline(h=1,lty=2); abline(v=1,lty=2)
    if(vehicle==1) legend(x=-0.5,y=-40,bty='n',legend=c('Regions','Pooled','Summed'),col=c('black','darkorange','turquoise'),pch=c(1,20,20),cex=1.25)
  }
  betas[1,] <- betas[1,]/2
  dev.off()
  x11(width=9);par(mar=c(5,5,1,1),fig = c(0,1,0,1));
  plot(rep(rowSums(betas),each=length(regions)),(as.vector(pred_count))-(as.vector(true_count)),frame=FALSE,pch=16,col='darkgray',ylim=range(c((as.vector(pred_count))-(as.vector(true_count)),country_pred[,2]-country_pred[,1])),
       xlab='Sum of SIN coefficients',ylab='Predicted count minus true count',cex.lab=1.5,cex.axis=1.5,main=roadlist[[k]]); 
  points(x=rowSums(betas),y=(country_pred[,2])-(country_pred[,1]),pch=20,cex=2,col='darkorange')
  #text(x=rowSums(betas),y=c(-700,-1000,-2200,-1700,-1100,-1800),labels=modes,srt=90)
  text(x=rowSums(betas),y=c(-130,-160,-110,-170,-120,-170),labels=modes,srt=90)
  abline(v=1,lty=2,col='darkgray'); abline(h=0,lty=2,col='darkgray'); 
  legend(x=1.2,y=-2500,legend=c('Country','Regions'),col=c('darkorange','darkgrey'),pch=c(16,20),pt.cex=c(1.5,1.25),bty='n',cex=1.25)
}

######################################################################################
## severity
true_count <- pred_count <- matrix(0,nrow=length(regions),ncol=length(modes)); betas <- country_pred <- matrix(0,ncol=2,nrow=length(modes)); 
sev <- c('Fatal','Serious')
{x11(width=9,height=6); par(mfrow=c(2,3),mar=c(5,5,0.5,0.5))
for(vehicle in 1:length(modes)){
  coefs <- matrix(0,ncol=2,nrow=length(regions)); coefs_meso <- matrix(0,ncol=2,nrow=2)
  se.coefs <- matrix(0,ncol=2,nrow=length(regions)); se.coefs_meso <- matrix(0,ncol=2,nrow=2)
  veh <- modes[vehicle]
  england_data <- data.frame(cas_severity=character(),strike_mode=character(),year=integer(),region=character(),count=numeric(),population=numeric(),cas_distance=numeric(),strike_distance=numeric())#subset(ssg.reg.base,cas_severity%in%sev&strike_mode==veh)
  dataset <- subset(ssg.la.base,cas_severity==sev[1]&strike_mode==veh)
  dataset$count <- dataset$count + subset(ssg.la.base,cas_severity==sev[2]&strike_mode==veh)$count
  dataset.reg <- subset(ssg.reg.base,cas_severity==sev[1]&strike_mode==veh)
  dataset.reg$count <- dataset.reg$count + subset(ssg.reg.base,cas_severity==sev[2]&strike_mode==veh)$count
  if(vehicle>1){
    for(i in 1:length(regions)){
      fit <- glm.nb(count~log(cas_distance)+log(strike_distance),data=subset(dataset,region==regions[i]),init.theta=1,control=glm.control(epsilon=1e-3,maxit=25))
      coefs[i,] <- coefficients(fit)[2:3]
      se.coefs[i,] <- summary(fit)$coefficient[2:3,2]
      cas_sin[i,vehicle] <- coefficients(fit)[2]; str_sin[i,vehicle] <- coefficients(fit)[3]
      true_count[i,vehicle] <- sum(subset(dataset.reg,region==regions[i])$count)
    }
    fit2 <- glm.nb(count~I(log(cas_distance))+I(log(strike_distance)),data=dataset,init.theta=0.8,control=glm.control(epsilon=1e-3,maxit=100))
    coefs_meso[1,] <- betas[vehicle,] <- coefficients(fit2)[2:3]
    se.coefs_meso[1,] <- summary(fit2)$coefficient[2:3,2]
    fit3 <- glm.nb(count~log(cas_distance)+log(strike_distance),data=dataset.reg,init.theta=0.8,control=glm.control(epsilon=1e-3,maxit=100))
    coefs_meso[2,] <- coefficients(fit3)[2:3]
    se.coefs_meso[2,] <- summary(fit3)$coefficient[2:3,2]
    fit2$coefficients[2:3] <- c(1,1)
  }else{
    formula <- count~I(log(cas_distance))
    for(i in 1:length(regions)){
      fit <- glm.nb(as.formula(formula),data=subset(dataset,region==regions[i]),init.theta=1,control=glm.control(epsilon=1e-3,maxit=25))
      coefs[i,] <- coefficients(fit)[2:1]
      se.coefs[i,] <- summary(fit)$coefficient[2:1,2]
      cas_sin[i,vehicle] <- coefficients(fit)[2]; str_sin[i,vehicle] <- coefficients(fit)[1]
      pred_count[i,vehicle] <- sum(predict(fit,newdata=subset(dataset.reg,region==regions[i]),type='response'))
      true_count[i,vehicle] <- sum(subset(dataset.reg,region==regions[i])$count)
    }
    fit2 <- glm.nb(as.formula(formula),data=dataset,init.theta=1.5,control=glm.control(epsilon=1e-3,maxit=100))
    coefs_meso[1,] <- coefficients(fit2)[2:1]; betas[vehicle,] <- coefficients(fit2)[2]
    se.coefs_meso[1,] <- summary(fit2)$coefficient[2:1,2]
    fit3 <- glm.nb(as.formula(formula),data=dataset.reg,init.theta=2.5,control=glm.control(epsilon=1e-3,maxit=100))
    coefs_meso[2,] <- coefficients(fit3)[2:1]
    se.coefs_meso[2,] <- summary(fit3)$coefficient[2:1,2]
    print(summary(fit2))
    fit2$coefficients[2] <- 1
  }
  for(i in 1:length(regions)) {
    newdata <- subset(dataset.reg,region==regions[i])
    newdata$cas_distance <- apply(newdata,1,function(x)sum(dataset$cas_distance[dataset$year==x[3]&dataset$region==x[4]]^coefs_meso[1,1]*
                                                             dataset$strike_distance[dataset$year==x[3]&dataset$region==x[4]]^coefs_meso[1,2]))
    if(vehicle==1) newdata$cas_distance <- apply(newdata,1,function(x)sum(dataset$cas_distance[dataset$year==x[3]&dataset$region==x[4]]^coefs_meso[1,1]))
    newdata$strike_distance <- 1
    pred_count[i,vehicle] <- sum(predict(fit2,newdata=newdata,type='response'))
    england_data <- rbind(england_data,newdata)
  }
  country_pred[vehicle,1] <- sum(england_data$count)
  country_pred[vehicle,2] <- sum(predict(fit2,newdata=data.frame(
    cas_distance=apply(expand.grid(2005:2015,sev[1]),1,function(x)sum(england_data$cas_distance[england_data$year==x[1]]*
                                                                        england_data$strike_distance[england_data$year==x[1]])),
    strike_distance=1),
    type='response'))
  plot(coefs[,1],coefs[,2],frame=F,cex.main=1.5,xlab='Cyclist',ylab=ifelse(vehicle==1,'Intercept',veh),cex.lab=1.5,cex.axis=1.5)
  for(i in 1:dim(coefs)[1]) lines(x=rep(coefs[i,1],2),y=coefs[i,2]+c(se.coefs[i,2],-se.coefs[i,2]),col='gray')
  for(i in 1:dim(coefs)[1]) lines(y=rep(coefs[i,2],2),x=coefs[i,1]+c(se.coefs[i,1],-se.coefs[i,1]),col='gray')
  points(coefs_meso[,1],coefs_meso[,2],col=c('darkorange','turquoise'),pch=20,cex=2)
  lines(y=rep(coefs_meso[1,2],2),x=coefs_meso[1,1]+c(se.coefs_meso[1,1],-se.coefs_meso[1,1]),col='darkorange')
  lines(x=rep(coefs_meso[1,1],2),y=coefs_meso[1,2]+c(se.coefs_meso[1,2],-se.coefs_meso[1,2]),col='darkorange')
  lines(y=rep(coefs_meso[2,2],2),x=coefs_meso[2,1]+c(se.coefs_meso[2,1],-se.coefs_meso[2,1]),col='turquoise')
  lines(x=rep(coefs_meso[2,1],2),y=coefs_meso[2,2]+c(se.coefs_meso[2,2],-se.coefs_meso[2,2]),col='turquoise')
  abline(h=1,lty=2); abline(v=1,lty=2)
  if(vehicle==1) legend(x=-0.5,y=-40,bty='n',legend=c('Regions','Pooled','Summed'),col=c('black','darkorange','turquoise'),pch=c(1,20,20),cex=1.25)
}
}

betas[1,] <- betas[1,]/2
x11(width=9);par(mar=c(5,5,1,1),fig = c(0,1,0,1));
plot(rep(rowSums(betas),each=length(regions)),(as.vector(pred_count))-(as.vector(true_count)),frame=FALSE,pch=16,col='darkgray',ylim=range(c((as.vector(pred_count))-(as.vector(true_count)),country_pred[,2]-country_pred[,1])),
     xlab='Sum of SIN coefficients',ylab='Predicted count minus true count',cex.lab=1.5,cex.axis=1.5); 
points(x=rowSums(betas),y=(country_pred[,2])-(country_pred[,1]),pch=20,cex=2,col='darkorange')
#text(x=rowSums(betas),y=c(-700,-1000,-2200,-1700,-1100,-1800),labels=modes,srt=90)
text(x=rowSums(betas),y=c(-130,-160,-110,-170,-120,-170),labels=modes,srt=90)
abline(v=1,lty=2,col='darkgray'); abline(h=0,lty=2,col='darkgray'); 
legend(x=1.2,y=-2500,legend=c('Country','Regions'),col=c('darkorange','darkgrey'),pch=c(16,20),pt.cex=c(1.5,1.25),bty='n',cex=1.25)

######################################################################################
## roads
ssg.road.base <- ssg.road.base[ssg.road.base$roadtype!='Motorway/A(M)',]
veh <- 'car/taxi'
sev <- c('Serious','Fatal','Slight')
for(veh in modes){
  fit1 <- glm.nb(count~-1+roadtype+(log(cas_distance)),
                 data=subset(ssg.road.base,cas_severity%in%sev&strike_mode==veh),init.theta=5,control=glm.control(epsilon=1e-3,maxit=100))
  fit2 <- glm.nb(count~-1+roadtype+roadtype:(log(cas_distance)),
                 data=subset(ssg.road.base,cas_severity%in%sev&strike_mode==veh),init.theta=5,control=glm.control(epsilon=1e-3,maxit=100))
  fit3 <- glm.nb(count~-1+roadtype+log(I(cas_distance_sum-cas_distance))+(log(cas_distance)),
                 data=subset(ssg.road.base,cas_severity%in%sev&strike_mode==veh),init.theta=5,control=glm.control(epsilon=1e-3,maxit=100))
  fit4 <- glm.nb(count~-1+roadtype+log(I(cas_distance_sum-cas_distance))+roadtype:(log(cas_distance)),
                 data=subset(ssg.road.base,cas_severity%in%sev&strike_mode==veh),init.theta=5,control=glm.control(epsilon=1e-3,maxit=100))
  fit5 <- glm.nb(count~-1+roadtype+roadtype:log(I(cas_distance_sum-cas_distance))+(log(cas_distance)),
                 data=subset(ssg.road.base,cas_severity%in%sev&strike_mode==veh),init.theta=5,control=glm.control(epsilon=1e-3,maxit=100))
  fit6 <- glm.nb(count~-1+roadtype+roadtype:log(I(cas_distance_sum-cas_distance))+roadtype:(log(cas_distance)),
                 data=subset(ssg.road.base,cas_severity%in%sev&strike_mode==veh),init.theta=5,control=glm.control(epsilon=1e-3,maxit=100))
  print(AIC(fit1,fit2,fit3,fit4,fit5,fit6)[,2])
}
print((fit))
AIC((fit))


######################################################################################





