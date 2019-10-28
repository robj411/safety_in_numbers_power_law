library(stringr)
library(dplyr)
library(gam)
library(readODS)
setwd('~/overflow_dropbox/safety_in_numbers_power_law/england_LAs/')

summary_counts <- readRDS('data/england_models_all_data.Rds')

model <- glm(count~log(I(dens*population/total_travel)),data=subset(summary_counts,Year==2011),family=poisson(),offset=log(I(dens*population/total_travel)))
summary(model)
model <- glm(count~log(population),data=subset(summary_counts,Year==2011),family=poisson(),offset=log(population))
summary(model)
summary(glm(whw_ksi_bike~log(population),data=subset(summary_counts,Year==2011&Region_Name!='London'),family=poisson(),offset=log(population)))
model <- glm(nov~log(population),data=subset(summary_counts,Year==2011),family=poisson(),offset=log(population))
summary(model)
model <- glm(count~log(total_travel),data=summary_counts,family=poisson(),offset=log(total_travel))
summary(model)
model <- glm(whw~log(total_travel),data=summary_counts,family=poisson(),offset=log(total_travel))
summary(model)
model <- glm(nov~log(total_travel),data=summary_counts,family=poisson(),offset=log(total_travel))
summary(model)
model <- glm(count~log(All.Motor.Vehicles),data=summary_counts,family=poisson(),offset=log(All.Motor.Vehicles))
summary(model)
model <- glm(count~log(Pedal.Cycles),data=summary_counts,family=poisson(),offset=log(Pedal.Cycles))
summary(model)
model <- glm(total_travel~log(population),data=subset(summary_counts,Year==2011),family=gaussian(link=log),offset=log(population))
summary(model)

{x11(height=10); par(mfrow=c(6,2),mar=c(5,5,1,1))
  xvals <- c(7,24)
  ylabels <- c(', OV',', NOV')
  xlabels <- c('Travel','Population')
  extension <- c('','_ksi','_fatal')
  lab_extension <- c('Total','KSI','Fatal')
  for(j in 1:2)
    for(k in 1:3)
      for(i in 1:2){
        type <- paste0(c('whw','nov')[i],extension[k])
        off <- c('total_travel','population')[j]
        form <- paste0(type,'~log(',off,')')
        subsum <- summary_counts
        if(off=='population') subsum <- subset(summary_counts,Year==2011)
        plot(log(subsum[[off]]),log(subsum[[type]]),pch=16,col='grey',
             frame=F,xlab=xlabels[j],ylab=paste0(lab_extension[k],ylabels[i],sep=' '),cex.lab=1.5,cex.axis=1.5)
        model <- glm(as.formula(form),data=subsum,family=poisson(),offset=log(subsum[[off]]))
        lines(xvals,model$coefficients[1]+(1+model$coefficients[2])*xvals,col='navyblue',lwd=2)
        lines(xvals,model$coefficients[1]+(1+model$coefficients[2]+coef(summary(model))[2,2])*xvals,col='navyblue',lwd=2,lty=2)
        lines(xvals,model$coefficients[1]+(1+model$coefficients[2]-coef(summary(model))[2,2])*xvals,col='navyblue',lwd=2,lty=2)
        print(c(type,off,coef(summary(model))[2,4]))
        text(x=1.1*min(log(subsum[[off]])),y=0.9*max(log(subsum[[type]])),round(1+model$coefficients[2],2),col='navyblue',cex=1.5)
      }
}

x11(); {par(mar=c(5,5,2,2)); 
  plot(log(subset(summary_counts,Year==2015&LA_Name!='Isles of Scilly'&Region_Name!='London')$Car),log(subset(summary_counts,Year==2015&Region_Name!='London'&LA_Name!='Isles of Scilly')$Pedal.Cycles),pch=16,col='grey',
     frame=F,xlab='Cars',ylab='Bikes',cex.lab=1.5,cex.axis=1.5,xlim=c(19,23),ylim=c(14,19))
model <- glm(log(Pedal.Cycles)~log(Car),data=subset(summary_counts,Year==2015&LA_Name!='Isles of Scilly'&Region_Name!='London'),family=gaussian(),offset=log(Car))
xval <- c(22,22.5)
yval <- model$coefficients[1]+(1+model$coefficients[2])*xval
arrows(x0=xval[1],x1=xval[2],y0=yval[1],y1=yval[2],col='navyblue',lwd=2)
xval <- c(21,20.5)
yval <- model$coefficients[1]+(1+model$coefficients[2])*xval
arrows(x0=xval[1],x1=xval[2],y0=yval[1],y1=yval[2],col='navyblue',lwd=2)
arrows(x0=21.5,x1=21,y0=17,y1=18,col='navyblue',lwd=2)
}

{x11(height=10); par(mfrow=c(6,2),mar=c(5,5,1,1))
  xvals <- c(7,24)
  ylabels <- c(', OV',', NOV')
  xlabels <- c('Bike travel','Travel','Population')
  extension <- c('','_ksi','_fatal')
  lab_extension <- c('Total','KSI','Fatal')
  for(j in c(1,3))
    for(k in 1:3)
      for(i in 1:2){
        type <- paste0(c('whw','nov')[i],extension[k],'_bike')
        off <- c('Pedal.Cycles','total_travel','population')[j]
        form <- paste0(type,'~log(',off,')')
        subsum <- summary_counts
        if(off=='population') subsum <- subset(summary_counts,Year==2011)
        plot(log(subsum[[off]]),log(subsum[[type]]),pch=16,col='grey',
             frame=F,xlab=xlabels[j],ylab=paste0(lab_extension[k],ylabels[i],sep=' '),cex.lab=1.5,cex.axis=1.5)
        model <- glm(as.formula(form),data=subsum,family=poisson(),offset=log(subsum[[off]]))
        lines(xvals,model$coefficients[1]+(1+model$coefficients[2])*xvals,col='navyblue',lwd=2)
        lines(xvals,model$coefficients[1]+(1+model$coefficients[2]+coef(summary(model))[2,2])*xvals,col='navyblue',lwd=2,lty=2)
        lines(xvals,model$coefficients[1]+(1+model$coefficients[2]-coef(summary(model))[2,2])*xvals,col='navyblue',lwd=2,lty=2)
        print(c(type,off,coef(summary(model))[2,4]))
        text(x=1.1*min(log(subsum[[off]])),y=0.9*max(log(subsum[[type]])),round(1+model$coefficients[2],2),col='navyblue',cex=1.5)
      }
}

summary_counts$PD <- summary_counts$Region_Name!='London'
j <- k <- i <- 1
{x11(height=3,width=8); par(mfrow=c(1,3),mar=c(5,5,2,2))
  lab_extension <- c('log(injuries)','log(KSI)','log(fatalities)')
  for(k in 1:3){
    type <- paste0(c('whw','nov')[i],extension[k],'_bike')
    off <- c('Pedal.Cycles','total_travel','population')[j]
    form <- paste0(type,'~log(',off,')')
    subsum <- subset(summary_counts,!PD)
    if(off=='population') subsum <- subset(summary_counts,Year==2011)
    plot(log(subsum[[off]]),log(subsum[[type]]),pch=16,col='grey',
         frame=F,xlab='log(bike distance)',ylab=lab_extension[k],cex.lab=1.5,cex.axis=1.5)
    model <- glm(as.formula(form),data=subsum,family=poisson(),offset=log(subsum[[off]]))
    lines(xvals,model$coefficients[1]+(1+model$coefficients[2])*xvals,col='navyblue',lwd=2)
    lines(xvals,model$coefficients[1]+(1+model$coefficients[2]+coef(summary(model))[2,2])*xvals,col='navyblue',lwd=2,lty=2)
    lines(xvals,model$coefficients[1]+(1+model$coefficients[2]-coef(summary(model))[2,2])*xvals,col='navyblue',lwd=2,lty=2)
    print(c(type,off,coef(summary(model))[2,4]))
    text(x=1.05*min(log(subsum[[off]])),y=0.9*max(log(subsum[[type]])),round(1+model$coefficients[2],2),col='navyblue',cex=1.5)
  }}
i <- 2
{x11(height=3,width=8); par(mfrow=c(1,3),mar=c(5,5,2,2))
  lab_extension <- c('log(injuries)','log(KSI)','log(fatalities)')
  for(k in 1:3){
    type <- paste0(c('whw','nov')[i],extension[k],'_bike')
    off <- c('Pedal.Cycles','total_travel','population')[j]
    form <- paste0(type,'~log(',off,')')
    subsum <- subset(summary_counts,!PD)
    if(off=='population') subsum <- subset(summary_counts,Year==2011)
    plot(log(subsum[[off]]),log(subsum[[type]]),pch=16,col='grey',
         frame=F,xlab='log(bike distance)',ylab=lab_extension[k],cex.lab=1.5,cex.axis=1.5)
    model <- glm(as.formula(form),data=subsum,family=poisson(),offset=log(subsum[[off]]))
    lines(xvals,model$coefficients[1]+(1+model$coefficients[2])*xvals,col='navyblue',lwd=2)
    lines(xvals,model$coefficients[1]+(1+model$coefficients[2]+coef(summary(model))[2,2])*xvals,col='navyblue',lwd=2,lty=2)
    lines(xvals,model$coefficients[1]+(1+model$coefficients[2]-coef(summary(model))[2,2])*xvals,col='navyblue',lwd=2,lty=2)
    print(c(type,off,coef(summary(model))[2,4]))
    text(x=1.05*min(log(subsum[[off]])),y=0.9*max(log(subsum[[type]])),round(1+model$coefficients[2],2),col='navyblue',cex=1.5)
  }}

non_london <- subset(summary_counts,PD&LA_Name!='Isles of Scilly'&Year==2011)
car_mod <- glm(log(Car)~log(Pedal.Cycles),data=non_london)
non_london$logcarpred <- car_mod$fitted.values
non_london$logcarresidual <- log(non_london$Car) - non_london$logcarpred
bike_mod <- glm(log(Pedal.Cycles)~log(Car),data=non_london)
non_london$logbikepred <- bike_mod$fitted.values
non_london$logbikeresidual <- log(non_london$Pedal.Cycles) - non_london$logbikepred
par(mfrow=c(2,2))
plot(log(non_london$Pedal.Cycles),log(non_london$Car),pch=16,col='grey',
     frame=F,xlab='log(bike distance)',ylab='log(car distance)',cex.lab=1.5,cex.axis=1.5)
plot(log(non_london$Pedal.Cycles),non_london$logcarresidual,pch=16,col='grey',
     frame=F,xlab='log(bike distance)',ylab='Car residual',cex.lab=1.5,cex.axis=1.5)
model <- glm(whw_bike~log(Pedal.Cycles)+log(Car),family=poisson,data=non_london,offset=log(Car))
model <- glm(whw_bike~log(Car)+logbikeresidual,family=poisson,data=non_london,offset=log(Car))
plot(log(non_london$Pedal.Cycles),log(non_london$whw_ksi_bike),pch=16,col='grey',
     frame=F,xlab='log(bike distance)',ylab='Injuries',cex.lab=1.5,cex.axis=1.5)
plot(log(model$fitted.values),log(non_london$whw_ksi_bike),pch=16,col='grey',
     frame=F,xlab='Fitted injuries',ylab='Injuries',cex.lab=1.5,cex.axis=1.5)

par(mfrow=c(2,2))
plot(log(summary_counts$Pedal.Cycles),log(summary_counts$count))
plot(log(summary_counts$All.Motor.Vehicles),log(summary_counts$count))
plot(log(summary_counts$total_travel),log(summary_counts$count))
plot(log(summary_counts$population),log(summary_counts$count))

car_mod <- glm(log(Car)~log(Pedal.Cycles),data=summary_counts)
summary_counts$logcarpred <- car_mod$fitted.values
summary_counts$logcarresidual <- log(summary_counts$Car) - summary_counts$logcarpred
bike_mod <- glm(log(Pedal.Cycles)~log(Car),data=summary_counts)
summary_counts$logbikepred <- bike_mod$fitted.values
summary_counts$logbikeresidual <- log(summary_counts$Pedal.Cycles) - summary_counts$logbikepred

non_london <- subset(summary_counts,PD&LA_Name!='Isles of Scilly')

ext <- c('','_ksi','_fatal')
cov <- c('','+LA_Name','+LA_Name+logcarresidual','+logcarresidual')
betas <- pvals <- matrix(0,nrow=3,ncol=4)
for(i in 1:3) for(j in 1:4){
  term <- 'log(Pedal.Cycles)'#ns(Pedal.Cycles, df = 4)'
  form <- paste0('whw',ext[i],'_bike~',term,cov[j])
  model <- glm(as.formula(form),data=subset(non_london,count>0),family=poisson())
  betas[i,j] <- model$coefficients[2]
  pvals[i,j] <- coef(summary(model))[2,4]
  plot.Gam(model,terms=term)
}
summary(model)

model <- glm(whw_ksi_bike~log(Pedal.Cycles),data=subset(summary_counts,count>10),family=poisson())
summary(model)
model <- glm(whw_ksi_bike~log(Pedal.Cycles)+logcarresidual,data=non_london,family=poisson(),offset=log(Pedal.Cycles))
summary(model)
plot(log(non_london$whw_ksi_bike),log(model$fitted.values))
plot((non_london$whw_ksi_bike),(model$fitted.values))
plot(log(non_london$Pedal.Cycles),log(non_london$whw_bike/non_london$Pedal.Cycles))
plot(log(non_london$Pedal.Cycles),log(non_london$whw_ksi_bike/non_london$Pedal.Cycles))
plot(log(non_london$Pedal.Cycles),log(non_london$whw_fatal_bike/non_london$Pedal.Cycles))
model <- glm(whw_ksi_bike~log(Pedal.Cycles)+LA_Name,data=non_london,family=poisson(),offset=log(Pedal.Cycles))
summary(model)
plot(log(non_london$whw_ksi_bike),log(model$fitted.values))

long_form <- list()
long_form[[1]] <- summary_counts[,c(1,2,4,13,21,23,35)]
long_form[[2]] <- summary_counts[,c(1,2,4,15,21,23,35)]
long_form[[3]] <- summary_counts[,c(1,2,4,17,21,23,35)]
for(i in 1:3) names(long_form[[i]])[4] <- 'injuries'
long_form[[1]]$severity <- 'slight'
long_form[[2]]$severity <- 'serious'
long_form[[3]]$severity <- 'fatal'
long_form[[1]]$injuries <- long_form[[1]]$injuries - long_form[[2]]$injuries
long_form[[2]]$injuries <- long_form[[2]]$injuries - long_form[[3]]$injuries
intable <- do.call('rbind',long_form)
intable$rate <- intable$injuries/intable$Pedal.Cycles
for(sev in c('slight','serious')){#,'fatal')){
  model <- glm(injuries~log(Pedal.Cycles),data=subset(intable,severity%in%sev&Year==2011),family=poisson(),offset=log(Pedal.Cycles))
  model2 <- glm(injuries~1,data=subset(intable,severity%in%sev&Year==2011),family=poisson(),offset=log(Pedal.Cycles))
  plot(log(model$data$Pedal.Cycles),log(model$data$injuries),frame=F,ylab='Injuries',xlab='Cycling',
       cex.axis=1.5,cex.lab=1.5,pch=16,col='grey')
  lines(xvals,model$coefficients[1]+(1+model$coefficients[2])*xvals,col='navyblue',lwd=2)
  lines(xvals,model2$coefficients[1]+xvals,col='navyblue',lwd=2,lty=2)
  #lines(xvals,model$coefficients[1]+(1+model$coefficients[2]+coef(summary(model))[2,2])*xvals,col='navyblue',lwd=2,lty=2)
  #lines(xvals,model$coefficients[1]+(1+model$coefficients[2]-coef(summary(model))[2,2])*xvals,col='navyblue',lwd=2,lty=2)
  plot(log(model$data$Pedal.Cycles),log(model$data$injuries/model$data$Pedal.Cycles),frame=F,ylab='Injuries per cyclist',xlab='Cycling',
       cex.axis=1.5,cex.lab=1.5,pch=16,col='grey')
  lines(xvals,model$coefficients[1]+(1+model$coefficients[2])*xvals-xvals,col='navyblue',lwd=2)
  lines(xvals,model2$coefficients[1]+xvals-xvals,col='navyblue',lwd=2,lty=2)
  #lines(xvals,model$coefficients[1]+(1+model$coefficients[2]+coef(summary(model))[2,2])*xvals-xvals,col='navyblue',lwd=2,lty=2)
  #lines(xvals,model$coefficients[1]+(1+model$coefficients[2]-coef(summary(model))[2,2])*xvals-xvals,col='navyblue',lwd=2,lty=2)
  print(summary(model))
}
plot(log(intable$Pedal.Cycles),log(intable$injuries))

model <- glm(count~log(total_travel),data=subset(summary_counts,count>10),family=poisson(),offset=log(total_travel))
summary(model)
model <- glm(count~log(All.Motor.Vehicles),data=subset(summary_counts,count>10),family=poisson(),offset=log(All.Motor.Vehicles))
summary(model)
model <- glm(count~log(Pedal.Cycles),data=subset(summary_counts,count>10),family=poisson(),offset=log(Pedal.Cycles))
summary(model)

{par(mfrow=c(2,2),mar=c(5,5,2,2))
  plot(log(subset(summary_counts,count>10&Year==2011)$All.Motor.Vehicles),log(subset(summary_counts,count>10&Year==2011)$count),frame=F,cex.lab=1.5,cex.axis=1.5,pch=15,xlab='Total distance',ylab='Count')
  points(log(subset(summary_counts,count>10&Year==2011&PD)$All.Motor.Vehicles),
         log(subset(summary_counts,count>10&Year==2011&PD)$count),pch=15,col='hotpink')
  plot(log(subset(summary_counts,count>10&Year==2011)$population),log(subset(summary_counts,count>10&Year==2011)$All.Motor.Vehicles),pch=15,frame=F,cex.lab=1.5,cex.axis=1.5,ylab='Total distance',xlab='Population')
  points(log(subset(summary_counts,count>10&Year==2011&PD)$population),
         log(subset(summary_counts,count>10&Year==2011&PD)$All.Motor.Vehicles),pch=15,col='hotpink')
  plot(log(subset(summary_counts,count>10&Year==2011)$population),log(subset(summary_counts,count>10&Year==2011)$count),pch=15,frame=F,cex.lab=1.5,cex.axis=1.5,ylab='Count',xlab='Population')
  points(log(subset(summary_counts,count>10&Year==2011&PD)$population),
         log(subset(summary_counts,count>10&Year==2011&PD)$count),pch=15,col='hotpink')
  plot(log(subset(summary_counts,count>10&Year==2011)$dens),log(subset(summary_counts,count>10&Year==2011)$count),pch=15,frame=F,cex.lab=1.5,cex.axis=1.5,xlab='Population density',ylab='Count')
  points(log(subset(summary_counts,count>10&Year==2011&PD)$dens),
         log(subset(summary_counts,count>10&Year==2011&PD)$count),pch=15,col='hotpink')
}
{par(mfrow=c(2,2))
  plot(log(subset(summary_counts,count>10&Year==2011)$Pedal.Cycles),log(subset(summary_counts,count>10&Year==2011)$whw_fatal),pch=15,xlab='Cycle distance',ylab='Count')
  points(log(subset(summary_counts,count>10&Year==2011&PD)$Pedal.Cycles),
         log(subset(summary_counts,count>10&Year==2011&PD)$whw_fatal),pch=15,col='hotpink')
  
  plot(log(subset(summary_counts,count>10&Year==2011)$All.Motor.Vehicles),log(subset(summary_counts,count>10&Year==2011)$whw_fatal),pch=15,xlab='Total distance',ylab='Count')
  points(log(subset(summary_counts,count>10&Year==2011&PD)$All.Motor.Vehicles),
         log(subset(summary_counts,count>10&Year==2011&PD)$whw_fatal),pch=15,col='hotpink')
  
  plot(log(subset(summary_counts,count>10&Year==2011)$population),log(subset(summary_counts,count>10&Year==2011)$All.Motor.Vehicles),pch=15,ylab='Total distance',xlab='Population')
  points(log(subset(summary_counts,count>10&Year==2011&PD)$population),
         log(subset(summary_counts,count>10&Year==2011&PD)$All.Motor.Vehicles),pch=15,col='hotpink')
  
  plot(log(subset(summary_counts,count>10&Year==2011)$population),log(subset(summary_counts,count>10&Year==2011)$whw_fatal),pch=15,ylab='Count',xlab='Population')
  points(log(subset(summary_counts,count>10&Year==2011&PD)$population),
         log(subset(summary_counts,count>10&Year==2011&PD)$whw_fatal),pch=15,col='hotpink')
}



x11(); plot(log(subset(summary_counts,Year==2011)$population),log(subset(summary_counts,Year==2011)$count))
text(subset(summary_counts,Year==2011&LA_Name=='Cambridgeshire')$LA_Name,
     x=log(subset(summary_counts,Year==2011&LA_Name=='Cambridgeshire')$population),
     y=log(subset(summary_counts,Year==2011&LA_Name=='Cambridgeshire')$count))

superset <- summary_counts


london <- subset(superset, Region_Name=='London')

plot(log(subset(superset,Year==2011)$cyclist_density),log(subset(superset,Year==2011)$whw_bike))
plot(log(london$Pedal.Cycles),log(london$whw_bike)-log(london$Pedal.Cycles),
     frame=F,ylab='Injuries per cyclist',xlab='Cycling',cex.axis=1.5,cex.lab=1.5,pch=16,col='grey')
text('Injuries',x=15,y=-12.5,col='navyblue',cex=1.5)
plot(log(london$cyclist_density),log(london$whw_bike)-log(subset(london,Year==2011)$Pedal.Cycles),
     frame=F,ylab='Injuries per cyclist',xlab='Cycling density',cex.axis=1.5,cex.lab=1.5,pch=16,col='grey')
text('Injuries',x=9,y=-12.5,col='navyblue',cex=1.5)
plot(log(london$Pedal.Cycles),log(london$whw_ksi_bike)-log(london$Pedal.Cycles),
     frame=F,ylab='Injuries per cyclist',xlab='Cycling',cex.axis=1.5,cex.lab=1.5,pch=16,col='grey')
text('KSI',x=15,y=-15.5,col='navyblue',cex=1.5)
plot(log(london$cyclist_density),log(london$whw_ksi_bike)-log(london$Pedal.Cycles),
     frame=F,ylab='Injuries per cyclist',xlab='Cycling density',cex.axis=1.5,cex.lab=1.5,pch=16,col='grey')
text('KSI',x=9,y=-15.5,col='navyblue',cex=1.5)


london$relCar <- sapply(1:nrow(london),function(x)london$Car[x]/london$Car[london$LA_Name==london$LA_Name[x]&london$Year==2005])
london$relBike <- sapply(1:nrow(london),function(x)london$Pedal.Cycles[x]/london$Pedal.Cycles[london$LA_Name==london$LA_Name[x]&london$Year==2005])


od_data <- readRDS('~/overflow_dropbox/ITHIM/InjuryModel/od_data.Rds')
commute_prop <- c(0.0462,0.1178)
urbanicity <- read_ods('~/overflow_dropbox/ITHIM/InjuryModel/urban_2011.ods',col_names=T)
urbanicity2 <- read.csv('~/overflow_dropbox/ITHIM/InjuryModel/urban_county.csv')

superset <- superset[superset$code%in%c(urbanicity$LAD11CD,as.character(urbanicity2$CTY11CD)),]
superset$ruralpercent <- 0
for(i in 1:length(unique(superset$code))){
  cd <- unique(superset$code)[i]
  if(cd%in%urbanicity$LAD11CD){
    pc <- urbanicity[which(urbanicity$LAD11CD==cd),9]
  }else{
    pc <- urbanicity2[which(urbanicity2$CTY11CD==cd),11]
  }
  superset$ruralpercent[superset$code==cd] <- pc/100
}
  
lad_to_county <- read.csv('~/overflow_dropbox/ITHIM/InjuryModel/LAD_to_county.csv')

  
superset$Walk <- 0
for(i in 1:length(unique(superset$code))){
  cd <- unique(superset$code)[i]
  if(!cd%in%unique(od_data[[2]]$lad11cd1)){
    cd <- lad_to_county$LAD16CD[lad_to_county$CTY16CD==cd]
  }
  walkset <- subset(od_data[[2]],lad11cd1%in%cd)
  dists <- sum(walkset$foot*walkset$rf_dist_km)*5*(52-3)*2
  superset$Walk[superset$code==unique(superset$code)[i]] <- dists/(superset$ruralpercent[i]*commute_prop[1]+(1-superset$ruralpercent[i])*commute_prop[2])
}
  
superset$total_travel_plus_Walk <- superset$total_travel + superset$Walk

{par(mfrow=c(2,2))
  plot(log(subset(superset,count>10&Year==2011)$dens),log(subset(superset,count>10&Year==2011)$count),pch=15,xlab='Population density',ylab='Count')
  points(log(subset(superset,count>10&Year==2011&PD)$dens),
         log(subset(superset,count>10&Year==2011&PD)$count),pch=15,col='hotpink')
  
  plot(log(subset(superset,count>10&Year==2011)$total_travel_plus_Walk),log(subset(superset,count>10&Year==2011)$count),pch=15,xlab='Total distance',ylab='Count')
  points(log(subset(superset,count>10&Year==2011&PD)$total_travel_plus_Walk),
         log(subset(superset,count>10&Year==2011&PD)$count),pch=15,col='hotpink')
  
  plot(log(subset(superset,count>10&Year==2011)$population),log(subset(superset,count>10&Year==2011)$total_travel_plus_Walk),pch=15,ylab='Total distance',xlab='Population')
  points(log(subset(superset,count>10&Year==2011&PD)$population),
         log(subset(superset,count>10&Year==2011&PD)$total_travel_plus_Walk),pch=15,col='hotpink')
  
  plot(log(subset(superset,count>10&Year==2011)$population),log(subset(superset,count>10&Year==2011)$count),pch=15,ylab='Count',xlab='Population')
  points(log(subset(superset,count>10&Year==2011&PD)$population),
         log(subset(superset,count>10&Year==2011&PD)$count),pch=15,col='hotpink')
}  
{par(mfrow=c(2,2))
  plot(log(subset(superset,count>10&Year==2011)$AB),log(subset(superset,count>10&Year==2011)$count),pch=15,xlab='Road length',ylab='Count')
  points(log(subset(superset,count>10&Year==2011&PD)$AB),
         log(subset(superset,count>10&Year==2011&PD)$count),pch=15,col='hotpink')
  
  plot(log(subset(superset,count>10&Year==2011)$total_travel_plus_Walk),log(subset(superset,count>10&Year==2011)$count),pch=15,xlab='Total distance',ylab='Count')
  points(log(subset(superset,count>10&Year==2011&PD)$total_travel_plus_Walk),
         log(subset(superset,count>10&Year==2011&PD)$count),pch=15,col='hotpink')
  
  plot(log(subset(superset,count>10&Year==2011)$total_travel_plus_Walk/subset(superset,count>10&Year==2011)$AB),log(subset(superset,count>10&Year==2011)$count),
       pch=15,xlab='Road density',ylab='Count')
  points(log(subset(superset,count>10&Year==2011&PD)$total_travel_plus_Walk/subset(superset,count>10&Year==2011&PD)$AB),
         log(subset(superset,count>10&Year==2011&PD)$count),pch=15,col='hotpink')
  
  plot(log(subset(superset,count>10&Year==2011)$population),log(subset(superset,count>10&Year==2011)$count),pch=15,ylab='Count',xlab='Population')
  points(log(subset(superset,count>10&Year==2011&PD)$population),
         log(subset(superset,count>10&Year==2011&PD)$count),pch=15,col='hotpink')
}  

xvals<-1:30
{x11(height=5,width=5); par(mfrow=c(2,2),mar=c(5,5,2,2))
  plot(log(london$Car),log(london$Pedal.Cycles),pch=16,col='grey',
       frame=F,xlab='log(car distance)',ylab='log(bike distance)',cex.lab=1.5,cex.axis=1.5)
  model <- glm(Pedal.Cycles~log(london$Car),data=london,family=poisson())
  lines(xvals,model$coefficients[1]+(model$coefficients[2])*xvals,col='navyblue',lwd=2)
  print(model$coefficients[2])
  plot(log(london$Pedal.Cycles+london$Car),log(london$Car),pch=16,col='grey',
       frame=F,xlab='log(total distance)',ylab='log(car distance)',cex.lab=1.5,cex.axis=1.5)
  model <- glm(Car~log(london$Pedal.Cycles+london$Car),data=london,family=poisson())
  lines(xvals,model$coefficients[1]+(model$coefficients[2])*xvals,col='navyblue',lwd=2)
  print(model$coefficients[2])
  plot(log(london$Car),log(london$whw_bike),pch=16,col='grey',
       frame=F,xlab='log(car distance)',ylab='log(bike injuries)',cex.lab=1.5,cex.axis=1.5)
  model <- glm(whw_bike~log(london$Car),data=london,family=poisson())
  lines(xvals,model$coefficients[1]+(model$coefficients[2])*xvals,col='navyblue',lwd=2)
  print(model$coefficients[2])
  plot(log(london$Pedal.Cycles),log(london$whw_bike),pch=16,col='grey',
       frame=F,xlab='log(bike distance)',ylab='log(bike injuries)',cex.lab=1.5,cex.axis=1.5)
  model <- glm(whw_bike~log(Pedal.Cycles),data=london,family=poisson())
  lines(xvals,model$coefficients[1]+(model$coefficients[2])*xvals,col='navyblue',lwd=2)
  print(model$coefficients[2])
  #text(x=1.05*min(log(subsum[[off]])),y=0.9*max(log(subsum[[type]])),round(1+model$coefficients[2],2),col='navyblue',cex=1.5)
}
{x11(height=5,width=5); par(mfrow=c(2,2),mar=c(5,5,2,2))
  plot(log(london$Car),log(london$Pedal.Cycles),pch=16,col='grey',
       frame=F,xlab='log(car distance)',ylab='log(bike distance)',cex.lab=1.5,cex.axis=1.5)
  model <- glm(Pedal.Cycles~log(london$Car),data=london,family=poisson())
  lines(xvals,model$coefficients[1]+(model$coefficients[2])*xvals,col='navyblue',lwd=2)
  print(model$coefficients[2])
  plot(log(london$Pedal.Cycles+london$Car),log(london$Car),pch=16,col='grey',
       frame=F,xlab='log(total distance)',ylab='log(car distance)',cex.lab=1.5,cex.axis=1.5)
  model <- glm(Car~log(london$Pedal.Cycles+london$Car),data=london,family=poisson())
  lines(xvals,model$coefficients[1]+(model$coefficients[2])*xvals,col='navyblue',lwd=2)
  print(model$coefficients[2])
  plot(log(london$Car),log(london$whw_ksi_bike),pch=16,col='grey',
       frame=F,xlab='log(car distance)',ylab='log(bike injuries)',cex.lab=1.5,cex.axis=1.5)
  model <- glm(whw_ksi_bike~log(london$Car),data=london,family=poisson())
  lines(xvals,model$coefficients[1]+(model$coefficients[2])*xvals,col='navyblue',lwd=2)
  print(model$coefficients[2])
  plot(log(london$Pedal.Cycles+london$Car),log(london$whw_ksi_bike),pch=16,col='grey',
       frame=F,xlab='log(bike distance)',ylab='log(bike injuries)',cex.lab=1.5,cex.axis=1.5)
  model <- glm(whw_ksi_bike~log(Pedal.Cycles+london$Car),data=london,family=poisson())
  lines(xvals,model$coefficients[1]+(model$coefficients[2])*xvals,col='navyblue',lwd=2)
  print(model$coefficients[2])
  #text(x=1.05*min(log(subsum[[off]])),y=0.9*max(log(subsum[[type]])),round(1+model$coefficients[2],2),col='navyblue',cex=1.5)
}
{pdf('outputs/county_linearity.pdf',height=2.5); par(mfrow=c(1,3),mar=c(5,5,2,2))
  plot(log(subset(non_london,Year==2011)$population),log(subset(non_london,Year==2011)$whw_ksi_bike),pch=16,col='grey',
       frame=F,xlab='log(population)',ylab='log(bike KSI)',cex.lab=1.5,cex.axis=1.5)
  plot(log(subset(non_london,Year==2011)$population),log(subset(non_london,Year==2011)$Car),pch=16,col='grey',
       frame=F,xlab='log(population)',ylab='log(car distance)',cex.lab=1.5,cex.axis=1.5)
  plot(log(subset(non_london,Year==2011)$population),log(subset(non_london,Year==2011)$Pedal.Cycles),pch=16,col='grey',
       frame=F,xlab='log(population)',ylab='log(bike distance)',cex.lab=1.5,cex.axis=1.5)
dev.off()
  }
summary(glm(whw_ksi_bike~log(population),family=poisson,data=subset(non_london,Year==2011)))
summary(glm(whw_ksi_bike~log(Pedal.Cycles)+log(Car),family=poisson,data=subset(non_london,Year==2011)))

library(latex2exp)
beta_1<-c(0.52,0.76,0.29,0.36,0.4,0.73,0.5,0.27,0.32,0.62,0.55,0.64,0.58,0.35); 
beta_2<-c(0.65,0.35,0.09,0.2,.044,0.48,0.56,0.34,0.39,0.26,0.44,0.53,0.65,0.67)
bmean <- mean(beta_1+beta_2)
bmedian <- median(beta_1+beta_2)
{x11(); par(mar=c(5,5,2,2))
plot(density(beta_1+beta_2),main='',xlab=TeX('$\\beta_1+\\beta_2$'),frame=F,cex.lab=1.5,cex.axis=1.5,lwd=2,col='navyblue')
abline(v=bmean,col='darkorange2',lwd=2)
abline(v=bmedian,col='turquoise',lwd=2)
text('mean',x=bmean-0.15,y=0.1,col='darkorange2',cex=1.5)
text('median',x=bmedian+0.18,y=0.1,col='turquoise',cex=1.5)

}

mod<-glm(whw_bike~log(cyclist_density)+I(log(Car/AB)),family=poisson(),offset=log(Pedal.Cycles)+log(Car),data=london)
plot(london$whw_bike/london$Pedal.Cycles,mod$fitted.values/london$Pedal.Cycles)
mod<-glm(whw_ksi_bike~log(cyclist_density)+I(log(Car/AB)),family=poisson(),offset=log(Pedal.Cycles)+log(Car),data=london)
plot(london$whw_ksi_bike/london$Pedal.Cycles,mod$fitted.values/london$Pedal.Cycles)


{par(mfrow=c(2,2))
  plot(log(subset(superset,count>10&Year==2011)$Pedal.Cycles),log(subset(superset,count>10&Year==2011)$whw_bike),col='navyblue',pch=15,xlab='Cycle distance',ylab='Count')
  points(log(subset(superset,count>10&Year==2011&!PD)$Pedal.Cycles),
         log(subset(superset,count>10&Year==2011&!PD)$whw_bike),pch=15,col='hotpink')
  
  plot(log(subset(superset,count>10&Year==2011)$All.Motor.Vehicles),log(subset(superset,count>10&Year==2011)$whw_bike),col='navyblue',pch=15,xlab='Total distance',ylab='Count')
  points(log(subset(superset,count>10&Year==2011&!PD)$All.Motor.Vehicles),
         log(subset(superset,count>10&Year==2011&!PD)$whw_bike),pch=15,col='hotpink')
  
  plot(log(subset(superset,count>10&Year==2011)$Pedal.Cycles/subset(superset,count>10&Year==2011)$AB),
       log(subset(superset,count>10&Year==2011)$whw_bike),col='navyblue',pch=15,xlab='Cycle density',ylab='Count')
  points(log(subset(superset,count>10&Year==2011&!PD)$Pedal.Cycles/subset(superset,count>10&Year==2011&!PD)$AB),
         log(subset(superset,count>10&Year==2011&!PD)$whw_bike),pch=15,col='hotpink')
  
  plot(log(subset(superset,count>10&Year==2011&!PD)$Pedal.Cycles/subset(superset,count>10&Year==2011&!PD)$AB),
       log(subset(superset,count>10&Year==2011&!PD)$whw_bike/subset(superset,count>10&Year==2011&!PD)$Pedal.Cycles),col='hotpink',pch=15,
       xlab='Cycle density',ylab='Count per cyclist')
  
}
