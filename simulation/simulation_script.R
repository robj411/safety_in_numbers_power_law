setwd('~/overflow_dropbox/safety_in_numbers_power_law/simulation/')
library(latex2exp)
library(reticulate)
use_python("/usr/local/bin/python")
reticulate::source_python('simulate_city.py')
repetitions <- 50
sequence <- seq(5,14,by=1)
seqsq <- sequence^2
seqlength <- length(sequence)
windowscale <- 20

###############################################################################

inputs <- data.frame(motorists=(rpois(repetitions*length(seqsq),seqsq)),
                     cyclists=(rpois(repetitions*length(seqsq),seqsq)),
                     windowsize=(rep(sequence*windowscale,repetitions)),
                     showgame=rep('F',seqlength*repetitions))

inputs$count <- 0
stepsize <- 5
if(file.exists('results/results.Rds')){
  inputs <- readRDS('results/results.Rds')
}else{
  for(j in 1:nrow(inputs)) 
    ## calling order = WINDOW_WIDTH,WINDOW_HEIGHT,STEP_SIZE,PROBABILITY,cyclists,motorists,showwindow
    inputs$count[j] <- main_function(as.integer(inputs[j,3]*stepsize),as.integer(inputs[j,3]*stepsize),as.integer(stepsize),as.double(1),as.integer(inputs[j,1]),as.integer(inputs[j,2]),'F')
  saveRDS(inputs,'results/results.Rds')
}


mod <- glm(count~log(motorists)+log(cyclists),family=poisson(),data=inputs,offset=0.5*log(motorists)+0.5*log(cyclists))
coef(summary(mod))
inputs$pred <- mod$fitted.values

results <- inputs
results$collisions_density <- results$count/results$windowsize^2
results$cyclist_density <- results$cyclists/results$windowsize^2
results$motorist_density <- results$motorists/results$windowsize^2
results$collisions_per_cyclist <- results$count/results$cyclists
results$collisions_per_motorist <- results$count/results$motorists
results$collisions_per_motorist_per_cyclist <- results$count/results$motorists/results$cyclists
results$collisions_per_sq_motorist_per_sq_cyclist <- results$count/sqrt(results$motorists)/sqrt(results$cyclists)
results

mod <- glm(count~log(motorists)+log(cyclists),family=poisson(),data=results,offset=0.5*log(motorists)+0.5*log(cyclists))
coef(summary(mod))
results$pred <- mod$fitted.values

window_positions <- sequence*windowscale
window_labels <- paste0(window_positions,'^2')
q1 <- sapply(unique(results$windowsize),function(x)quantile(subset(results,windowsize==x)$count,0.05))
q9 <- sapply(unique(results$windowsize),function(x)quantile(subset(results,windowsize==x)$count,0.95))
{x11(width=10); par(mar=c(5,5,1,1))
plot(window_positions^2,sapply(unique(results$windowsize),function(x)mean(subset(results,windowsize==x)$count)),col='navyblue',
     frame=F,cex=1.5,pch=15,xlab='Frame size',ylab='Number of collisions',ylim=range(c(0,q1,q9)),cex.lab=1.5,cex.axis=1.5,xaxt='n')
for(i in 1:length(q1)) lines(c(unique(results$windowsize)[i],unique(results$windowsize)[i])^2,c(q1[i],q9[i]),col='navyblue')
axis(1,at=window_positions^2,labels=TeX(window_labels),cex.axis=1.5)
#abline(h=pred100,col='grey',lwd=2.5)
#legend(pch=c(15,NA),lwd=c(1,2.5),bty='n',legend=c('Simulation (90% CI)','Prediction'),x=10000,y=200,col=c('navyblue','grey'),cex=1.25)
}

############

pred100 <- predict(mod,newdata=data.frame(cyclists=100,motorists=100),type='response')

results100 <- data.frame(motorists=rep(100,repetitions*seqlength),
                         cyclists=rep(100,repetitions*seqlength),
                         windowsize=(rep(sequence*windowscale,repetitions)),
                         showgame=rep('F',seqlength*repetitions))
if(file.exists('results/results100.Rds')){
  results100 <- readRDS('results/results100.Rds')
}else{
  results100$count <- 0
  for(j in 1:nrow(inputs))
    ## calling order = WINDOW_WIDTH,WINDOW_HEIGHT,STEP_SIZE,PROBABILITY,cyclists,motorists,showwindow
    results100$count[j] <- main_function(as.integer(results100[j,3]*stepsize),as.integer(results100[j,3]*stepsize),as.integer(stepsize),as.double(1),as.integer(results100[j,1]),as.integer(results100[j,2]),'F')
  saveRDS(inputs,'results/results100.Rds')
}

q1 <- sapply(unique(results100$windowsize),function(x)quantile(subset(results100,windowsize==x)$collisions,0.05))
q9 <- sapply(unique(results100$windowsize),function(x)quantile(subset(results100,windowsize==x)$collisions,0.95))
{x11(width=11); par(mar=c(5,5,1,1))
plot(window_positions^2,sapply(unique(results100$windowsize),function(x)mean(subset(results100,windowsize==x)$collisions)),col='navyblue',
     frame=F,cex=1.5,pch=15,xlab='Frame size',ylab='Number of collisions',ylim=range(c(q1,q9)),cex.lab=1.5,cex.axis=1.5,xaxt='n')
for(i in 1:length(q1)) lines(c(unique(results100$windowsize)[i],unique(results100$windowsize)[i])^2,c(q1[i],q9[i]),col='navyblue')
abline(h=pred100,col='hotpink',lwd=2.5,xaxt='n')
axis(1,at=window_positions^2,labels=TeX(window_labels),cex.axis=1.5)
legend(pch=c(15,NA),lwd=c(1,2.5),bty='n',legend=c('Simulation (90% CI)','Prediction'),x=250^2,y=350,col=c('navyblue','hotpink'),cex=1.25)
}


#####50100

aug_seq <- c(10,seqsq,230)
pred50100 <- predict(mod,newdata=data.frame(cyclists=aug_seq,motorists=rep(100,length(aug_seq))),type='response')

results50100 <- data.frame(motorists=rep(100,repetitions*seqlength),
                         cyclists=rpois(repetitions*length(seqsq),seqsq),
                         windowsize=(rep(10*windowscale,repetitions*seqlength)),
                         showgame=rep('F',seqlength*repetitions))

if(file.exists('results/results50100.Rds')){
  results50100 <- readRDS('results/results50100.Rds')
}else{
  results50100$count <- 0
  for(j in 1:nrow(inputs)) 
    ## calling order = WINDOW_WIDTH,WINDOW_HEIGHT,STEP_SIZE,PROBABILITY,cyclists,motorists,showwindow
    results50100$count[j] <- main_function(as.integer(results50100[j,3]*stepsize),as.integer(results50100[j,3]*stepsize),as.integer(stepsize),as.double(1),as.integer(results50100[j,1]),as.integer(results50100[j,2]),'F')
  saveRDS(inputs,'results/results50100.Rds')
}

par(mar=c(5,5,1,1))
plot(results50100$cyclists,results50100$collisions,col='navyblue',xlim=c(0,max(results50100$cyclists)),ylim=c(0,max(results50100$collisions)),
     frame=F,pch=15,xlab='Number of cyclists',ylab='Number of collisions',cex.lab=1.5,cex.axis=1.5)
lines(aug_seq,pred50100,col='hotpink',lwd=3)
legend(pch=c(15,NA),lwd=c(NA,2.5),bty='n',legend=c('Simulation','Prediction'),x=0,y=190,col=c('navyblue','hotpink'),cex=1.25)


#####15050

short_seq <- c(25,50,75,100)
pred15050 <- predict(mod,newdata=data.frame(cyclists=rep(150,length(short_seq)+2),motorists=c(15,short_seq,120)),type='response')

results15050 <- data.frame(motorists=rpois(repetitions*length(short_seq),short_seq),
                         cyclists=rep(150,repetitions*length(short_seq)),
                         windowsize=(rep(10*windowscale,repetitions*length(short_seq))),
                         showgame=rep('F',length(short_seq)*repetitions))

if(file.exists('results/results15050.Rds')){
  results15050 <- readRDS('results/results15050.Rds')
}else{
  results15050$count <- 0
  for(j in 1:nrow(inputs))
    ## calling order = WINDOW_WIDTH,WINDOW_HEIGHT,STEP_SIZE,PROBABILITY,cyclists,motorists,showwindow
    results15050$count[j] <- main_function(as.integer(results15050[j,3]*stepsize),as.integer(results15050[j,3]*stepsize),as.integer(stepsize),as.double(1),as.integer(results15050[j,1]),as.integer(results15050[j,2]),'F')
  saveRDS(inputs,'results/results15050.Rds')
}

par(mar=c(5,5,1,1))
plot(results15050$motorists,results15050$collisions,col='navyblue',xlim=c(0,max(results15050$motorists)),ylim=c(0,max(results15050$collisions)),
     frame=F,pch=15,xlab='Number of motorists',ylab='Number of collisions',cex.lab=1.5,cex.axis=1.5)
lines(c(15,short_seq,120),pred15050,col='hotpink',lwd=3)
legend(pch=c(15,NA),lwd=c(NA,2.5),bty='n',legend=c('Simulation','Prediction'),x=0,y=160,col=c('navyblue','hotpink'),cex=1.25)
lines(short_seq,exp(coef(mod)[1])+100*short_seq,col='grey',lwd=3)

###############################################################################

###############################################################################
## junctions
reticulate::source_python('junction.py')
inputs <- 7:14
nsamples <- 50
counts <- matrix(0,nrow=nsamples,ncol=length(inputs))
for(j in 1:nsamples){
  print(j)
  for(i in inputs)
    counts[j,i-6] <- main_function(as.integer(100/5*25),as.integer(100/5*25),as.integer(25),as.integer(20*i),as.integer(10*i),as.integer(10*i),'F')
}
quantiles <- apply(counts,2,quantile,c(0.1,0.9))
plot(inputs,apply(counts,2,median),ylim=range(quantiles))
for(i in inputs) lines(c(i,i),quantiles[,i-6])
apply(counts,2,median)/inputs


###############################################################################
reticulate::source_python('simulate_city.py')
repetitions <- 50
sequence <- seq(5,14,by=1)
seqsq <- sequence^2
seqlength <- length(sequence)
windowscale <- 20


inputs <- data.frame(motorists=(rpois(repetitions*length(seqsq),seqsq)),
                     cyclists=(rpois(repetitions*length(seqsq),seqsq)),
                     windowsize=(rep(sequence*windowscale,repetitions)),
                     showgame=rep('F',seqlength*repetitions))
inputs$count <- 0
if(file.exists('results/results75.Rds')){
  inputs <- readRDS('results/results75.Rds')
}else{
  for(j in 1:nrow(inputs))
    inputs$count[j] <- main_function(as.integer(inputs[j,3]*5),as.integer(inputs[j,3]*5),as.integer(5),as.double(0.75),as.integer(inputs[j,1]),as.integer(inputs[j,2]),'F')
  saveRDS(inputs,'results/results75.Rds')
}


mod <- glm(count~log(motorists)+log(cyclists),family=poisson(),data=inputs,offset=0.5*log(motorists)+0.5*log(cyclists))
coef(summary(mod))
inputs$pred <- mod$fitted.values

window_positions <- sequence*windowscale
window_labels <- paste0(window_positions,'^2')
q1 <- sapply(unique(inputs$windowsize),function(x)quantile(subset(inputs,windowsize==x)$count,0.05))
q9 <- sapply(unique(inputs$windowsize),function(x)quantile(subset(inputs,windowsize==x)$count,0.95))
{x11(width=10); par(mar=c(5,5,1,1))
  plot(window_positions^2,sapply(unique(inputs$windowsize),function(x)mean(subset(inputs,windowsize==x)$count)),col='navyblue',
       frame=F,cex=1.5,pch=15,xlab='Frame size',ylab='Number of collisions',ylim=range(c(0,q1,q9)),cex.lab=1.5,cex.axis=1.5,xaxt='n')
  for(i in 1:length(q1)) lines(c(unique(inputs$windowsize)[i],unique(inputs$windowsize)[i])^2,c(q1[i],q9[i]),col='navyblue')
  axis(1,at=window_positions^2,labels=TeX(window_labels),cex.axis=1.5)
  #abline(h=pred100,col='grey',lwd=2.5)
  #legend(pch=c(15,NA),lwd=c(1,2.5),bty='n',legend=c('Simulation (90% CI)','Prediction'),x=10000,y=200,col=c('navyblue','grey'),cex=1.25)
}



###################################
preddouble <- predict(mod,newdata=data.frame(cyclists=2*seqsq,motorists=2*seqsq),type='response')

new_inputs <- data.frame(motorists=(rpois(repetitions*length(seqsq),2*seqsq)),
                         cyclists=(rpois(repetitions*length(seqsq),2*seqsq)),
                         windowsize=(rep(sequence*windowscale,repetitions)),
                         showgame=rep('F',seqlength*repetitions))
new_inputs$count <- 0
if(file.exists('results/results75double.Rds')){
  inputs <- readRDS('results/results75double.Rds')
}else{
  inputs <- new_inputs
  for(j in 1:nrow(inputs))
    inputs$count[j] <- main_function(as.integer(inputs[j,3]*5),as.integer(inputs[j,3]*5),as.integer(5),as.double(0.75),as.integer(inputs[j,1]),as.integer(inputs[j,2]),'F')
  saveRDS(inputs,'results/results75double.Rds')
}

q1 <- sapply(unique(inputs$windowsize),function(x)quantile(subset(inputs,windowsize==x)$count,0.05))
q9 <- sapply(unique(inputs$windowsize),function(x)quantile(subset(inputs,windowsize==x)$count,0.95))
{x11(width=10); par(mar=c(5,5,1,1))
  plot(window_positions^2,sapply(unique(inputs$windowsize),function(x)mean(subset(inputs,windowsize==x)$count)),col='navyblue',
       frame=F,cex=1.5,pch=15,xlab='Frame size',ylab='Number of collisions',ylim=range(c(0,q1,q9)),cex.lab=1.5,cex.axis=1.5,xaxt='n')
  for(i in 1:length(q1)) lines(c(unique(inputs$windowsize)[i],unique(inputs$windowsize)[i])^2,c(q1[i],q9[i]),col='navyblue')
  axis(1,at=window_positions^2,labels=TeX(window_labels),cex.axis=1.5)
  lines(window_positions^2,preddouble,col='hotpink',lwd=2.5)
  legend(pch=c(15,NA),lwd=c(1,2.5),bty='n',legend=c('Simulation (90% CI)','Prediction'),x=250^2,y=350,col=c('navyblue','hotpink'),cex=1.25)
}
inputsdouble <- inputs
mod <- glm(count~log(motorists)+log(cyclists),family=poisson(),data=inputsdouble,offset=0.5*log(motorists)+0.5*log(cyclists))
coef(summary(mod))
inputs$pred <- mod$fitted.values

#################################################################
inputs <- readRDS('results/results75.Rds')
mod <- glm(count~log(motorists)+log(cyclists),family=poisson(),data=rbind(inputs,inputsdouble),offset=0.5*log(motorists)+0.5*log(cyclists))
coef(summary(mod))
mod <- glm(count~log(motorists)+log(cyclists)+log(windowsize),family=poisson(),data=rbind(inputs,inputsdouble),offset=0.5*log(motorists)+0.5*log(cyclists))
coef(summary(mod))
mod <- glm(count~log(motorists)+log(cyclists),family=poisson(),data=rbind(inputs,inputsdouble),offset=0.5*log(motorists)+0.5*log(cyclists)+log(windowsize))
coef(summary(mod))
mod <- glm(count~log(motorists)+log(cyclists)+log(windowsize),family=poisson(),data=rbind(inputs,inputsdouble),offset=log(motorists)/3+log(cyclists)/3+log(windowsize)/3)
coef(summary(mod))
mod <- glm(count~I(log(motorists)-log(windowsize^2))+I(log(cyclists)-log(windowsize^2)),family=poisson(),data=rbind(inputs),offset=log(windowsize^2))
coef(summary(mod))

###############################################################################
reticulate::source_python('simulate_city.py')
repetitions <- 50
sequence <- seq(5,14,by=1)
seqsq <- sequence^2
seqlength <- length(sequence)
windowscale <- 20


inputs <- data.frame(motorists=(rpois(repetitions*length(seqsq),seqsq)),
                     cyclists=(rpois(repetitions*length(seqsq),seqsq)),
                     windowsize=(rep(sequence*windowscale,repetitions)),
                     showgame=rep('F',seqlength*repetitions))
inputs$count <- 0
if(file.exists('results/results50.Rds')){
  inputs <- readRDS('results/results50.Rds')
}else{
  for(j in 1:nrow(inputs))
    inputs$count[j] <- main_function(as.integer(inputs[j,3]*5),as.integer(inputs[j,3]*5),as.integer(5),as.double(0.5),as.integer(inputs[j,1]),as.integer(inputs[j,2]),'F')
  saveRDS(inputs,'results/results50.Rds')
}


mod <- glm(count~log(motorists)+log(cyclists),family=poisson(),data=inputs,offset=0.5*log(motorists)+0.5*log(cyclists))
coef(summary(mod))
inputs$pred <- mod$fitted.values

window_positions <- sequence*windowscale
window_labels <- paste0(window_positions,'^2')
q1 <- sapply(unique(inputs$windowsize),function(x)quantile(subset(inputs,windowsize==x)$count,0.05))
q9 <- sapply(unique(inputs$windowsize),function(x)quantile(subset(inputs,windowsize==x)$count,0.95))
{x11(width=10); par(mar=c(5,5,1,1))
  plot(window_positions^2,sapply(unique(inputs$windowsize),function(x)mean(subset(inputs,windowsize==x)$count)),col='navyblue',
       frame=F,cex=1.5,pch=15,xlab='Frame size',ylab='Number of collisions',ylim=range(c(0,q1,q9)),cex.lab=1.5,cex.axis=1.5,xaxt='n')
  for(i in 1:length(q1)) lines(c(unique(inputs$windowsize)[i],unique(inputs$windowsize)[i])^2,c(q1[i],q9[i]),col='navyblue')
  axis(1,at=window_positions^2,labels=TeX(window_labels),cex.axis=1.5)
  #abline(h=pred100,col='grey',lwd=2.5)
  #legend(pch=c(15,NA),lwd=c(1,2.5),bty='n',legend=c('Simulation (90% CI)','Prediction'),x=10000,y=200,col=c('navyblue','grey'),cex=1.25)
}



###################################
preddouble <- predict(mod,newdata=data.frame(cyclists=2*seqsq,motorists=2*seqsq),type='response')

new_inputs <- data.frame(motorists=(rpois(repetitions*length(seqsq),2*seqsq)),
                         cyclists=(rpois(repetitions*length(seqsq),2*seqsq)),
                         windowsize=(rep(sequence*windowscale,repetitions)),
                         showgame=rep('F',seqlength*repetitions))
new_inputs$count <- 0
if(file.exists('results/results50double.Rds')){
  inputs <- readRDS('results/results50double.Rds')
}else{
  inputs <- new_inputs
  for(j in 1:nrow(inputs))
    inputs$count[j] <- main_function(as.integer(inputs[j,3]*5),as.integer(inputs[j,3]*5),as.integer(5),as.double(0.5),as.integer(inputs[j,1]),as.integer(inputs[j,2]),'F')
  saveRDS(inputs,'results/results50double.Rds')
}

q1 <- sapply(unique(inputs$windowsize),function(x)quantile(subset(inputs,windowsize==x)$count,0.05))
q9 <- sapply(unique(inputs$windowsize),function(x)quantile(subset(inputs,windowsize==x)$count,0.95))
{x11(width=10); par(mar=c(5,5,1,1))
  plot(window_positions^2,sapply(unique(inputs$windowsize),function(x)mean(subset(inputs,windowsize==x)$count)),col='navyblue',
       frame=F,cex=1.5,pch=15,xlab='Frame size',ylab='Number of collisions',ylim=range(c(0,q1,q9)),cex.lab=1.5,cex.axis=1.5,xaxt='n')
  for(i in 1:length(q1)) lines(c(unique(inputs$windowsize)[i],unique(inputs$windowsize)[i])^2,c(q1[i],q9[i]),col='navyblue')
  axis(1,at=window_positions^2,labels=TeX(window_labels),cex.axis=1.5)
  lines(window_positions^2,preddouble,col='hotpink',lwd=2.5)
  legend(pch=c(15,NA),lwd=c(1,2.5),bty='n',legend=c('Simulation (90% CI)','Prediction'),x=250^2,y=350,col=c('navyblue','hotpink'),cex=1.25)
}

inputsdouble <- inputs
mod <- glm(count~log(motorists)+log(cyclists),family=poisson(),data=inputsdouble,offset=0.5*log(motorists)+0.5*log(cyclists))
coef(summary(mod))
inputs$pred <- mod$fitted.values

#################################################################
mod <- glm(count~log(motorists)+log(cyclists),family=poisson(),data=rbind(inputs,inputsdouble),offset=0.5*log(motorists)+0.5*log(cyclists))
coef(summary(mod))
mod <- glm(count~log(motorists)+log(cyclists)+windowsize,family=poisson(),data=rbind(inputs,inputsdouble),offset=0.5*log(motorists)+0.5*log(cyclists))
coef(summary(mod))



