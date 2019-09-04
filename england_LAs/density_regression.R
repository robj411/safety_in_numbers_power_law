library(magrittr)
library(ISwR)
library(rstan)
library(parallel)
library(tableone)
setwd('~/overflow_dropbox/safety_in_numbers_power_law/england_LAs/')

summary_counts <- readRDS('data/england_models_all_data.Rds')
rural_percent_threshold <- 0.02
urban <- subset(summary_counts,ruralpercent<rural_percent_threshold)
rural <- subset(summary_counts,ruralpercent>=rural_percent_threshold)
datasets <- list(all=summary_counts,urban=urban,rural=rural)

summary(summary_counts)


## Load model
fileName <- "./density_regression.stan"
stan_code <- readChar(fileName, file.info(fileName)$size)
options(mc.cores = parallel::detectCores())

for(set in c(1,3))
  for(lab in c('','_ksi','_fatal')){
    ## Run glm
    glm1 <- glm(formula = whw_ksi_bike ~ log(Pedal.Cycles)+log(Car) + offset(-log(AB)),
                family  = poisson(link = "log"),
                data    = datasets[[set]])
    summary(glm1)
    
    ## Get model matrix
    modMat <- as.data.frame(model.matrix(glm1))
    modMat$offset <- -log(datasets[[set]]$AB)
    names(modMat) <- c("intercept", "cycle", "motor", "offset")
    
    dat   <- as.list(modMat)
    dat$N <- nrow(modMat)
    dat$p <- ncol(modMat) - 1
    
    dat$y <- datasets[[set]][[paste0('whw',lab,'_bike')]]
    
    ## Run stan
    resStan <- stan(model_code = stan_code, data = dat,
                    chains = 3, iter = 3000, warmup = 500, thin = 10)
    
    ## Show traceplot
    traceplot(resStan, pars = c("beta"), inc_warmup = TRUE)
    
    ## Frequentist
    tableone::ShowRegTable(glm1, exp = FALSE)
    
    print(c(names(datasets)[set],lab))
    ## Bayesian
    print(resStan, pars = c("beta","beta_sum"))
  }


long_all <- summary_counts[,c(1,2,3,4,13,20,22,36,40)]
long_ksi <- summary_counts[,c(1,2,3,4,15,20,22,36,40)]
long_fatal <- summary_counts[,c(1,2,3,4,17,20,22,36,40)]
long_all$severity <- 'slight'
long_ksi$severity <- 'serious'
long_fatal$severity <- 'fatal'
names(long_all)[5] <- names(long_ksi)[5] <- names(long_fatal)[5] <- 'count'
long_all$count <- long_all$count - long_ksi$count
long_ksi$count <- long_ksi$count - long_fatal$count
all_data <- rbind(long_all,long_ksi,long_fatal)
all_data$urban <- all_data$ruralpercent < 0.01
## Run glm
glm1 <- glm(formula = count ~ log(Pedal.Cycles)+log(Car) + offset(-log(AB)) + severity + urban,
            family  = poisson(link = "log"),
            data    = all_data)
summary(glm1)

## Get model matrix
modMat <- as.data.frame(model.matrix(glm1))
modMat$offset <- -log(all_data$AB)
names(modMat) <- c("intercept", "cycle", "motor", "serious", "slight", "urban", "offset")

dat   <- as.list(modMat)
dat$N <- nrow(modMat)
dat$p <- ncol(modMat) - 1

dat$y <- all_data$count

## Load model
fileName <- "./density_regression_long.stan"
stan_code <- readChar(fileName, file.info(fileName)$size)

## Run stan
resStan <- stan(model_code = stan_code, data = dat,
                chains = 3, iter = 3000, warmup = 500, thin = 10)

## Show traceplot
traceplot(resStan, pars = c("beta"), inc_warmup = TRUE)

## Frequentist
tableone::ShowRegTable(glm1, exp = FALSE)

## Bayesian
print(resStan, pars = c("beta","beta_sum"))
