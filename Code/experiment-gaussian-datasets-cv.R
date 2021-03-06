library(methods)
library(RSSL)
library(randomForest)
library(parallel)

source("code/datasets-and-classifiers-(Gaussian).R")

set.seed(2019)

start_time <- Sys.time()

modelforms <- models
repeats <- 1
n_labeled <- "enough"
pca <- FALSE
description <-"final"
verbose <- TRUE

PU <- TRUE
positive_case <- "1"

## Calculate Learning Curves
cvresults <- mclapply(names(datasets),function(dname){
  cat(dname,"\n");
  data <- data.frame(datasets[[dname]]) 
  classname<-all.vars(modelforms[[dname]])[1]
  
  X <- model.matrix(modelforms[[dname]],datasets[[dname]])
  X <- X[,colnames(X)!="(Intercept)"]
  X <- X[,apply(X, 2, var, na.rm=TRUE) != 0] # Remove constant columns
  X <- scale(X) # Pre-scale data
  
  if (pca) {
    t_pca <- princomp(X)
    n_comp <- sum(cumsum(t_pca$sdev^2)/sum(t_pca$sdev^2)<0.99)
    n_comp <- n_comp #min(c(n_comp,floor(n_labeled/2)))
    X <- t_pca$scores[,1:n_comp]
  }
  y <- data[,classname]
  
  if (n_labeled=="enough") { n_l <- max(ncol(X)+5,20) }
  else if (n_labeled=="d") { n_l <- ncol(X)+1 }
  else if (n_labeled=="2d") { n_l <- ncol(X)*2 }
  else {n_l<-n_labeled}
  
  
  result <- CrossValidationSSL(X,y,classifiers,
                               measures = list("Accuracy" = measure_accuracy,
                                               "Sensitivity" = measure_sensitivity,
                                               "Specifity" = measure_specifity
                               ),
                               time=FALSE,
                               repeats=repeats,
                               n_labeled=n_l,
                               verbose=TRUE,
                               k=10,
                               PU=PU,
                               positive_case=positive_case
                               
  )
  save(result,file=paste0("data/cv-backup-",dname,".RData"))
  result
},mc.cores = 1)

names(cvresults) <- names(datasets)

total_time <- Sys.time() - start_time 
print(total_time)

dir.create("data/",showWarnings = FALSE)
save(cvresults,classifiers,repeats,n_labeled,total_time,file=paste0("data/crossvalidation-",repeats,"repeats-",n_labeled,"labeled-",description,".RData"))