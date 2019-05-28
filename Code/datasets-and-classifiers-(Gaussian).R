set.seed(2019)

dataset_names <- apply(expand.grid(2^c(1:5),seq(2,0,-1),seq(400,1600,200),2000),1,function(spec){
  paste("Variables",spec[1],"NormalMean",spec[2],"ProportionPos",spec[3]/spec[4], sep="_")
})

datasets <- lapply(as.list(data.frame(t(expand.grid(2^c(1:5),seq(2,0,-1),seq(400,1600,200),2000)))), function(spec){
  xpos <- matrix(rnorm(spec[1]*spec[3]), ncol = spec[1])
  ypos <- rep(1, spec[3])
  xneg <- matrix(rnorm(spec[1]*(spec[4]-spec[3]),spec[2]), ncol = spec[1])
  yneg <- rep(0, spec[4]-spec[3])
  mydf <- data.frame(rbind(cbind(xpos,ypos),cbind(xneg,yneg)))
  colnames(mydf)[colnames(mydf)=="ypos"] <- "y"
  mydf[, 'y'] <- as.factor(mydf[, 'y'])
  return(mydf)
})

names(datasets) <- dataset_names


models <- sapply(dataset_names, function(variables) {
  return(formula(y~.))
})
rm(dataset_names)


classifiers<-list(
  # "Supervised"=function(X,y,X_u,y_u) {LeastSquaresClassifier(X,y,intercept=TRUE,x_center=TRUE,scale=FALSE) },
  # "Self-Learning"=function(X,y,X_u,y_u) {SelfLearning(X, y, X_u, method=LeastSquaresClassifier,intercept=TRUE,x_center=TRUE,scale=FALSE)},
  # "USM"=function(X,y,X_u,y_u) {USMLeastSquaresClassifier(X,y,X_u,intercept=FALSE,x_center=TRUE,y_scale=TRUE) }, 
  "ICLS"=function(X,y,X_u,y_u) {ICLeastSquaresClassifier(X,y,X_u,intercept=TRUE,x_center=TRUE,scale=FALSE) }
  # "ICLS_prior"=function(X,y,X_u,y_u) { ICLeastSquaresClassifier(X,y,X_u,x_center=FALSE,scale=FALSE, lambda_prior=1000000, trueprob=mean(model.matrix(~y-1,data.frame(y=y_u))[,1])) },
  # "Oracle"=function(X,y,X_u,y_u) {LeastSquaresClassifier(rbind(X,X_u),unlist(list(y,y_u)),intercept=TRUE,x_center=TRUE,scale=FALSE) }
)
