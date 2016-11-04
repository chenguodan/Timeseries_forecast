# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}

out25 <- function(x){
  n=length(x)-1
  re <- c()
  for (i in 2:n){
    if(x[i]==0){
      re<-c(re,i)}else{
    b=abs(x[i]-x[i-1])
    d=b/x[i]
    c=abs(x[i+1]-x[i])
    e=c/x[i]
    if((d>0.25)&&(e>0.25)){
      re<-c(re,i)}}}
    if(x[n+1]==0){re<-c(re,n+1)}else{a=abs(x[i+1]-x[i])
    b=a/x[i]
    if(b>0.25){re<-c(re,i)}}
  print (re)
  return(re)}

out_one_side3 <- function(x){
  n=length(x)-1
  re <- c()
  for (i in 2:n){
    if(x[i]==0){
      re<-c(re,i)}else{
    b=abs(x[i]-x[i-1])
    d=b/x[i]
    if(d>0.3){
      re<-c(re,i)}
      }}
  if(x[n+1]==0){re<-c(re,n+1)}else{a=abs(x[i+1]-x[i])
  b=a/x[i]
  if(b>0.3){re<-c(re,i)}}
  print (re)
  return(re)}

out3 <- function(x){
  n=length(x)-1
  re <- c()
  for (i in 2:n){
    if(x[i]==0){
      re<-c(re,i)}else{
    b=abs(x[i]-x[i-1])
    d=b/x[i]
    c=abs(x[i+1]-x[i])
    e=c/x[i]
    if((d>0.3)&&(e>0.3)){
      re<-c(re,i)}}
  }
  if(x[n+1]==0){re<-c(re,n+1)}else{a=abs(x[i+1]-x[i])
  b=a/x[i]
  if(b>0.3){re<-c(re,i)}}
  print (re)
  return(re)}

ave_2<-function(ts,outlier){
  n=length(outlier)
  for (i in 1:n){
    ind<-outlier[i]
    f=ind-1
    l=ind+1
    ts[ind] = (ts[f]+ts[l])/2
  }
  return(ts)}

data_split<-function(ts,outlier){
  if (length(outlier)==0){
    l = length(ts)
    a=l-4
    b=l-3
    test<ts[b:l]
    train<-ts[1:a]

  }else{
    while (length(outlier)!=0){
      ts = ave_2(ts,outlier)
      outlier = out25(ts)}
    l = length(ts)
    a=l-4
    b=l-3
    test<-ts[b:l]
    train<-ts[1:a]}
  data <- list(train=train,test=test)
  return(data)}


