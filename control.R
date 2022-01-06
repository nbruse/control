# Author: NB
# Description: A function to control a data set (x) for a certain variable (y) in it
#              Returns new data frame with residuals

control<-function(x,y){
  temp<-x
  for(i in 1:ncol(x)){
    if(colnames(x)[i] != y){
      name <- colnames(x)[i]
      model_cl<-lm(as.formula(paste0(name,'~',y)), data = as.data.frame(x))
      temp<-add_residuals(as.data.frame(temp), model_cl, var = paste0('res_',name))
    }
  }
  return(temp[,(ncol(x)+1):ncol(temp)])
}