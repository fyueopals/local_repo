### this is the initial attempt on the second piece of function 1
### This function allows user to use arima to fit the time series model

### Input:
# -- y.con: the response of con group 
# -- pre.period: the time zone for pre.period (I would like it to be consistent with CausalImpact)
# -- post.period: the time zone for post.period (I would like it to be consistent with CausalImpact)
# -- feature: a matrix X, with number of rows same as y.con and some number of columns

### Output:
# -- y0: the response of the pseudo control group

### Dependencies:
# -- keras: the R package that is necessary for fitting lstm
# -- tensorflow: the R package that is necessary for fitting lstm
# -- I do not want the function contains something like install_keras and install_tensorflow
# -- I would prefer the function itself generates error automatically and let users to fit before calling the function

### Input quality control:
# -- 0. check environment available to call(i dont know how to do that for now) 
# -- 1. check non-empty input, using y.con only
# -- 2. check length(y.con) == length(pre.period) + length(post.period) and length(y.con == feature)


library(keras)
library(tensorflow)



install.packages('tensorflow')
lstm = function(y.con, pre.period, post.period, feature){
  
install.packages('ggplot2')

sessionInfo()
  
  ## input quality control
  
  if(length(y.con) == 0){
    stop("empty vector of response in control group")
  }
  
  if(!(length(y.con) == (length(pre.period) + length(post.period)))){
    stop("length of input vector is different")
  }
  
  if(!(length(y.con) == dim(feature)[1])){
    stop("length of input vector is different")
  }
  
  ## make data a copy
  y.con0 = y.con
  feature0 = feature
  
  ## for time series forcasting, I will only need the data from pre period
  y.con = y.con0[pre.period]
  feature = as.matrix(feature0[pre.period,])
  
  ## see reference http://datasideoflife.com/?p=1171
  
  #################################################################
  ####################### data processing #########################
  #################################################################
  
  ## step 1: calculate the scale factors, for the response y.con and also feature
  ## also, normalize the response and feature
  
  scale.factor.response = c(mean(y.con), sd(y.con))
  y.con.norm = (y.con - scale.factor.response[1])/scale.factor.response[2]
  
  scale.factor.feature = matrix(NA, 2, dim(feature)[2])
  feature.norm = feature
  
  for (i in 1:dim(feature)[2]) {
    
    xx.i = feature[,i]
    mu.i = mean(xx.i)
    sd.i = sd(xx.i)
    scale.factor.feature[,i] = c(mu.i, sd.i)
    
    xx.i = (xx.i- mu.i)/sd.i
    feature.norm[,i] = xx.i
  }
  
  ## step 2: based on the constant, lag, and prediction
  ## to construct the 3D array of LSTM input
  
  ## I will temporally use 3 to predict 2 (those two tuning parameters could be adjusted)
  
  lag = 3
  prediction = 2
  
  ## for input of training data, construct x_train_arr
  
  x_train_data = list() ## this is a list of length: dim(feature)[2] + 1(for response)
  
  x_train_data[[1]] = t(sapply(
    1:(length(y.con.norm) - lag - prediction + 1), 
    function(x){
      return(y.con.norm[x:(x + lag - 1)])
    }))
  
  for (i in 1:dim(feature.norm)[2]) {
    
    xx = feature.norm[,i]
    
    x_train_data[[i+1]] = t(sapply(
      1:(length(xx) - lag - prediction + 1), 
      function(x){
        return(xx[x:(x + lag - 1)])
      }))
  }
  
  x_train_arr = array(
    data = as.numeric(unlist(x_train_data)),
    dim = c(nrow(x_train_data[[1]]),
            lag,
            (dim(feature)[2] + 1))
  )
  
  ## for output of training data, construct y_train_arr
  
  y_train_data = t(sapply(
    (1 + lag):(length(y.con.norm) - prediction + 1),
    function(x){
      return(y.con.norm[x:(x + prediction - 1)])
    }))
  
  y_train_arr = array(
    data = as.numeric(unlist(y_train_data)),
    dim = c(
      nrow(y_train_data),
      prediction,
      1
    )
  )
  
  #################################################################
  ####################### fit lstm model ##########################
  #################################################################
  
  #################################################################
  ##################### predict using lstm ########################
  #################################################################
  
}