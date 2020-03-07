#' getForecastAR.r
#' developped on www.alphien.com
#' predict gold future returns with AR model
#' @param pxs  data points
#' @param lags  the AR order  
#' @param trainDataLen  length of train data for one model fit
#' @param forecastStep  do forecastStep-step ahead prediction for one model
#' @param rollStep  interval between two model fits, =1 if no date is skipped
#' @param showGraph  binary variable, whether to show (yPred, yTrue)
#'     
#' @return an xts object of 4 columns
#' yPred: predicted returns
#' yTrue: true returns
#' mse: mean square error(s)
#' mae: mean absolute error(s)
#' @export
#'
#' @examples
#' one-step ahead forecast
#' pxs = ROC(getBB("GC", start = "2019-11-11", end = "2019-11-19"), n = 1, type ="continuous", na.pad = FALSE)
#' res = getForecastAR(pxs, lags = 2,  trainDataLen = 5, forecastStep = 1, rollStep = 1, showGraph = FALSE)
#' 
#' multiple-step ahead forecast
#' pxs = ROC(getBB("GC", start = "2019-10-12", end = "2019-11-20"), n = 1, type ="continuous", na.pad = FALSE)
#' res = getForecastAR(pxs, lags = 2,  trainDataLen = 5, forecastStep = 2, rollStep = 2, showGraph = TRUE)
#' 
#' @seealso
#'* [https://www.alphien.com/mynotebooks/PRIMLOGIT/Library/Dongrui/AR_model_performance.ipynb Notebook that illustrates the use of this function, references are also provided] 
#'* [https://www.alphien.com/mynotebooks/PRIMLOGIT/Library/Dongrui/AR_Model_Validation.ipynb Notebook that proposed some validation test cases related to this function] 



getForecastAR = function(pxs, lags = 2, trainDataLen = 5, forecastStep = 1, rollStep = 1, showGraph = FALSE){
  
  if(length(pxs)<(trainDataLen+forecastStep)){
    stop("Not enough data")
  }
  
  if(rollStep < forecastStep){
    stop("Too small rollStep or too big forecastStep, cannot have 2 predictions for a same day")
  }
  
  data=cbind(as.character(index(pxs)),data.frame(pxs))  
  names(data)=c("date","y")
  
  # apply AR model and do forecastStep-ahead forecast
  perfs=rollapply(data.frame(data), by=rollStep, width=(trainDataLen+forecastStep), by.column=FALSE,
                  FUN=function(df){
                    ARmodel=ar(head(as.numeric(df[,2]), n=trainDataLen), aic = FALSE, order.max = lags, method = "yw",  na.action = na.omit, demean = FALSE)
                    Xpred = tail(head(as.numeric(df[,2]), n=trainDataLen), n=lags)
                    yPred = predict(ARmodel, Xpred, n.ahead = forecastStep, se.fit = FALSE)
                    return(list(df[(nrow(df)-forecastStep+1):nrow(df),"date"],
                                as.numeric(yPred),
                                as.numeric(df[(nrow(df)-forecastStep+1):nrow(df),"y"]),
                                as.numeric(cumsum((yPred - as.numeric(tail(df[,"y"], n = forecastStep)))^2)/c(1:forecastStep)),
                                as.numeric(cumsum(abs(yPred - as.numeric(tail(df[,"y"], n = forecastStep))))/c(1:forecastStep))))
                  })
  
  res = xts(cbind("yPred"=unlist(perfs[,2]),
                  "yTrue"=unlist(perfs[,3]), 
                  "MSE"=unlist(perfs[,4]),
                  "MAE"=unlist(perfs[,5])),
            order.by = as.POSIXct(unlist(perfs[,1])))
  
  # visualisation
  if(showGraph){
    par(mfrow = c(1, 1)) 
    plotAl(cbind(res[,"yPred"], res[,"yTrue"]),
           color = c("#8DD3C7", "#BEBADA"),
           title = "AR(2) model",
           legendPlace = "bottom")
  }
  
  return(res)
}
