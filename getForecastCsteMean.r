#' getForecastCsteMean.R
#' developped on www.alphien.com
#' Fitting a constant mean model to gold future returns 
#' @param pxs   gold return data 
#' @param trainDataLen  length of train data for one model fit
#' @param forecastStep  do forecastStep-step ahead prediction for one model
#' @param rollStep  interval between two model fits, =1 if no date is skipped
#' @param showGraph whether to show (yPred, yTrue)
#' @return an xts object of 4 columns, yPred, yTrue, mse and mae
#' @export
#'
#' @examples
#' one-step ahead forecast
#' pxs = ROC(getBB("GC", start = "2017-10-31", end = "2019-10-31"), type ="continuous", na.pad = FALSE)
#' res = getForecastCsteMean(pxs, showGraph = TRUE)   
#' 
#' two-steps ahead forecast
#' pxs = ROC(getBB("GC", start = "2017-10-31", end = "2019-10-31"), type ="continuous", na.pad = FALSE)
#' res = getForecastCsteMean(pxs, trainDataLen = 20, forecastStep = 2, rollStep = 2, showGraph = TRUE)  
#' 
#' 
#' @seealso
#'* [https://www.alphien.com/mynotebooks/PRIMLOGIT/Library/Dongrui/Constant_Mean_Model_Performance.ipynb Notebook that illustrates the use of this function]  
#'* [https://www.alphien.com/mynotebooks/PRIMLOGIT/Library/Dongrui/Constant_Mean_Model_Validation.ipynb Notebook that shows some validation tests related to this function]  

getForecastCsteMean = function(pxs, trainDataLen = 20, forecastStep = 1, rollStep = 1, showGraph = FALSE){
  
  if(length(pxs)<(trainDataLen+forecastStep)){
    stop("Not enough data")
  }
  
  if(rollStep < forecastStep){
    stop("Too small rollStep or too big forecastStep")
  }
  
  train=data.frame(cbind(pxs, rep(1, nrow(pxs))))
  train=cbind(as.character(index(pxs)),train)  
  names(train)=c("date","y","X")
  
  perfs=rollapply(data.frame(train), by=rollStep, width=(trainDataLen+forecastStep), by.column=FALSE,
                  FUN=function(df){
                    CsteMeanModel=lm(formula=y ~ 1, data=head(data.frame(df), n=-forecastStep),  na.action = na.omit)
                    yPred=predict(CsteMeanModel, newdata=data.frame("X"=rep(1, forecastStep)))
                    yTrue= as.numeric(tail(df[,"y"], n = forecastStep))
                    return(list(df[(nrow(df)-forecastStep+1):nrow(df),"date"],
                                as.numeric(yPred),
                                as.numeric(df[(nrow(df)-forecastStep+1):nrow(df),"y"]),
                                as.numeric(cumsum((yPred - yTrue)^2)/c(1:forecastStep)),
                                as.numeric(cumsum(abs(yPred - yTrue))/c(1:forecastStep))))
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
           title = "constant mean model",
           legendPlace = "bottom")
  }
  
  return(res)
}