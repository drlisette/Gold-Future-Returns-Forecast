#' getForecastPCA.r
#' developped on www.alphien.com
#' predict gold future return with PCA-based multiple linear regression
#' @param pxs   data 
#' @param maxLagOrder  the maximal lag order of gold return used for PCA
#' @param windowSize  rolling window size
#' @param forecastHorizon  forecast step for one rolling forecast
#' @param level  determines the precision of PCA analysis, i.e., keep how many PCs, i.e,
#'               sum(variances of components taken)> level * sum(all variances)
#'
#' @return a list of 4 elements, an xts object of predicted and true prices, mse and mae
#' @export
#'
#' @examples
#' ######## relevant factor names ########
#'secuNames = c("USGGBE10 Index", "USGG5YR Index")
# BBNames = c("KC","RY", "RX", "HI", "XID","KU", "PA", "PL", "IH", "SI", "SMix", "TU", "W ","JY")
#  ######## get data ########
# gold = ROC(getBB("GC", start = "2018-01-01", end = "2019-07-31"), n = 1, type ="continuous", na.pad = FALSE)
# pxs = cbind(gold,
#            ROC(getSecurity(secuNames, start = "2018-01-01",end = "2019-07-31"),
#               type="continuous", na.pad=FALSE),
#            ROC(getBB(BBNames, start = "2018-01-01",end = "2019-07-31"),
#                type="continuous", na.pad=FALSE))
# ######## change column names #########
# colNames = c("gold-t",secuNames,BBNames)
# colnames(pxs) = colNames               
#' ######## call function ########               
#' P = getForecastPCA(pxs, maxLagOrder = 2, windowSize = 20, forecastHorizon = 1, level = 0.8)  
#' 
#' @seealso
#'* [https://www.alphien.com/mynotebooks/PRIMLOGIT/Library/Dongrui/PCA-based_Multiple_Regression_Model_Performance.ipynb Notebook that illustrates the use of this function, references are also provided]         

getForecastPCA = function(pxs, maxLagOrder = 2, windowSize = 20, forecastHorizon = 1, level = 0.8, showGraph = FALSE){  
  
  # check data adequacy
  if(dim(pxs)[1] < dim(pxs)[2]){
    stop("Need to have more data than explanatory variables")
  }    
  
  if(dim(pxs)[1]<(windowSize+forecastHorizon)){
    stop("Not enough data")
  }
  
  # lag gold series 
  gold = pxs[,1]
  goldLag = lag(gold, k = 1:maxLagOrder)
  lagColNames = c()
  for (i in 1:maxLagOrder){
    lagColNames = rbind(paste("gold","t", toString(i), sep = "-"),lagColNames)
  }
  
  # get features and target values
  # get rid of rows that contain NAs 
  X = na.omit(cbind(goldLag, pxs[,-1]))
  Y = pxs[,1][-(dim(pxs[,1])[1] - dim(X)[1]):-1]
  colnames(X) = c(lagColNames, colnames(pxs[,-1]))
  
  ## rolling PCA
  # get the explanatory variables after PCA transformation
  Z = rollapply(as.matrix(X),
                width = windowSize+forecastHorizon,
                by.column=FALSE,
                function(pxs){ 
                  pxs = as.matrix(pxs)
                  PCA = prcomp(head(pxs, n=windowSize), rank. = level*(dim(pxs)[2]))
                  coefs = PCA$rotation
                  return(as.numeric(pxs %*% as.matrix(coefs)))
                },
                by = forecastHorizon)
  
  # get true values
  yTrue = tail(Y, n = -windowSize)
  
  ## prepare rolling MLR data 
  # create batches of dependent variables 
  Y = rollapply(as.matrix(Y),
                width = windowSize+forecastHorizon,
                by.column = FALSE,
                function(pxs) return(as.numeric(pxs)),
                by = forecastHorizon)
  # combine explanatory variables and dependent variables
  M = cbind(Y, Z)
  
  ## rolling MLR
  perfs = rollapply(as.matrix(M), 
                    width = 1,
                    by.column = FALSE,
                    function(pxs){
                      # fill by column    
                      pxs = as.data.frame(matrix(pxs, nrow = (windowSize+forecastHorizon))) 
                      colnames(pxs)[1] = "y"
                      MLR = lm(formula = y ~ ., data = head(pxs, n=windowSize))
                      yTrue = tail(pxs[,1],n = forecastHorizon)
                      yPred = predict(MLR, tail(pxs[,-1], n = forecastHorizon))
                      return(list(as.numeric(yPred),
                                  as.numeric(yTrue),
                                  as.numeric(cumsum((yPred - yTrue)^2)/c(1:forecastHorizon)),
                                  as.numeric(cumsum(abs(yPred - yTrue))/c(1:forecastHorizon)))
                      )
                    })
  
  # organize the results
  res = xts(cbind("yPred" = unlist(perfs[,1]),
                  "yTrue" = unlist(perfs[,2]),
                  "MSE" = unlist(perfs[,3]),
                  "MAE" = unlist(perfs[,4])),
            order.by = head(tail(index(X), n=-windowSize), n=forecastHorizon*dim(perfs)[1]))
  
  # visualisation
  if (showGraph){
    par(mfrow = c(1, 1)) 
    plotAl(cbind(res[,"yPred"], res[,"yTrue"]),
           color = c("#8DD3C7", "#BEBADA"),
           title = "PCA LM model",
           legendPlace = "bottom")
  }
  
  return(res)
}