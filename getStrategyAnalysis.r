#' getStrategyAnalysis.r
#' developped on www.alphien.com
#' @param pxs  target series 
#' @param window  an integer, rolling forecast window, use how many history data to build a rolling forecast model
#' @param rollStep  an integer, rolling forecast step
#' @param forecastHorizon  an integer, forecast how many steps ahead for 1 rolling forecast
#' @param model  model name
#'
#' @return a 0(sell)-1(buy) strategy based on the given model
#' @export
#'
#' @examples
#' ######### change directory and prepare source functions ########
#' setwd("~/PRIMLOGIT/Library/Dongrui")
#' source("PRIMLOGIT_getForecastAR.r")
#' source("PRIMLOGIT_getForecastPCA.r")
#' source("PRIMLOGIT_getForecastCsteMean.r")
#' ######### build strategy based on a model ###########
#' algoEngine("GC",type = "MM") %>%
#' payout(SomePayout,model="PCA-LR",window=20) -> strategy
#' ######### strategy evaluation and visualization ###########
#' strategy = evaluate(strategy)
#' backtest(strategy)
#' 
#' @seealso
#' *[https://www.alphien.com/mynotebooks/PRIMLOGIT/Library/Dongrui/Model_Strategy_Comparison.ipynb Notebook that illustrates the use of this function, references are also provided]


SomePayout<-function(pxs=na.omit(ROC(getBB("GC",start = "2018-12-01",end = Sys.Date()),
                                     type="discrete", na.pad=FALSE)),window=20,rollStep=1,forecastHorizon=1,model="naive"){
  
  if(identical(model,"AR1")){
    
    ar1=getForecastAR(pxs, lags = 1,  trainDataLen = window, forecastStep = forecastHorizon, rollStep = rollStep, showGraph = FALSE)
    forecast = ar1$yPred
  }
  else if (identical(model,"AR2")){
    
    ar2=getForecastAR(pxs, lags = 2,  trainDataLen = window, forecastStep = forecastHorizon, rollStep = rollStep, showGraph = FALSE)
    forecast = ar2$yPred
  }
  else if(identical(model,"PCA-LR")){
    secuNames = c("USGGBE10 Index", "USGG5YR Index")
    BBNames = c("KC","RY", "RX", "HI", "XID","KU", "PA", "PL", "IH", "SI", "SMix", "TU", "W ","JY")
    start = "2018-12-01"
    end = Sys.Date()
    pxs = cbind(pxs,
                ROC(getSecurity(secuNames, start = start, end = end),
                    type="discrete", na.pad=FALSE),
                ROC(getBB(BBNames, start = start, end = end),
                    type="discrete", na.pad=FALSE))
    pcaLM = getForecastPCA(pxs, maxLagOrder = 2, windowSize = window, forecastHorizon = forecastHorizon)
    forecast = pcaLM$yPred
  }
  else if (identical(model,"CsteMean")){
    naive = getForecastCsteMean(pxs, trainDataLen = window, forecastStep = forecastHorizon, rollStep = rollStep, showGraph = FALSE)   
    forecast = naive$yPred
  }
  
  return(setNames(na.omit(ifelse(forecast>0,1,0)),"Y"))
  
}