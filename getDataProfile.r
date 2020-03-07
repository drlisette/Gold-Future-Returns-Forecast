#' getDataProfile.r
#' developped on www.alphien.com
#' analyse a return series (and PCA applicability with other relevent return series)
#' @param pxs  target return series (and relevent series)
#'
#' @return a list of 10 elements if pxs has multiple columns, else 8 elements
#' @export
#'
#' @examples
#' setwd("~/PRIMLOGIT/Library/Dongrui")
#' source("PRIMLOGIT_getAutoCorrelMatrix.r")
#' 
#' pxs = ROC(getBB("GC",start = "2013-10-31",end = "2019-07-31" ), 
#'           type="continuous", na.pad=FALSE)
#' secuNames = c("SPX Index", "VIX Index", "USGGBE10 Index", "USGG5YR Index")
#' BBNames = c("KC","RY", "RX", "HI", "XID","KU", "PA", "PL", "IH", "SI", "SMix", "TU", "W ","JY")
#' M = ROC(getSecurity(secuNames, start = "2013-10-31", end = "2019-07-31"),
#'         type="continuous", na.pad=FALSE)
#' M = cbind(M,ROC(getBB(BBNames, start = "2013-10-31", end = "2019-07-31"),
#'                 type="continuous", na.pad=FALSE))
#' pxs = cbind(pxs, M)
#' pxsProfile = getDataProfile(pxs)
#'
#' @seealso
#'* [https://www.alphien.com/mynotebooks/PRIMLOGIT/Library/Dongrui/Gold_Data_Profiling.ipynb Notebook that illustrates the use of this function and interprets the metrics] 


getDataProfile = function(pxs){
  
  # mean
  mean = mean(pxs[,1])
  
  # sd
  sd = sd(pxs[,1])
  
  # skew, measure symmetry 
  skew = skewness(pxs[,1], method = "moment")
  
  # kurtosis
  ktosis = kurtosis(pxs[,1], method = "moment")
  
  # Jarque-Bera Normality Test
  # test null hypothesis if pxs follows normal distribution
  jbTest = jarque.bera.test(pxs[,1])
  
  # Augmented Dickey-Fuller Test
  # test H0 if pxs has a root unit i.e., r_t = r_t-1 + ...
  adfTest = adf.test(pxs[,1], alternative = "stationary",
                     k = trunc((ndays(pxs)-1)^(1/3)))
  
  # check lag-length choice in ADF test
  lags = c(0:trunc((ndays(pxs[,1])-1)^(1/3)*2))
  DFval = c()
  pVal = c()
  for (i in lags){
    res = adf.test(pxs[,1], alternative = "stationary",
                   k = i)
    DFval = c(DFval, res$statistic)
    pVal = c(pVal, res$p.value)
  }
  adfLagChoice = data.frame("Lag length"=lags,"Test value" = DFval, "P value" = pVal)
  
  # autocorrelation matrix
  # meaningful only if pxs doesn't have a root unit
  if ((adfTest$statistic < -5) & (adfTest$p.value < 0.05)){
    covMtrx = getAutoCorrelMatrix(pxs[,1], tLag = 4)
  }
  
  ### do barlett's test and Kaiser sampling test if pxs has multiple columns
  if (ncol(pxs) > 1){
    # Bartlett's sphericity test
    # test H0 if the correlation matrix of pxs and other series is an identity matrix
    # check the applicability of PCA
    bartSphereTest = bart_spher(pxs, use = "pairwise.complete.obs") 
    
    # Kaiser-Meyer-Olkin
    # measure sampling adequacy
    kmo = KMOS(pxs, use = c("pairwise.complete.obs"))
    
    return(list("mean" = mean,
                "sd" = sd,
                "skewness" = skew,
                "kurtosis" = ktosis,
                "JBtest" = jbTest,
                "ADFtest" = adfTest,
                "ADFtestLagChoice" = adfLagChoice,
                "correlation" = covMtrx,
                "BartlettSpheretest" = bartSphereTest,
                "KMO" = kmo))
    
  }
  else{
    
    return(list("mean" = mean,
                "sd" = sd,
                "skewness" = skew,
                "kurtosis" = ktosis,
                "JBtest" = jbTest,
                "ADFtest" = adfTest,
                "ADFtestLagChoice" = adfLagChoice,
                "correlation" = covMtrx))
  }
  
}