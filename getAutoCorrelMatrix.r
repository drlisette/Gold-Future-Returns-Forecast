#' getAutoCorrelMatrix.R
#' developped on www.alphien.com
#' plot auto-correlation matrix for r_t and return auto-correlation coefficients
#' r_t = 100 * ln(P_t/P_(t-n)), achieved by ROC function
#' @return 
#' @export
#'
#' @examples
#' data = ROC(getBB("GC"), type="continuous", na.pad=FALSE)["2018/"]
#' getAutoCorrelMatrix(data, tLag = 4)


getAutoCorrelMatrix = function(data, tLag){
  pxs = lags(data, n = tLag)  #r_(t-j)s, j = 0,1,2,..,t list of lagged returns
  M = rcorr(pxs)
  
  # rename rownames and colnames to x, x-1,...
  for (i in c(1:tLag)){
    colnames(M$r)[i+1] = paste("x",i,sep = '-')
    rownames(M$r)[i+1] = paste("x",i,sep = '-')
  }
  colnames(M$r)[1] = "x"
  rownames(M$r)[1] = "x"
  
  # plot auto-correlation matrix
  col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#35d4bc", '#208172'))
  corrplot::corrplot(M$r, method="color", col=col(200),
                     type="upper", addCoef.col = "white", tl.col="#666666",
                     tl.cex=0.75, number.cex=0.75,
                     p.mat = M$p, sig.level = 0.001, insig = "blank",
                     diag=T)
  return(invisible(M))
  
}