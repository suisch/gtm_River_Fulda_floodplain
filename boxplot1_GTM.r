#leaning heavily on https://github.com/SAFEtoolbox/SAFE-R/blob/main/R/boxplot1.R


#' "Boxplots", or rather points with error margins, 
#'  when the mean, lower values and upper values are specified.

#' @param mu vector (\code{M}) of mean or median values to be plotted
#' @param lb vector (\code{M}) of lower values to be plotted. 
#' @param au vector (\code{M}) of upper values to be plotted. 
#' @param prnam labels for the x-axis of the boxplot
#' @seealso \code{\link{boxplot2}} \code{\link{boxplot}} \code{\link{plot}}
#' @export
#' @examples


boxplot1_gtm <- function(mu, lb = NULL, ub = NULL, prnam = NULL){
  
  dat <- data.frame(x = factor( prnam, levels = prnam ),
                    mu = mu)
  
  .pl <- ggplot(data = dat, mapping = aes(x = x, y = mu))
  
  if( !is.null(lb) && !is.null(ub) ){
    dat$lb = lb 
    dat$ub = ub
    .pl <- .pl + geom_errorbar(mapping = aes(ymin = lb, ymax = ub), width = 0.5) 
  }
  
  .pl <- .pl + geom_point(color = 'red', size = 3) + 
  theme_bw() + 
              theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +#sis
  xlab(NULL) + ylab("Sensitivity") +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1))
  
  return( .pl )
  
}