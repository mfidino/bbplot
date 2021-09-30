#' @title Add labels to the axis
#'
#' @description A wrapper function for \code{\link[base]{mtext}} to plot the labels to the axes. 
#' The changes made to this function are simple quality of life improvements. For
#' example, if \code{at = NA}, then the function uses \code{\link[graphics]{axTicks}}
#' to the mean of the axis an place the text there. Likewise, if \code{line = NA}, then the margin size of 
#' the associated side of the plot will be queried and the axis labels will be added 60% 
#' down the margin
#'
#' @param text a character or expression vector specifying the text to be written.
#' If NA, then \code{\link[graphics]{axTicks}} is used to find values.
#' 
#' @param side an integer specifying which side of the plot the axis is to be drawn on. The axis is placed as follows: 1=below, 2=left, 3=above and 4=right.
#' 
#' @param line at which MARgin line, starting at 0 counting outwards. Defaults to 0.75.
#' 
#' @param outer use outer margins if available
#' 
#' @param at give location of each string in user coordinates. If \code{text} can be coerced to numeric values, it will try those. Otherwise, if the component of at corresponding to a particular text item is not a finite value (the default), the location will be determined by \code{adj.}
#'
#' @param adj set to NA. See \code{\link[graphics]{mtext}}.
#' 
#' @param padj set to NA. See \code{\link[graphics]{mtext}}.
#' 
#' @param cex character expansion factor. Defaults to 1.2. See \code{\link[graphics]{mtext}} for more details.
#' 
#' @param col color to use.  Can be a vector. \code{NA} values (the default) mean use \code{par("col")}.
#' 
#' @param font font for text. Can be a vector. \code{NA} values (the default) mean use \code{par("font")}.
#' 
#' @param ... Other graphical parameters that may apply to \code{\link[graphics]{mtext}}.
#' 
#'
#'
#' @examples
#' \dontrun{
#'    blank(
#'      xlim = c(0,50),
#'      ylim = c(0,100),
#'      bty ="l"
#'    )
#'    
#'    axis_blank(side = 1, at = seq(0,50,10))
#'    axis_blank(side = 2, at = seq(0,100,20))
#'    axis_label(side = 1)
#'    axis_label(side = 2)
#'    
#' }
#'
#' @export
#' 
axis_label <- function(text, side = 1, line = NA, outer = FALSE, at = NA,
                       adj = NA, padj = NA, cex = 1.7,
                       col = NA, font = NA, ...){
  # assume it's placed at the same spot if numeric
  # and not supplied
  if(all(is.na(at))){
    at <- mean(axTicks(side = side,...))
  } 
  if(is.na(line)){
    my_mar <- par("mar")[side]
    line <- my_mar * 0.6
  }
  
  mtext(text = text, side = side, line = line, outer = outer,
        at = at, adj = adj, padj = padj, cex = cex, col = col,
        font = font, ...
  )
}
