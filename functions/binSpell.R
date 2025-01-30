##############
## binSpell ##
##############
#' @title Function to compute number and length of binary (e.g. 0=dry, 1=wet) spells
#' @return Sequence of spells, including the duration of each spell. The search is done within the "data" vector
#' @param data Vector with data (e.g. daily precipitation) (pr < thereshold for rachas))
#' @author R. Manzanas
#' @export
#' 
#' @micaso Trabajo con un vector de datos binarios (0,1) y quiero saber cuantas rachas de 0 y 1 hay de una serie diaria
#' lo que hace la función es compoara si cada valor es igual al anterior.
#' 
#' @NAvalues Cuando el valor es NA, considera las racha de valor NA, añadiendo el valor del día o los días anteriores.

binSpell <- function(data) {
  data[is.na(data)] <- FALSE # add Ana

  ix <- c(which(data[-length(data)] != data[-1]), length(data))  
 
  # output list
  out <- list()
  out$len <- diff(c(0, ix))
  out$val <- data[ix]
  return(out)
}