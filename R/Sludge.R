#' sludge data set
#'
#' @description A data set containing measurements of methane gas (CH4) emissions from
#' different sludge specimens, with and without added VHA nutrients. The data set is
#' used to study the effect of processing time and nutrient addition on methane emissions
#' from sludge.
#'
#' @format A data frame with 27 rows and 4 columns:
#' \describe{
#'   \item{CH4}{Methane emission in milligrams per liter, a numeric vector.}
#'   \item{TIME}{Processing time in days, a numeric vector.}
#'   \item{VHA}{A numeric vector where 1 indicates that VHA nutrients were added and 0 indicates no addition of VHA nutrients.}
#'   \item{VHA.Added}{A logical vector indicating whether VHA nutrients were added, where 'Yes' means VHA was added, and 'No' means it wasn't.}
#' }
#'
#' @details The data provides insights into the impact of adding VHA nutrients to sludge
#' and the processing time on the emission of methane gas to understand and mitigate greenhouse
#' gas emissions.
#'
#' @source Textbook exercise 12.51.
#'
#' @examples
"sludge"
