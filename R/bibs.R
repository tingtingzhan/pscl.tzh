

#' @title bibs of \CRANpkg{pscl}
#' 
#' @param key,... \link[utils]{bibentry}
#' 
#' @keywords internal
#' @importFrom utils bibentry person
#' @name pscl_bib
#' @export
.lambert92 <- \(key = 'Lambert92', ...) {
  bibentry(
    bibtype = 'article', key = key, ...,
    author = person(given = 'Diane', family = 'Lambert'),
    journal = 'Technometrics',
    number = '1',
    pages = '1--14',
    title = 'Zero-Inflated Poisson Regression, with an Application to Defects in Manufacturing',
    volume = '34',
    year = '1992',
    doi = '10.2307/1269547'
  )
}
