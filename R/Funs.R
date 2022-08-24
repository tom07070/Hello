#' Title
#'Hello World
#'
#' @description
#' `hello` says _"Hello"_ in the user specified language. the user is asked to
#' give her/his name so that the hello message gets personalized.
#'
#'
#' @param who a `character` vector of length 1, ..
#'
#' @param  lang a `character` vector of
#'
#' @param langData a vector see `?language`
#'
#'
#' @return
#' a `character` vector with person
#'
#' @export
#'
#' @examples
#' # Say hello
#' hello("James")
#' hello("Amelia", "Es")
#'
hello <- function(who, lang = 'EN', langData = Hello::language) {
  if (!exists("who", mode = "character") | length(who) > 1) {
    stop("Please enter a valid name; see ?hello")
  }

  langData <- data.frame(langData)

  if (ncol(langData) > 2) {
    stop("Please enter a valid language data set; see ?hello")
  }

  colnames(langData) <- c("code", "hello")

  if ((mode(langData$code) != "character") | (mode(langData$hello) != "character")) {
    stop("Please enter a valid language data set; see ?hello")
  }

  lang2 <- tolower(lang)
  txt <- ifelse(!(lang2 %in% language[,1]),
         paste0("Sorry, ", who, ' your language (', lang, ') is not available!'),
         stringr::str_c(language[language[,1] == lang2, 2],", ", who, "!"))
  cat(txt)
}
