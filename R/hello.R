#' Hello!
#'
#' I'd like to say hello!
#' @param name Tell me your name!
#' @keywords hello
#' @export
#' @examples
#' hello()

hello <- function(name=NULL) {
    if(is.null(name)) {
        print('Hello, world!')
    } else {
        print(paste0('Hello, ', name, '!'))
    }
}
