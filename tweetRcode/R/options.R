# Variable, global to package's namespace. 
# This function is not exported to user space and does not need to be documented.
MYPKGOPTIONS <- settings::options_manager(
  open_browser = TRUE,
  print_code = TRUE,
  print_output = FALSE,
  print_errors = TRUE,
  stop_on_errors = TRUE,
  do_gist = TRUE,
  tweet_image = TRUE,
  gif_from_images = TRUE,
  gif_delay = 2,
  image_height = 800,
  image_aspr = 2,
  image_res = 200,
  twitter_api_key = NULL, 
  twitter_api_secret = NULL,
  twitter_token = NULL, 
  twitter_token_secret = NULL,
  getablog_leave_space = 9,
  getablog_max_char = 280
)


# User function that gets exported:

#' Set or get options for my package
#' 
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported
#' \itemize{
#'  \item{\code{print_code}}{(\code{logical};TRUE) Print R code to the tweet? }
#'  \item{\code{print_output}}{(\code{logical};FALSE) Print R output to the tweet? }
#'  \item{\code{do_gist}}{(\code{logical};TRUE) Create a gist, and link to it? }
#'  \item{\code{tweet_image}}{(\code{logical};TRUE) Add an image to the tweet? }
#'  \item{\code{gif_from_images}}{(\code{logical};TRUE) If multiple plots, create an animated GIF? }
#'  \item{\code{gif_delay}}{(\code{numeric};2) Delay between GIF frame transitions (seconds) }
#'  \item{\code{image_height}}{(\code{numeric};800) Image height, in pixels }
#'  \item{\code{image_aspr}}{(\code{numeric};2) Image aspect ratio }
#'  \item{\code{image_res}}{(\code{numeric};200) Image resolution }

#' }
#'
#' @export
#' @import settings
pkg_options <- function(...){
  # protect against the use of reserved words.
  settings::stop_if_reserved(...)
  MYPKGOPTIONS(...)
}
