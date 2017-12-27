tweet_id_from_text = function(s){
  if(length(s)>1){
    s = s[1]
  }
  
  if(is.null(s)) return(s)
  
  if(nchar(s)==0){
    return(NULL)
  }
  
  contains_slash = grepl("/", s, fixed = TRUE)
  
  if(contains_slash){
    # Probably a URL
    if(!grepl("twitter.com/.+/status/", s, perl = TRUE)){
      stop("Could not extract tweet ID.", s)
    }
    id = strsplit(s, split = "/")[[1]]
    id = id[length(id)]
  }else{
    id = s
  }
  
  ## Check to make sure it is numeric
  if(grepl("\\D", id, perl = TRUE)){
    stop("Not a valid ID (should be a string of digits)", id)
  }
  return(id)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr last
get_device_image = function(which = dev.cur(), 
                            image_height = pkg_options("image_height"),
                            image_aspr = pkg_options("image_aspr"),
                            image_res = pkg_options("image_res"))
{
  
  force(which)
  image_width = image_height * image_aspr
  
  tf = tempfile(fileext = ".png")
  png(filename = tf, height = image_height, width = image_width, res = image_res)
  nd = dplyr::last(dev.list())
  dev.set(which)
  dev.copy(which=nd)
  on.exit({
    dev.off(nd)
  })
  
  return(tf)
}

interactive_devices = function(dev){
  idx = names(dev.list()) %in% deviceIsInteractive()
  dev.list()[idx]
}

#' Post a tweet and get its ID
#'
#' @param ... Arguments to pass to rtweet::post_tweet
#'
#' @return
#' @importFrom rtweet post_tweet get_timeline
#'
#' @examples
post_and_return_id = function(...){
  result = rtweet::post_tweet(...)
  username = result$request$auth_token$credentials$screen_name
  my_timeline <- get_timeline(username, n=1)
  c(id = my_timeline[1,]$status_id, username = username)
}
