#' Title
#'
#' @return
#' @export
#'
#' @examples
tweet_device = function(which = dev.cur(), 
                        tweet_text = "", 
                        reply = NULL,
                        do_tweet = TRUE,
                        image_height = pkg_options("image_height"),
                        image_aspr = pkg_options("image_aspr"),
                        image_res = pkg_options("image_res"),
                        open_browser = pkg_options("open_browser"))
{
  force(which)

  tf = get_device_image(which, image_height, image_aspr, image_res)
  
  if(do_tweet){
    
    ## Authenticate twitter
    twitter_auth()
    
    status = twitteR::updateStatus(tweet_text,
                                   mediaPath = tf,
                                   bypassCharLimit = TRUE,
                                   inReplyTo = tweet_id_from_text(reply))
    
    if(open_browser){
      url = paste0("https://twitter.com/",status$screenName,"/status/",status$id)
      browseURL(url)
    }
    
  }else{
    status = c(text = tweet_text,
               image = tf)
  }
  
  return(status)
  
}



