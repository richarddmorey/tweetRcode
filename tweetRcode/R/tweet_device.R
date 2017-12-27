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
                        image_res_scale = pkg_options("image_res_scale"),
                        open_browser = pkg_options("open_browser"))
{
  force(which)

  tf = get_device_image(which, image_height, image_aspr, image_res, image_res_scale)
  
  if(do_tweet){
    
    status = post_and_return_id(tweet_text,
                                media = tf,
                                in_reply_to_status_id = tweet_id_from_text(reply))
    
    if(open_browser){
      url = paste0("https://twitter.com/",status['username'],"/status/",status['id'])
      browseURL(url)
    }
    
  }else{
    status = c(text = tweet_text,
               image = tf)
  }
  
  return(status)
  
}



