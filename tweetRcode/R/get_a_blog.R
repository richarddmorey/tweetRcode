#' Tweet, potentially creating tweet storms by splitting tweets on white space
#'
#' @param s character string; text of tweet(s) 
#' @param leave_space integer; how much space to leave at the end for numbering
#' @param max_char maximum characters that can go in a tweet
#' @param open_browser open browser to tweet?
#' @param reply ID of a post to reply to (null if not a reply)
#' @param do_tweet logical; tweet, or just return the splitted text?
#' @param image_device Device number to share as image
#' @param image_height Image height (px)
#' @param image_aspr Image aspect ratio (width / height)
#' @param image_res Image resolution (ppi)
#' @param split_on User-defined POSIX regular expression for forced splits in tweets
#' @param n_format format for tweet numbering, passed to sprintf()
#'
#' @return
#' @export
#' 
#' @examples
get_a_blog <- function(s, 
                     leave_space = pkg_options("getablog_leave_space"),
                     max_char = pkg_options("getablog_max_char"),
                     open_browser = pkg_options("open_browser"),
                     reply = NULL,
                     do_tweet = TRUE,
                     image_device = NULL,
                     image_height = pkg_options("image_height"),
                     image_aspr = pkg_options("image_aspr"),
                     image_res = pkg_options("image_res"),
                     split_on = pkg_options("getablog_split_on"),
                     n_format = pkg_options("getablog_n_format"))
{
  
  reply = tweet_id_from_text(reply)
  
  if(!is.null(image_device)){
    tf = get_device_image(image_device, 
                          image_height,
                          image_aspr,
                          image_res)
  }else{
    tf = NULL 
  }
  
  if( nchar(s) > max_char ){
    splits = split_tweet0(s, split_on, leave_space, max_char)
  }else{
    splits = s
  }
  
  if(length(splits)>1)
    splits = paste(splits, sprintf(n_format, 1:length(splits), length(splits)), sep = " ")
  
  if(do_tweet){
    
    statuses = list()
    
    for(i in 1:length(splits)){
      if(i == 1){
        statuses[[i]] = post_and_return_id(status = splits[i], 
                                           in_reply_to_status_id = reply, 
                                           media = tf)
      }else{
        statuses[[i]] = post_and_return_id(status = splits[i], 
                                           in_reply_to_status_id = statuses[[i-1]]['id'])
      }
    }
    
    if(open_browser){
      url = paste0("https://twitter.com/",statuses[[1]]['username'],"/status/",statuses[[1]]['id'])
      browseURL(url)
    }
    
    return(statuses)
  }else{
    return(splits)
  }
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
#' @import rstudioapi
get_a_blog_addin_simple = function(){

  context <- rstudioapi::getActiveDocumentContext()
  
  # get selection text and full text of Rmd
  selection_text <- unname(unlist(context$selection)["text"])

  # if the selection has no characters (ie. there is no selection), then count the words in the full text of the Rmd
  if(nchar(selection_text) < 1){
    stop("No text selected.")
  }
  
  get_a_blog(selection_text, do_tweet = TRUE)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
#' @import miniUI
#' @import rstudioapi  
#' @import shiny
get_a_blog_addin_settings <- function(){
  
  ui <- miniPage(
    gadgetTitleBar("Tweets and tweet storms"),
    tags$head(tags$style(
      type="text/css",
      "#image1 img {max-width: 100%; width: 100%; height: auto}"
    )),
    miniContentPanel(
      fluidPage(
        column(6,
        textAreaInput("tweet_text", "Tweet text", unname(unlist(rstudioapi::getActiveDocumentContext()$selection)["text"]), 
                      width = "450px", height="225px", resize = "vertical"),
        textInput("reply", "Reply to (Tweet URL or ID)"),
        textOutput("warnUser")
        ),
        column(6,
          selectInput("device", "Image from device?", c(None=-1,interactive_devices())),
          conditionalPanel("input.device != -1",
            imageOutput("image1", height = 100, width=150),
            numericInput("image_height", "Image height (px)", pkg_options("image_height")),
            numericInput("image_aspr", "Image aspect ratio", pkg_options("image_aspr")),
            numericInput("image_res", "Image resolution (ppi)", pkg_options("image_res"))
          )
         )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    output$warnUser <- renderText({
      
      if(input$reply == "") return("")
      
      id = tweet_id_from_text(input$reply)
      txt = mention_note_text(id)
            
      return(txt)
      
    })
    
    output$image1 <- renderImage({
      if(input$device==-1){
        tf = system.file("img/blank.png", package="tweetRcode")
        width=1
        height=1
      }else{  
        # Get width and height of image1
        height <- input$image_height
        aspr <- input$image_aspr
        width = height * aspr 
        res = input$image_res
        pixelratio <- session$clientData$pixelratio
      
        device = input$device
        tf = get_device_image(device, 
                            height,
                            aspr,
                            res)
      }  
      # Return a list containing information about the image
      list(src = tf,
           contentType = "image/png",
           width = width,
           height = height,
           alt = "Device preview")
      
    }, deleteFile = TRUE)
    
    

    observeEvent(input$done, {
      
      # Collect inputs
      if(!is.null(input$reply)){
        if(input$reply==""){
          reply = NULL
        }else{
          reply = tweet_id_from_text(input$reply)
        }
      }
      
      if(input$device!=-1){
        image_device = input$device
      }else{
        image_device = NULL
      }

      get_a_blog(input$tweet_text, reply = reply, image_device = image_device,
                 image_height = input$image_height, 
                 image_aspr = input$image_aspr,
                 image_res = input$image_res)
      
      invisible(stopApp())
    })
    
    
  }
  
  viewer <- dialogViewer("Tweet from R", width = 1000, height = 800)
  runGadget(ui, server, viewer = viewer)
}


#' Initial user-defined splits, then passes each segment off 
## to be split by size
#'
#' @param s string containing tweet
#' @param split_on  User-defined POSIX regular expression for forced splits in tweets
#' @param leave_space integer; how much space to leave at the end for numbering
#' @param max_char maximum characters that can go in a tweet
#'
#' @return
#' @importFrom stringi stri_wrap
#'
#' @examples
split_tweet0 = function(s, split_on = "\\n---*\\n", leave_space, max_char){
  splt = strsplit(s, split_on)[[1]]
  stringi::stri_wrap(splt, max_char - leave_space, whitespace_only = TRUE, simplify = TRUE)
}
