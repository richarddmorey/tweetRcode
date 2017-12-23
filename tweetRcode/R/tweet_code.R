#' Tweet R code 
#'
#' @param code string variable containing R code 
#' @param pre_text Text to prepend to the tweet (null if none)
#' @param print_code print the code in the tweet?
#' @param print_output print the output in the tweet?
#' @param do_gist create a GitHub gist with the code?
#' @param tweet_image Tweet an image with the code (if code creates image)?
#' @param gif_from_images If code creates multiple images, combine them as GIF?
#' @param gif_delay delay, in seconds, between GIF frames
#' @param image_height height of images in pixels
#' @param image_aspr aspect ratio of images
#' @param image_res resolution of images
#' @param envir environment in which to evaluate the R code
#'
#' @return
#' @export
#'
#' @examples
tweet_code <-
function(code,
         pre_text = NULL,
         reply = NULL,
         print_code = pkg_options("print_code"),
         print_output = pkg_options("print_output"),
         print_errors = pkg_options("print_errors"),
         stop_on_errors = pkg_options("stop_on_errors"),
         do_gist = pkg_options("do_gist"),
         do_tweet = TRUE,
         tweet_image = pkg_options("tweet_image"),
         gif_from_images = pkg_options("gif_from_images"),
         gif_delay = pkg_options("gif_delay"),
         image_height = pkg_options("image_height"),
         image_aspr = pkg_options("image_aspr"),
         image_res = pkg_options("image_res"),
         envir = parent.frame()) {
  
  asx = parse(text = code)
  expr = paste(as.character(asx), collapse = "\n")
  out = evaluate::evaluate(expr,envir = envir)
  
  images = sapply(out, function(o) "recordedplot" %in% class(o) )
  tweet_text = ""
  
  for (obj in out) {
     if ( ( "source" %in% class(obj) ) & print_code) {
      tweet_text = paste0(tweet_text, obj)
    } else if ( ( "character" %in% class(obj) ) & print_output) {
      tweet_text = paste0(tweet_text, obj)
    } else if ( ( "error"  %in% class(obj) ) & print_errors) {
      if(stop_on_errors) stop(obj)
      tweet_text = paste0(tweet_text, "## ", obj)
    }
  }
  
  
  if (do_gist) {
    ## Authenticate GitHub
    gistr::gist_auth()
    
    tf_code = tempfile(fileext = ".Rmd")
    cat("```{r echo=TRUE}",
        code,
        "```",
        file = tf_code,
        sep = "\n")
    
    g = gistr::gist_create(
      tf_code,
      knit = TRUE,
      knitopts = list(envir = envir),
      imgur_inject = TRUE,
      browse = FALSE
    )
    gist_url = paste0("https://gist.github.com/", g$id)
    tweet_text = paste0(tweet_text, "\n## ", gist_url)
  }
  
  if (tweet_image) {
    if (sum(images) > 1 & gif_from_images) {
      gif_tf = tempfile(fileext = ".gif")
      frames = c()
      for(i in which(images)){
        frames = c(frames, tempfile(fileext = ".png"))
        png(
          filename = frames[length(frames)],
          height = image_height,
          width = image_aspr * image_height,
          res = image_res
        )
        replayPlot(out[[i]])
        dev.off()
      }
      ao = ani.options()
      ani.options(interval = gif_delay, autobrowse = FALSE)
      animation::im.convert(files = frames, output = gif_tf)
      do.call(ani.options, ao)
      tweet_image_fn = gif_tf
    } else {
      if (sum(images) > 0) {
        tf = tempfile(fileext = ".png")
        png(
          filename = tf,
          height = image_height,
          width = image_aspr * image_height,
          res = image_res
        )
        replayPlot(out[[which(images)[1]]])
        dev.off()
        tweet_image_fn = tf
      } else{
        tweet_image_fn = NULL
      }
    }
  } else{
    tweet_image_fn = NULL
  }
  
  tweet_text = paste0(pre_text, "\n", tweet_text)
  
  ## Authenticate twitter
  twitteR::setup_twitter_oauth(
    tweetRcode::pkg_options("twitter_api_key"),
    tweetRcode::pkg_options("twitter_api_secret"),
    tweetRcode::pkg_options("twitter_token"),
    tweetRcode::pkg_options("twitter_token_secret")
  )
  
  if(do_tweet){
    status = twitteR::updateStatus(tweet_text,
               mediaPath = tweet_image_fn,
               bypassCharLimit = TRUE,
               inReplyTo = reply)
  }else{
    status = c(text = tweet_text,
               image = tweet_image_fn)
  }
  
  return(status)
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
tweetRcodeAddin <- function(){
  
  ui <- miniPage(
    gadgetTitleBar("Tweet R code"),
    miniContentPanel(
      fluidPage(
        column(6,
          textInput("pre_text", "Preface text"),
          checkboxInput("print_code", "Include code in tweet?",  pkg_options("print_code")),
          checkboxInput("print_output", "Include output in tweet?",  pkg_options("print_output")),
          checkboxInput("print_errors", "Include errors in tweet?",  pkg_options("print_errors")),
          checkboxInput("do_gist", "Create gist?",  pkg_options("do_gist")),
          textInput("reply", "Reply to (ID)")
        ),
        column(6,
          checkboxInput("tweet_image", "Include image in tweet?",  pkg_options("tweet_image")),
          checkboxInput("gif_from_images", "Include multiple images in animation?",  pkg_options("gif_from_images")),
          numericInput("gif_delay", "Delay between images (seconds)", pkg_options("gif_delay")),
          numericInput("image_height", "Image height (px)", pkg_options("image_height")),
          numericInput("image_aspr", "Image aspect ratio", pkg_options("image_aspr")),
          numericInput("image_res", "Image resolution (ppi)", pkg_options("image_res"))
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    # Get the document context.
    context <- rstudioapi::getActiveDocumentContext()
    
    observeEvent(input$done, {
      # Collect inputs
      if(!is.null(input$reply)){
        if(input$reply==""){
          reply = NULL
        }else{
          reply = input$reply
        }
      }
      
      selection_text <- unname(unlist(context$selection)["text"])

      # if the selection has no characters (ie. there is no selection), then count the words in the full text of the Rmd
      if(nchar(selection_text) > 0){
        my_text = selection_text
      } else  {
        my_text = context$contents
      }

      status = tweetRcode::tweet_code(code = my_text,
                                      reply = reply,
                                      pre_text = input$pre_text,
                                      print_code = input$print_code,
                                      print_output = input$print_output,
                                      print_errors = input$print_errors,
                                      stop_on_errors = pkg_options("stop_on_errors"),
                                      do_gist = input$do_gist,
                                      do_tweet = TRUE,
                                      tweet_image = input$tweet_image,
                                      gif_from_images = input$gif_from_images,
                                      gif_delay = input$gif_delay,
                                      image_height = input$image_height,
                                      image_aspr = input$image_aspr,
                                      image_res = input$image_res,
                                      envir = new.env()
                                      )
      invisible(stopApp())
    })
    
  }
  
  viewer <- dialogViewer("Tweet R code", width = 1000, height = 800)
  runGadget(ui, server, viewer = viewer)
  

}

