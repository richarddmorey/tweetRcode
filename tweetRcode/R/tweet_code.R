#' Tweet R code 
#'
#' @param code string variable containing R code 
#' @param pre_text Text to prepend to the tweet (null if none)
#' @param reply ID of a post to reply to (null if not a reply)
#' @param print_code print the code in the tweet?
#' @param print_output print the output in the tweet?
#' @param print_errors print the errors in the tweet?
#' @param stop_on_errors stop if an error is encountered?
#' @param do_gist create a GitHub gist with the code?
#' @param tweet_image Tweet an image with the code (if code creates image)? Options are "none", "gif", "first", and "last"
#' @param gif_delay delay, in seconds, between GIF frames
#' @param image_height height of images in pixels
#' @param image_aspr aspect ratio of images
#' @param image_res resolution of images
#' @param envir environment in which to evaluate the R code
#' @param open_browser open browser to tweet?
#'
#' @return
#' @export
#' @importFrom evaluate evaluate
#' @importFrom animation im.convert ani.options
#' @importFrom gistr gist_create
#' @importFrom dplyr last
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
         gif_delay = pkg_options("gif_delay"),
         image_height = pkg_options("image_height"),
         image_aspr = pkg_options("image_aspr"),
         image_res = pkg_options("image_res"),
         envir = parent.frame(),
         open_browser = pkg_options("open_browser")) {
  

  if(!(tweet_image %in% c("none", "gif", "first", "last"))){
    stop("Invalid value for tweet_image: ", tweet_image)
  }
    
  asx = parse(text = code)
  expr = paste(as.character(asx), collapse = "\n")
  if(substring(expr,nchar(expr)) != "\n"){
    expr = paste0(expr, "\n")
  }
  
  tf = get_tf_wildcard()
  
  # evaluate code dumping images to png files
  png(filename = tf$name, height = image_height,
      width = image_aspr * image_height,
      res = image_res)
  dv = dev.cur()
  on.exit( { dev.off(dv) }, add = TRUE)
  out = evaluate::evaluate(expr, envir = envir, new_device = FALSE, 
                           stop_on_error = stop_on_errors)
  dev.off(dv)

  # get the image paths
  images = dir(tf$dir, tf$pattern, full.names = TRUE)
  
  # Convert results to tweet text
  # Note that this excludes warnings and messages
  tweet_text = ""
  
  for (obj in out) {
     if ( ( "source" %in% class(obj) ) & print_code) {
      tweet_text = paste0(tweet_text, obj)
    } else if ( ( "character" %in% class(obj) ) & print_output) {
      tweet_text = paste0(tweet_text, "## " ,obj)
    } else if ( ( "error"  %in% class(obj) ) & print_errors) {
      if(stop_on_errors) stop(obj)
      tweet_text = paste0(tweet_text, "## ", obj)
    }
  }
  
  # Create a gist
  if (do_gist) {

    h = image_height / image_res
    w = h * image_aspr
    
    td = basename(tempfile(pattern = "tmp"))
    if(dir.exists(td)){
      stop("Could not create temporary directory for gist images.")
    }
    on.exit({
      if(dir.exists(td)){ unlink(td, recursive = TRUE) }
    }, add = TRUE)
    
    tf_code = tempfile(fileext = ".Rmd")
    cat("```{r echo=TRUE, fig.path='",td,"/', fig.width=",w,
        ", fig.height=",h,", dpi=", image_res,"}\n",
        expr,
        "\n```",
        file = tf_code,
        sep = "")
    
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
  
  # Create images
  if (tweet_image != "none" & length(images)>0) {
    if( length(images)==1){
      tweet_image_fn = images[1]
    }else if(tweet_image == "gif") {
      gif_tf = tempfile(fileext = ".gif")
      ao = animation::ani.options()
      animation::ani.options(interval = gif_delay, autobrowse = FALSE)
      animation::im.convert(files = images, output = gif_tf)
      do.call(animation::ani.options, ao)
      tweet_image_fn = gif_tf
    } else if(tweet_image == "first") {
      tweet_image_fn = images[1]
    } else if(tweet_image == "last"){
      tweet_image_fn = dplyr::last(images)  
    }else{  
      tweet_image_fn = NULL
    }
  }else{
    tweet_image_fn = NULL
  }
  
  tweet_text = paste0(pre_text, "\n", tweet_text)
  
  # tweet
  if(do_tweet){
    
    status = post_and_return_id(tweet_text,
                                media = tweet_image_fn,
                                in_reply_to_status_id = tweet_id_from_text(reply))
    
    if(open_browser){
      url = paste0("https://twitter.com/",status['username'],"/status/",status['id'])
      browseURL(url)
    }
    
  }else{
    status = c(text = tweet_text,
               image = tweet_image_fn)
  }
  
  return(status)
}

get_tf_wildcard <- function(){
  tf_base = tempfile()
  tf_ext = "_%04d.png"
  tf_dir = dirname(tf_base)
  tf = paste0(tf_base,tf_ext)
  tf_pattern = paste0("^",basename(tf_base),"_[0-9]*\\.png")
  return(list(name = tf, pattern = tf_pattern, dir = tf_dir))
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
          textAreaInput("pre_text", "Preface text", height=100),
          checkboxInput("print_code", "Include code in tweet?",  pkg_options("print_code")),
          checkboxInput("print_output", "Include output in tweet?",  pkg_options("print_output")),
          checkboxInput("print_errors", "Include errors in tweet?",  pkg_options("print_errors")),
          checkboxInput("do_gist", "Create gist?",  pkg_options("do_gist")),
          textInput("reply", "Reply to (Tweet URL or ID)"),
          textOutput("warnUser")
        ),
        column(6,
               selectInput("tweet_image", "Include image?", 
                           c(None="none","All with GIF"="gif",First="first",Last="last"), pkg_options("tweet_image")),
               conditionalPanel("input.tweet_image != 'none'",
                                conditionalPanel("input.tweet_image == 'gif'",
                                  numericInput("gif_delay", "Delay between images (seconds)", pkg_options("gif_delay"))
                                ),
                                numericInput("image_height", "Image height (px)", pkg_options("image_height")),
                                numericInput("image_aspr", "Image aspect ratio", pkg_options("image_aspr")),
                                numericInput("image_res", "Image resolution (ppi)", pkg_options("image_res"))
               )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    # Get the document context.
    context <- rstudioapi::getActiveDocumentContext()
    
    output$warnUser <- renderText({
      
      id = tweet_id_from_text(input$reply)
      status = rtweet::lookup_statuses(id)
      if(is.null(status) | input$reply == "") return("")
      username = paste0("@",status$screen_name)
      
#      cur_text = input$pre_text
#      if(substring(cur_text, 1, nchar(username)) != username){
#        new_text = paste(username, " ", cur_text, sep = "")
#        updateTextAreaInput(session, "pre_text", value = new_text)
#      }
      
      
      txt = paste("If you are not ", username, 
                  ", you'll need to mention them (e.g., in the preface text).", sep = "")
      
      return(txt)
      
    })
    
    observeEvent(input$done, {
      # Collect inputs
      if(!is.null(input$reply)){
        if(input$reply==""){
          reply = NULL
        }else{
          input$reply        
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

