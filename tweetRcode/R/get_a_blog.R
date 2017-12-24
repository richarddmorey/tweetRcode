#' Tweet, potentially creating tweet storms by splitting tweets on white space
#'
#' @param s character string; text of tweet(s) 
#' @param leave_space integer; how much space to leave at the end for numbering
#' @param max_char maximum characters that can go in a tweet
#' @param open_browser open browser to tweet?
#' @param reply ID of a post to reply to (null if not a reply)
#' @param do_tweet logical; tweet, or just return the splitted text?
#'
#' @return
#' @export
#' @import twitteR
#'
#' @examples
get_a_blog <- function(s, 
                     leave_space = pkg_options("getablog_leave_space"),
                     max_char = pkg_options("getablog_max_char"),
                     open_browser = pkg_options("open_browser"),
                     reply = NULL,
                     do_tweet = TRUE)
{
  
  split_at = max_char - leave_space
  s0 = unlist(strsplit(s, " ", fixed = TRUE))
  nc = nchar(s0)
  cc = cumsum(nc)
  cc = cc + 1:length(cc)
  
  total_word = 0
  total_char = 0
  splits = c()
  done = FALSE
  
  while(!done){
    
    if(total_word == 0){
      new_split = max(which(cc < split_at))
    }else{
      new_split = new_split + max(which(cc[-(1:total_word)] - total_char < split_at ))
    }
  
    
    splits = c(splits, paste(paste(s0[(total_word + 1):new_split]," ", sep = ""), collapse = ""))
    total_char = sum(sapply(splits, nchar))
    total_word = new_split
    if(total_word >= length(cc))
      done = TRUE
  }
  
  splits = trimws(splits)
  
  if(length(splits)>1)
    splits = paste(splits, " (",1:length(splits),"/",length(splits),")", sep = "")
  
  if(do_tweet){
    
    ## Authenticate twitter
    twitteR::setup_twitter_oauth(
      pkg_options("twitter_api_key"),
      pkg_options("twitter_api_secret"),
      pkg_options("twitter_token"),
      pkg_options("twitter_token_secret")
    )
    
    statuses = list()
    
    for(i in 1:length(splits)){
      if(i == 1){
        statuses[[i]] = twitteR::updateStatus(splits[i], inReplyTo = reply, bypassCharLimit = TRUE)
      }else{
        statuses[[i]] = twitteR::updateStatus(splits[i], inReplyTo = statuses[[i-1]]$id, bypassCharLimit = TRUE)
      }
    }
    
    if(open_browser){
      url = paste0("https://twitter.com/",statuses[[1]]$screenName,"/status/",statuses[[1]]$id)
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
    miniContentPanel(
      fluidPage(
        textAreaInput("tweet_text", "Tweet text", unname(unlist(rstudioapi::getActiveDocumentContext()$selection)["text"]), 
                      width = "500px", height="300px"),
        textInput("reply", "Reply to (ID)"),
        checkboxInput("browser", "Open browser after?",  pkg_options("getablog_open_browser"))
      )
    )
  )
  
  server <- function(input, output, session) {
    

    observeEvent(input$done, {
      
      # Collect inputs
      if(!is.null(input$reply)){
        if(input$reply==""){
          reply = NULL
        }else{
          reply = input$reply
        }
      }
  
      
      get_a_blog(input$tweet_text, reply = reply, do_tweet = TRUE, open_browser = input$browser)
      
      invisible(stopApp())
    })
    
  }
  
  viewer <- dialogViewer("Tweet from R", width = 1000, height = 800)
  runGadget(ui, server, viewer = viewer)
  
  
}



