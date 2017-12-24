#' Tweet, potentially creating tweet storms by splitting tweets on white space
#'
#' @param s character string; text of tweet(s) 
#' @param leave_space integer; how much space to leave at the end for numbering
#' @param max_char maximum characters that can go in a tweet
#' @param do_tweet logical; tweet, or just return the splitted text?
#'
#' @return
#' @export
#'
#' @examples
get_a_blog <- function(s, 
                     leave_space = pkg_options("getablog_leave_space"),
                     max_char = pkg_options("getablog_max_char"), do_tweet = TRUE)
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
        statuses[[i]] = twitteR::updateStatus(splits[i], bypassCharLimit = TRUE)
      }else{
        statuses[[i]] = twitteR::updateStatus(splits[i], inReplyTo = statuses[[i-1]]$id, bypassCharLimit = TRUE)
      }
    }
    return(statuses)
  }else{
    return(splits)
  }
}


get_a_blog_addin = function(){

  context <- rstudioapi::getActiveDocumentContext()
  
  # get selection text and full text of Rmd
  selection_text <- unname(unlist(context$selection)["text"])

  # if the selection has no characters (ie. there is no selection), then count the words in the full text of the Rmd
  if(nchar(selection_text) < 1){
    stop("No text selected.")
  }
  
  cat(selection_text)
  get_a_blog(selection_text, do_tweet = TRUE)
}



