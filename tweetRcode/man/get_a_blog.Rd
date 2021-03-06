% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_a_blog.R
\name{get_a_blog}
\alias{get_a_blog}
\title{Tweet, potentially creating tweet storms by splitting tweets on white space}
\usage{
get_a_blog(s, leave_space = pkg_options("getablog_leave_space"),
  max_char = pkg_options("getablog_max_char"),
  open_browser = pkg_options("open_browser"), reply = NULL,
  do_tweet = TRUE, image_device = NULL,
  image_height = pkg_options("image_height"),
  image_aspr = pkg_options("image_aspr"),
  image_res = pkg_options("image_res"),
  image_res_scale = pkg_options("image_res_scale"),
  split_on = pkg_options("getablog_split_on"),
  n_format = pkg_options("getablog_n_format"))
}
\arguments{
\item{s}{character string; text of tweet(s)}

\item{leave_space}{integer; how much space to leave at the end for numbering}

\item{max_char}{maximum characters that can go in a tweet}

\item{open_browser}{open browser to tweet?}

\item{reply}{ID of a post to reply to (null if not a reply)}

\item{do_tweet}{logical; tweet, or just return the splitted text?}

\item{image_device}{Device number to share as image}

\item{image_height}{Image height (px)}

\item{image_aspr}{Image aspect ratio (width / height)}

\item{image_res}{Image resolution (ppi)}

\item{image_res_scale}{scale factor to multiply dimensions and resolution by}

\item{split_on}{User-defined POSIX regular expression for forced splits in tweets}

\item{n_format}{format for tweet numbering, passed to sprintf()}
}
\description{
Tweet, potentially creating tweet storms by splitting tweets on white space
}
