# tweetRcode
Rstudio addin to tweet from Rstudio

This was cobbled together in a few hours, so *no guarantees*! It works for me. If you want to fix a bug, please make a pull request.

## What you'll need

* You'll need a recent version of [RStudio](https://www.rstudio.com/products/rstudio/download/) for this, so that you can use [RStudio addins](https://rstudio.github.io/rstudioaddins/).

* If you want to create animated GIFs, you'll need [ImageMagick](https://www.imagemagick.org/script/download.php) installed.

* If you want to post GitHub gists, you'll need a [GitHub account](https://github.com).

* You'll need to create app authentication keys for GitHub and twitter. This is easy and only needs to be done once. Instructions are below.

## Installing the package

You can install the package with `devtools`:

    install.packages('devtools') # if necessary
    devtools::install_github("richarddmorey/tweetRcode", 
        subdir = "tweetRcode")
        
You then need to make sure R can authenticate with GitHub (for posting gists) and twitter (for tweeting, of course).

## Authenticating GitHub

In order to post GitHub gists, you need to authenticate to GitHub within R.

There are two ways to do this; an easier one that you'd have to do every session, and a slightly (but only just) more complicated one that you only need to do once.

1. **Easiest** Install the `gistr` package. 

```    
install.packages('gistr')
gistr::gist_auth()
```

This will open up a browser window for you to authenticate.

2. **Easy** Install the `gistr` package. 
    
```
install.packages('gistr')
```

a. Follow [these directions](https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/) to get a GitHub personal identification token (PAT). 

b. Open your `.Rprofile` (if you don't know what that is, it is an R file that gets run everytime you start. Look [here](http://www.noamross.net/blog/2012/11/2/rprofile.html)). Add the lines:
    
```
Sys.setenv(GITHUB_PAT = "YOUR_GIST_PAT_KEY")
```

replacing `YOUR_GIST_PAT_KEY` with the one you generated.

Now when you start R, the key will be set.

## Authenticating Twitter

Authenticating twitter is a bit more complicated, but not much. Follow [these instructions](https://www.slickremix.com/docs/how-to-get-api-keys-and-tokens-for-twitter/) to get your API key, API secret, token, and token secret (you'll need all four). Make sure the "Access level" is "read and write". Copy these four numbers down.

Like with GitHub, there's an easiest way that you have to do every time and a more complicated (but only just) way that you only need to do once.

1. **Easiest** Load the `tweetRcode` package, then define the options you got from twitter as options for `tweetRcode`.

```    
library(tweetRcode)
  tweetRcode::pkg_options(
    twitter_api_key = "YOUR_TWITTER_API_KEY", 
    twitter_api_secret = "YOUR_TWITTER_API_SECRET",
    twitter_token = "YOUR_TWITTER_TOKEN", 
    twitter_token_secret = "YOUR_TWITTER_TOKEN_SECRET" 
  )
```

When you run `tweetRcode` these will be used to authenticate you.

2. **Easy** Put the above code in your `.Rprofile`. It will be run every time you start.

## Tweeting some code

You'll need to have configured the authentications before you do this.

1. Open a new R script document in Rstudio.
2. Type some code that you'd like to tweet.
3. Highlight the lines you want to run and tweet.
4. Select "Tweet R Code" from the "Addins" menu at the top of the RStudio window.
5. Select the appropriate options, and click "Done".

Depending on the options you've selected, it may take a few seconds to run the code, generate the images, and upload the tweet and gist.


