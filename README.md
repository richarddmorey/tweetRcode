# tweetRcode
Rstudio addin to tweet from Rstudio

This was cobbled together in a few hours, so *no guarantees*! It works for me. If you want to fix a bug, please make a pull request.


## What it does

Have you ever wanted to...

* ...easily tweet a plot you're working on, from the RStudio interface?
* ...respond to someone on Twitter with R code example and plot, to make a point?
* ...tweet from RStudio, so you don't have to open Twitter and kill your produtivity?

*You're in luck.*

The package has two features (right now):

* **Tweet plain text from R.** Highlight text, select "Get a blog" from the RStudio addins menu (under tweetRcode), and the package will split the text, add tweet numbers (if necessary), and then tweet for you. The tweets will be properly threaded. You can add an active plot to the tweet.

![Tweeting text from RStudio](http://richarddmorey.org/content/img/tweetRcode_tweet.gif)

* **Tweet R code and plots from R.** Highlight R code, select "Tweet R code" from the RStudio addins menu (under tweetRcode), and the package will run the R code and tweet your text/code along with the generated figures. Can also automatically upload a copy to a GitHub gist and link to it.

![Tweeting code from RStudio](http://richarddmorey.org/content/img/tweetRcode_plot.gif)


## What you'll need

* You'll need a recent version of [RStudio](https://www.rstudio.com/products/rstudio/download/) for this, so that you can use [RStudio addins](https://rstudio.github.io/rstudioaddins/).

* If you want to create animated GIFs, you'll need [ImageMagick](https://www.imagemagick.org/script/download.php) installed. If you're a Windows user, you'll need to *make sure* you install the "legacy utilities":

![Legacy utilities](http://richarddmorey.org/content/img/imagemagick.png)


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

2. **Easy** Put the above code in your `.Rprofile`. It will be run every time you start R.

I've found Twitter authentication to be a bit iffy sometimes. If you want to authenticate on start up, you can put this code in your `.Rprofile`:

```
twitteR::setup_twitter_oauth(
    tweetRcode::pkg_options("twitter_api_key"),
    tweetRcode::pkg_options("twitter_api_secret"),
    tweetRcode::pkg_options("twitter_token"),
    tweetRcode::pkg_options("twitter_token_secret")
)
```

You can also run this when code if you're having trouble authenticating while using `tweetRcode`.

## Creating a tweet storm

1. In RStudio, open a new text document (File -> New file -> Text file). 2. Type your tweet storm. A single tweet is fine too; you can just post a single tweet from R.
3. Highlight the entire text.
4. Open the Addins menu from the top of the RStudio window, and click "Get a blog" under "tweetRcode". If you've set up authentication correctly, the tweet should post automatically.

"Get a blog (configurable)" lets you edit the text and post it as a reply, if you like. It will also allow you to add an image from your active R plotting devices.

## Tweeting some R code

You'll need to have configured the authentications before you do this.

1. Open a new R script document in Rstudio.
2. Type some code that you'd like to tweet.
3. Highlight the lines you want to run and tweet.
4. Select "Tweet R Code" from the "Addins" menu at the top of the RStudio window.
5. Select the appropriate options, and click "Done".

Depending on the options you've selected, it may take a few seconds to run the code, generate the images, and upload the tweet and gist.

## Known issues

* There seems to be an issue where posting a reply to *someone else's tweet* gets posted on your own timeline (not as a reply). I assume this is an issue with the [`twitteR`](https://cran.r-project.org/web/packages/twitteR/) package.

* Replies aren't tagged with anyone in the thread. You'll need to mention users explicitly if you want them to be notified that you replied. 
