# tweetRcode
Rstudio addin to tweet from Rstudio

This was cobbled together in a few hours, so *no guarantees*! It works for me. If you want to fix a bug, please make a pull request.

**If you installed this on or before 25 Dec 2017, you'll need to redo the authentication for twitter. See instructions below.**

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

## Authenticating Twitter

Authenticating twitter is a bit more complicated, but not much. Follow [these instructions](https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html) and you'll be good to go.

**If you followed the old instructions for the `twitteR` package, you'll need to redo it; the new functions are based on the `rtweet` package.**


## Authenticating GitHub

In order to post GitHub gists, you need to authenticate to GitHub within R.

Follow [these directions](https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/) to get a GitHub personal identification token (PAT). 

Open the `.Renviron` file that was created/appended in the steps to authenticate Twitter. On the first new line of the `.Renviron` file (after everything else), add

```
GITHUB_PAT = YOUR_GIST_PAT_KEY
```

replacing `YOUR_GIST_PAT_KEY` with the one you generated. Save the file.

Restart R, and type this in the R console:

```
Sys.getenv()[c("TWITTER_PAT","GITHUB_PAT")]
```

and you should see your PAT tokens. 


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

* If you want to post a reply to someone else's tweet, you'll need to mention the owner of that tweet in order to properly reply. If you don't, the tweet will be posted to your timeline (i.e., not a reply). This is a limitation of the Twitter API.

* Replies don't seem to be tagged with anyone in the thread. You'll need to mention users explicitly if you want them to be notified that you replied. 

## See also

* [rtweet](https://github.com/mkearney/rtweet): used for Twitter API calls. You can use this for much more than tweeting for R.

* [gistr](https://github.com/ropensci/gistr): used for GitHub gist API calls.

* [codefinch](https://github.com/ropenscilabs/codefinch): RStudio addin for tweeting images of R code. Similar to tweetRcode but different in execution.

* [hrbraddins](https://github.com/hrbrmstr/hrbraddins): Includes RStudio addin for Tweet storms, but with fewer features.


