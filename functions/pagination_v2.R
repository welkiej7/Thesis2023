



paginate_trends <- function(country = "Turkey", path = "~/networked/"){
library(rtweet)
library(tidyverse)





for (trend.check in 1:28) {
  get_trends(country) -> trends.temp
  trends.temp[1:20,] -> trends.temp # nolint: commas_linter.
  for (get.tweets in 1:nrow(trends.temp)) {
    # Get Tweets
    search_tweets(trends.temp$trend[get.tweets], n = 10000,
                  retryonratelimit = TRUE,
                  lang = "tr",
                  type = "mixed") -> tweets.temp
    network_data(tweets.temp) -> network
    #Select Necessary Columns


    #Save the Tweets for Future Research
    message("Writing the Results to the Disk")

    write.csv(network,
              paste(path,trends.temp$trend[get.tweets], sep = ""))

  }
  progress.bar <- txtProgressBar(min = 0, max = 21600, style = 3, char = "%")
  for (time in 1:21600) {
    Sys.sleep(1)
    setTxtProgressBar(progress.bar, value = time)
  }
}
}
