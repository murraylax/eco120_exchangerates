library(fredr)
library(tidyverse)
library(lubridate)
library(scales)

# Each user should get their own FRED API key
# Go to fred.stlouisfed.org, login (create account if necessary), go to My Account -> API Keys
fredr_set_key("replace_me")

# Data parameters - First and last date, frequency
enddate <- Sys.Date()
begdate <- enddate - years(10) # 10 years data
freq <- "m" # Monthly frequency

# Function to fetch the exchange rate data from FRED
get_exchangerate <- function(fredcode, begdate, enddate, freq) {
  df <- fredr(fredcode, observation_start = begdate, observation_end = enddate, frequency = freq) %>%
    select(Date=date, `Exchange Rate`=value)
  return(df)
}


# Function to plot the exchange rate, given the exchange rate in date frame, df (columns Date and `Exchange Rate`); y-axis label slabel; and filename for where to save .png file
plot_exchangerate <- function(df, slabel, filename) {
  ggplot(df, aes(x=Date, y=`Exchange Rate`)) +
    geom_line(color="dodgerblue4", size=1) +
    scale_x_date(
      date_breaks = "1 year",
      labels = date_format("%Y")) +
    scale_y_continuous(breaks=pretty_breaks(n=8)) +
    theme_bw() +
    theme(text = element_text(size=14)) +
    labs(title="", x="", y=slabel) ->
    er.plt
  show(er.plt)
  ggsave(filename=filename, plot=er.plt, width=8, height=5)
  return(er.plt)
}

fredcodes <- c("EXMXUS", 
               "EXUSAL", 
               "EXCAUS", 
               "EXCHUS", 
               "EXUSEU", 
               "EXJPUS", 
               "DEXKOUS",
               "TWEXAFEGSMTH"
               )
slabels <- c("Mexican Pesos per One U.S. Dollar",
             "U.S. Dollars per One Australian Dollar",
             "Canadian Dollars per One U.S. Dollar",
             "Chinese Yuan per One U.S. Dollar",
             "U.S. Dollars per One Euro",
             "Japanese Yen per One U.S. Dollar",
             "South Korean Won per One U.S. Dollar",
             "Foreign Currency per One U.S. Dollar (Index)"
             )
files <- c("mxn_usd.png", 
           "aud_usd.png", 
           "cad_usd.png", 
           "cny_usd.png", 
           "eur_usd.png", 
           "jpy_usd.png", 
           "rub_usd.png",
           "krw_usd.png",
           "trade.png")

for(i in 1:length(fredcodes)) {
  df <- get_exchangerate(fredcodes[i], begdate, enddate, freq)
  plot_exchangerate(df, slabels[i], files[i])
}
