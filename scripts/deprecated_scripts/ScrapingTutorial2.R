
## Install & Evoke packages
install.packages("rvest")
library(rvest)

# Loading websites to scrap data
url <- read_html("https://www.alexa.com/topsites/countries/IN")


Sites <- url %>% html_nodes(".DescriptionCell a") %>% html_text()
Dailytime <- url %>% html_nodes(".DescriptionCell+ .right p") %>% html_text()
DailyPageViews <-  url %>% html_nodes(".td:nth-child(4) p") %>% html_text()
Trafficpersearch <- url %>% html_nodes(".td:nth-child(5) p") %>% html_text()
Sitelinked <- url %>% html_nodes(".td:nth-child(6) p") %>% html_text()


# Creating a DataFrame
Alexatopsites <- data.frame(Sites, Dailytime, DailyPageViews, Trafficpersearch,Sitelinked)
View(Alexatopsites)

# Write the data to a file
getwd()
write.csv(Alexatopsites, file = "Alexa Top Sites.csv")

getwd()


#### EXAMPLE: NYTIMES ####
library(rvest)
link <- "https://www.nytimes.com/"
NYT_page <- read_html(link)

NYT_page

# one method
summaries_css <- NYT_page %>%
  html_elements(css = ".summary-class")

head(summaries_css)

# # other method
# summaries_xpath <- NYT_page %>%
#   html_elements(xpath = "//*[contains(@class, 'summary-class')]")
# 
# head(summaries_xpath)

NYT_summaries_css <- html_text(summaries_css)

head(NYT_summaries_css)

# All summaries available now


#### Philippines ####
link2 <- "http://www.database.mpasupportnetwork.com/"
PhMPA <- read_html(link2)

PhMPA

summaries_css <- PhMPA %>%
  html_elements(css = ".td")

head(summaries_css)

summaries_css2 <- PhMPA %>%
  html_elements(css = ".sorting")

head(summaries_css)

# # other method
# summaries_xpath <- NYT_page %>%
#   html_elements(xpath = "//*[contains(@class, 'summary-class')]")
# 
# head(summaries_xpath)

NYT_summaries_css <- html_text(summaries_css)

head(NYT_summaries_css)
