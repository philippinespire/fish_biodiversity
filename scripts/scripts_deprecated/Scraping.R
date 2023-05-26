
#### EXAMPLE: NYTIMES ####
# Tutorial Link: https://statsandr.com/blog/web-scraping-in-r/
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


#### Philippines MPA Scraping ####
library(rvest)
link2 <- "http://www.database.mpasupportnetwork.com/"
PhMPA <- read_html(link2)

PhMPA

data_css <- PhMPA %>%
  html_elements(css = ".dataTables_info")

head(data_css)

NYT_summaries_css <- html_text(summaries_css)

summaries_css2 <- PhMPA %>%
  html_elements(css = ".sorting")

head(summaries_css)


#### Another tutorial ####
# Tutorial site: https://www.scrapingbee.com/blog/web-scraping-r/
# XML package commands: http://www.columbia.edu/~cjd11/charles_dimaggio/DIRE/styled-4/styled-6/code-13/

#install.packages("XML")
library(XML)
url1 <- "http://www.database.mpasupportnetwork.com/"

read1 <- readLines(url1, encoding = "UTF-8", warn=FALSE)

parsed_url1 <- htmlParse(read1, encoding = "UTF-8")

length((readHTMLTable(read1)))
