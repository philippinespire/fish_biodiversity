#### Tutorial from uc-r.github.io using example website ####
# Tutorial link: http://uc-r.github.io/scraping_HTML_tables
webpage2 <- read_html("http://www.bls.gov/web/empsit/cesbmart.htm")

tbls0 <- html_nodes(webpage2, "table") # Describes website sections manually named "Table#"
tbls1 <- html_nodes(webpage2, "tr")
tbls2 <- html_nodes(webpage2, "td")
tbls3 <- html_nodes(webpage2, "th")

# each of these elements bring info back



head(tbls0)
head(tbls1)
head(tbls2) #HTML script says <td headers=.. is under th id=...
head(tbls3) #

text0 <- html_text(tbls0)
text2 <- html_text(tbls2)
head(text0)


#### Tutorial from uc-r.github.io using MPA website ####
# Tutorial link: http://uc-r.github.io/scraping_HTML_tables
library(rvest)
library(XML)
library(tidyverse)

webpage <- read_html("http://www.database.mpasupportnetwork.com/")

ptbls1 <- html_nodes(webpage, "h3") #               Doesn't work:
ptbls2 <- html_nodes(webpage, "div") #            td, th, div class,
ptbls3 <- html_nodes(webpage, "style") #          tr, tfoot, tbody,
ptbls4 <- html_nodes(webpage, "div") #            table, why-us, u
ptbls5 <- html_nodes(webpage, "a") #
ptbls6 <- html_nodes(webpage, "i") #
ptbls7 <- html_nodes(webpage, "li") #
ptbls8 <- html_nodes(webpage, "ul") #
ptbls9 <- html_nodes(webpage, "table")


text1 <- html_text(ptbls1)
text2 <- html_text(ptbls2)
text3 <- html_text(ptbls3)
text4 <- html_text(ptbls4)
text5 <- html_text(ptbls5)
text6 <- html_text(ptbls6)
text7 <- html_text(ptbls7)
text8 <- html_text(ptbls8)
text9 <- html_text(ptbls9)

head(ptbls1)
head(ptbls2)
head(ptbls3)
head(ptbls4)
head(ptbls5)
head(ptbls6)

#### uc-r.github.io tutorial with wikipedia page ####

wiki <- read_html("https://en.wikipedia.org/wiki/Ashley_Tisdale")

cell <- html_nodes(wiki, "a")
cell1 <- html_nodes(wiki, "td")
cell2 <- html_nodes(wiki, "tr")
cell3 <- html_nodes(wiki, "th")
cell4 <- html_nodes(wiki, "table")
cell5 <- html_nodes(wiki, "li")

head(cell)
head(cell1)
head(cell2)
head(cell3)
head(cell4)
head(cell5)

textcell <- html_text(cell)
textcell1 <- html_text(cell1) #view(textcell1) lists each table cell l->r, t->b
textcell2 <- html_text(cell2) #view(textcell2) lists table rows individually in 1 chr string
textcell3 <- html_text(cell3)
textcell4 <- html_text(cell4)
textcell5 <- html_text(cell5) #view(textcell5) lists all subheadings

head(textcell)
head(textcell1)
head(textcell2)
head(textcell3)
head(textcell4)



#### ChatGPT with wikipedia page ####

# load the required libraries
library(rvest)
library(tidyverse)
library(XML)

# specify the URL of the Wikipedia page
url2 <- "https://en.wikipedia.org/wiki/Ashley_Tisdale"

# read the HTML content of the page
page2 <- read_html(url2)

# extract the table with the caption "Awards and nominations"
awards_table2 <- page2 %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>%
  html_table(fill = TRUE)

view(awards_table2)

# Success! 


#### ChatGPT attempt with MPA Database ####

library(rvest)

# specify the URL of the webpage
url <- "http://www.database.mpasupportnetwork.com/"

# read the HTML code from the webpage
webpage <- read_html(url)

# extract the table with the "Marine Protected Areas List" caption
mpa_node <- webpage %>%
  html_nodes(xpath = '/html/body/main/section[3]/div/table[1]')

# convert the table node to a data frame
mpa_table <- html_table(mpa_node)[[1]]

# display the table
mpa_table


#### Tutorial from statsandr.com using MPA website ####
link <- "http://www.database.mpasupportnetwork.com/"
MPApage <- read_html(link)

MPA_css <- MPApage %>%
  html_elements(css = ".summary-class")

head(summaries_css)

#### ChatGPT ####
library(rvest)

url <- "http://www.database.mpasupportnetwork.com/"
webpage <- read_html(url)
element <- html_nodes(webpage, "td[style][class]")
text <- html_text(element)

head(element)


url <- "http://www.database.mpasupportnetwork.com/"
webpage <- read_html(url)
table <- html_table(webpage)[[1]]
text <- table[table == "Bacud Reef"]
