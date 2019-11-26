url <- "https://www.mobile01.com/forumtopic.php?c=30"

title <- read_html(url)
title_nodes <- html_nodes(title, ".u-ellipsis")
title_text <- html_text(title_nodes) %>% 
  iconv(from = "UTF-8", to = "UTF-8")
View(title_text)
####
m01 <- "https://www.mobile01.com/forumtopic.php?c=30"
Url <- read_html(url)##save the html data
###
time <- read_html(url) %>% 
  html_nodes("time") %>% 
  html_text() %>% 
  iconv(from = "UTF-8", to = "UTF-8")

m01title <- Url %>% 
  html_nodes("h1") %>% 
  html_text() %>% 
  iconv(from = "UTF-8", to = "UTF-8")

category <- Url %>% 
  html_nodes("h2") %>% 
  html_text() %>% 
  iconv(from = "UTF-8", to = "UTF-8")

domain <- url
hred <- Url %>% 
  html_attr('href') %>% 
  iconv(from = "UTF-8", to = "UTF-8")
  
Url <- gsub(domain, "", Url)
Url <- paste0(domain, Url)
mobile01 <- data.frame(title = m01title, category = category, url = Url)


##suppliment
html_attr(x, "href")   #retrieve the attributes of the webpages
html_text(x, "UTF-8") # can encode the text to provent”garbled message"
##start to scrap hypelink

