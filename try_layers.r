library(httr)
library(rvest)
library(dplyr)
source("utils/saveHTML.r") # saveHTML()


# try ---------------------------------------------------------------------

## GET layer 1
url_sitemap <- "http://www.rt-mart.com.tw/direct/"
res_sitemap <- GET(url_sitemap,
                   user_agent("Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/45.0.2454.85 Safari/537.36")
)

node_1 <- content(res_sitemap, encoding = "UTF-8")
name_1 <- node_1 %>% html_nodes("ul.main_nav li a") %>% html_text()
link_1 <- paste0("http://www.rt-mart.com.tw/direct/",
                 node_1 %>% html_nodes("ul.main_nav li a") %>% html_attr("href"))

# ## GET layer 2
# # node_1["//div[@class='list_box']/h4"]
# name_2 <- xpathSApply(node_1, "//div[@class='list_box']/h4/*", xmlValue)
# link_2 <- xpathSApply(node_1, "//div[@class='list_box']/h4/*", xmlAttrs) %>% unname()

## GET layer 2
layer2 <- GET(link_1[1])
node_2 <- content(layer2, encoding = "UTF-8")
# node_2["//div[@class='classify_leftBox']/h3"]
name_2 <- xpathSApply(node_2, "div.'classify_leftBox h3", xmlValue)
n_name_2 <- length(name_2)
link_2 <- unname(xpathSApply(node_2, "//div[@class='classify_leftBox']/h3/a", xmlAttrs))


## GET layer 3
layer3 <- GET(link_2[1])
node_3 <- content(layer3, encoding = "UTF-8")
# node_3["//div[@class='classify_leftBox'][1]/ul/li[1]"]
link_3 <- xpathSApply(node2, "//div[@class='classify_leftBox'][3]/ul/li[2]", xmlValue)

## GET items


