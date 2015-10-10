library(httr)
library(XML)
library(dplyr)
library(stringr)
library(readr)
source("utils/saveHTML.r") # saveHTML()


# try ---------------------------------------------------------------------

## GET layer 1
url_sitemap <- "http://www.rt-mart.com.tw/direct/"
res_sitemap <- GET(url_sitemap,
                   user_agent("Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/45.0.2454.85 Safari/537.36")
)

node_1 <- content(res_sitemap, encoding = "UTF-8")
name_1 <- xpathSApply(node_1, "//ul[@class='main_nav']/li/a", xmlValue)
link_1 <- paste0("http://www.rt-mart.com.tw/direct/",
                 unname(xpathSApply(node_1, "//ul[@class='main_nav']/li/a", xmlAttrs)))
link_1 <- gsub("http://www.rt-mart.com.tw/direct/http://www.rt-mart.com.tw/direct/",
               "http://www.rt-mart.com.tw/direct/",
               link_1)
# link_1 <- setNames(link_1, name_1)

# get items ---------------------------------------------------------------

## get item links
get_item_link <- function(link, p_data_num = 10000) {
  # link <- link_1[1]
  if (length(link)!=1) return(NULL)
  page_link <- paste0(link, sprintf("&p_data_num=%s", as.character(p_data_num)))
  res <- tryCatch({
    GET(page_link,
        user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/45.0.2454.101 Safari/537.36")
    )},
    warning = function(w) {cat("FAIL!", page_link, "\n"); return(NULL)}
  )
  if (is.null(res)) return(NULL)

  node <- content(res, encoding = "UTF-8")
  # node["//h5[@class='for_proname']/a"]
  unname(xpathSApply(node, "//h5[@class='for_proname']/a", xmlAttrs))
}

# item_links <- unlist(sapply(link_1[1:2] , get_item_link, USE.NAMES = FALSE))

## get item data
get_item_data <- function(link) {
  # link <- item_links[1]
  if (length(link)!=1) return(NULL)
  res <- tryCatch({
    GET(link,
        user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/45.0.2454.101 Safari/537.36")
    )},
    warning = function(w) {cat("FAIL!", link, "\n"); return(NULL)}
  )
  if (is.null(res)) return(NULL)

  node <- content(res, encoding = "UTF-8")

  misc_text <- xmlValue(node["//td[@valign='top' and @style='line-height:20px;']"][[1]])
  # as(node, "character") %>% saveHTML()
  result <- list(
    item_name = xmlValue(node["//span[@id='prod_title']"][[1]]),
    pic = unname(unclass(node["//img[@id='item_img']/@src"][[1]])),
    class_1 = xmlValue(node["//div[@class='siteMargin']/ul/li[3]/a"][[1]]),
    class_2 = xmlValue(node["//div[@class='siteMargin']/ul/li[5]/a"][[1]]),
    class_3 = xmlValue(node["//div[@class='siteMargin']/ul/li[7]/a"][[1]]),
    price = stringr::str_extract(xmlValue(node["//span[@class='price_num']"][[1]]),
                                 "[0-9]+"),
    spec = stringr::str_match(misc_text, "規格:(.*)\r\n")[,2],
    made_in = stringr::str_match(misc_text, "產地:(.*)\r\n")[,2],
    unit = stringr::str_match(misc_text, "單位:(.*)$")[,2]
  )

  result
}


# run ---------------------------------------------------------------------

item_links <- unlist(sapply(link_1 , get_item_link))
data_result_list <- lapply(item_links, get_item_data)
data_result <- dplyr::bind_rows(data_result_list)
readr::write_csv(data_result, "result/data_result.csv")
