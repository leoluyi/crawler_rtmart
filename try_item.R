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


get_res <- function(page_link) {
  res <- tryCatch({
    GET(page_link,
        user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/45.0.2454.101 Safari/537.36")
    )},
    error = function(w) {cat("FAIL!", page_link, "\n"); return(NULL)}
  )
  if (is.null(res)) return(NULL)

  node <- content(res, encoding = "UTF-8")
  # node["//h5[@class='for_proname']/a"]
}


## get item links
get_item_link <- function(link, p_data_num = 999) {
  # link <- link_1[1]
  if (length(link) != 1) return(NULL)
  page_link <- paste0(link, sprintf("&p_data_num=%s", as.character(p_data_num)))

  node <- get_res(page_link)
  item_link <- unname(xpathSApply(node, "//h5[@class='for_proname']/a", xmlAttrs))

  ## pagination
  # node["//li[@class='list_num']/a"]
  if (length(node["//li[@class='list_num']/a"]) != 0) {
    pagination_links <- sprintf(
      "http://www.rt-mart.com.tw/direct/%s",
      unique(unname(xpathSApply(
        node, "//li[@class='list_num']/a", xmlAttrs
      ))), collapse="")

    for (i in seq_along(pagination_links)) {
      node_temp <- get_res(pagination_links[i])
      temp <- unname(xpathSApply(node_temp, "//h5[@class='for_proname']/a", xmlAttrs))
      item_link <- c(item_link, temp)
    }
  }
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
    error = function(w) {cat("FAIL!", link, "\n"); return(NULL)}
  )
  if (is.null(res)) return(NULL)

  node <- content(res, encoding = "UTF-8")

  misc_text <- xmlValue(node["//td[@valign='top' and @style='line-height:20px;']"][[1]])
  # as(node, "character") %>% saveHTML()
  result <- list(
    item_name = xmlValue(node["//span[@id='prod_title']"][[1]]),
    pic = unname(unclass(node["//img[@id='item_img']/@src"][[1]])),
    class_1 = tryCatch(xmlValue(node["//div[@class='siteMargin']/ul/li[3]/a"][[1]]),
                       error = function(w) {cat("('class_1' not exist)", link, "\n"); return(NA)}
    ),
    class_2 = tryCatch(xmlValue(node["//div[@class='siteMargin']/ul/li[5]/a"][[1]]),
                       error = function(w) {cat("('class_2' not exist)", link, "\n"); return(NA)}
    ),
    class_3 = tryCatch(xmlValue(node["//div[@class='siteMargin']/ul/li[7]/a"][[1]]),
                       error = function(w) {cat("('class_3' not exist)", link, "\n"); return(NA)}
    ),
    price = stringr::str_extract(xmlValue(node["//span[@class='price_num']"][[1]]),
                                 "[0-9]+"),
    spec = stringr::str_match(misc_text, "\u898f\u683c:(.*)\r\n")[,2],
    made_in = stringr::str_match(misc_text, "\u7522\u5730:(.*)\r\n")[,2],
    unit = stringr::str_match(misc_text, "\u55ae\u4f4d:(.*)$")[,2],
    url = link
  )

  result
}


# run ---------------------------------------------------------------------

item_links <- unlist(sapply(link_1 , get_item_link))
# save(item_links, file="temp/item_links.RData")
# load("temp/item_links.RData")

data_result_list_1 <- lapply(item_links[1:2000], get_item_data)
# save(data_result_list_1, file="temp/data_result_list_1.RData")
# load("temp/data_result_list_1.RData")
data_result_list_2 <- lapply(item_links[2001:4000], get_item_data)
# save(data_result_list_2, file="temp/data_result_list_2.RData")
# load("temp/data_result_list_2.RData")
data_result_list_3 <- lapply(item_links[4001:6000], get_item_data)
# save(data_result_list_3, file="temp/data_result_list_3.RData")
# load("temp/data_result_list_3.RData")
data_result_list_4 <- lapply(item_links[6001:length(item_links)], get_item_data)
# save(data_result_list_4, file="temp/data_result_list_4.RData")
# load("temp/data_result_list_4.RData")

data_result <- lapply(list(data_result_list_1,
                           data_result_list_2,
                           data_result_list_3,
                           data_result_list_4),
                      dplyr::bind_rows
) %>% bind_rows

# parse -------------------------------------------------------------------

remove_unit <- function(x, spec) {
  #   x <- "雪芙蘭草本修護洗髮乳- 滋養修護"
  #   spec <- "650g/滋養修護"
  if (length(spec)!=1 || is.na(spec)) return(x)

  trim <- function (x) gsub("^\\s+|[ /-]+$", "", x)

  ## clear spec
  spec <- gsub("[#!$@~\u25ce]+|^[*]", "", spec)
  spec <- gsub("\\*", "\\\\*", spec)
  spec <- trim(spec)

  match_string <- strsplit(spec, "/")[[1]]
  pattern_2 <- paste0(match_string, collapse = "|")

  ## clear x
  x <- gsub("[#!,\uff0f\u25c6]|^[*]|\u2605+[^\u2605]*\u2605+|\u3010[^\u3011]+\u3011", "", x)

  try({
    if(!grepl(paste0("^",spec), x)) {
      x <- gsub(paste0(spec, "$"), "", x)
      x <- gsub(pattern_2, "", x)
    }
  }, silent = TRUE)

  x <- gsub("/.*$", "", x)
  x <- trim(x)
  x
}

clear_spec <- function(x) {
  x <- gsub("[#!$@~\u25ce]+|^[*]", "", x)
  x <- gsub("\\*", "\\\\*", x)
  x <-  gsub("^\\s+|[ /-]+$", "", x)
  x
}
data_result_new <- data_result %>%
  mutate(item_name = mapply(remove_unit, item_name, spec)) %>%
  mutate(spec = clear_spec(spec))

# save(data_result_new, file="temp/data_result_new.RData")


readr::write_csv(data_result_new, "result/data_result.csv")

# hierarchical structure --------------------------------------------------

# load("temp/data_result_new.RData")
struc <- data_result_new %>%
  dplyr::select(class_1, class_2, class_3) %>%
  table() %>%
  as.data.frame.table(., stringsAsFactors = FALSE) %>%
  tbl_df %>%
  filter(Freq!=0) %>%
  dplyr::select(-Freq) %>%
  arrange(class_1, class_2, class_3)

write_tsv(struc, "result/ftab.txt")
