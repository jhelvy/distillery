library(tidyverse)
library(htmltools)

clean_sites <- function(sites, check_url = FALSE) {

    if (isTRUE(check_url)) {
      sites <- sites %>%
        mutate(
          url_exists = RCurl::url.exists(url)
        ) %>%
        filter(url_exists == TRUE) %>%
        select(-url_exists)
    }

    # Prepare for uniting categories
    categories <- names(sites)[4:length(sites)]
    for (i in categories) {
      sites[, i] <- ifelse(sites[, i] == 0, NA_character_, i)
    }

    sites <- sites %>%
      unite(4:last_col(), col = "categories", sep = " ", na.rm = T) %>%
      mutate(
        name_clean = clean_name(name),
        path_png = file.path("images", "sites", paste0(name_clean, ".png"))
      ) %>%
      arrange(name)

    return(sites)
}

buttons_filter <- function(sites) {
  categories <- unique(unlist(strsplit(sites$categories, " ")))
  cat_button <- tagList(lapply(categories, function(x) {
      tags$button(
        class="btn",
        onclick = paste0("filterSelection('", x, "')"),
        firstup(x)
      )
  }))

  final_html <- div(
    id = "myBtnContainer",
    tags$button(
      class="btn active",
      onclick="filterSelection('all')",
      "Show all"
    ),
    cat_button
  )

  x <- tempfile(fileext = ".Rmd")
  save_raw(as.character(final_html), x)
  return(x)
}

# Upper case for first letter
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

clean_name <- function(x) {
    x <- x %>%
        str_to_lower() %>%
        str_replace_all(" ", "_") %>%
        str_replace_all("\\//", "_") %>%
        str_replace_all("\\/", "_") %>%
        str_replace_all("\\.", "_") %>%
        str_replace_all("\\:", "")
    return(x)
}

update_screenshots <- function(sites, update_all = FALSE) {
  for (i in seq_len(nrow(sites))) {
    site <- sites[i,]
    if (update_all == TRUE) {
      webshot2::webshot(
        site$url,
        vwidth = 1200,
        vheight = floor(1200*0.65),
        cliprect = "viewport",
        file = site$path_png)
    } else {
      if (!file.exists(site$path_png)) {
        webshot2::webshot(
          site$url,
          vwidth = 1200,
          vheight = floor(1200*0.65),
          cliprect = "viewport",
          file = site$path_png)
      }
    }
  }
}

make_showcase_chunks <- function(sites, image_width = 600) {
  sites_div <- tagList(apply(sites, 1, function(x) {
    tagList(
      tags$div(
        class = paste0("filterDiv ", x[["categories"]]),
        tags$h3(x[["name"]]),
          tag(
            "center",
            list(
              tags$img(
                src = x[["path_png"]], width = image_width,
                style = "float: left; width: 80%;",
              )
            )
        ),
        tags$a(
          href = x[["url"]],
          class = "icon-link",
          tag(
            "i", list(class = "fas fa-external-link-alt",
                      style = "display: inline-block")
          ),
          "Site"
        ),
        br(),
        tags$a(
          href = x[["source"]],
          class = "icon-link",
          tag(
            "i", list(class = "fab fa-github",
                      style = "display: inline-block"),
            ),
          "Source"
        ),
        hr()
      )
    )
  }))

  final_html <- tagList(
    tags$div(
      class = "container",
      sites_div
    )
  )

  x <- tempfile(fileext = ".Rmd")
  save_raw(as.character(sites_div), x)
  return(x)

}

create_footer <- function() {

  fill <- '#ededeb'
  height <- '14px'

  footer <- HTML(paste0(
  'Made with ',
  fontawesome::fa('heart', fill = fill, height = height), ', [',
  fontawesome::fa('code-branch', fill = fill, height = height),
  '](https://github.com/jhelvy), and the [',
  fontawesome::fa('r-project', fill = fill, height = height),
  '](https://cran.r-project.org/) ',
  '[distill](https://github.com/rstudio/distill) package\n',
  br(),
  last_updated(), "\n\n",

  '<!-- Add function to open links to external links in new tab, from: -->',
  '<!-- https://yihui.name/en/2018/09/target-blank/ -->\n\n',
  '<script src="js/external-link.js"></script>'
  ))

  save_raw(footer, "_footer.html")
}

last_updated <- function() {
  return(span(
    paste0(
      'Last updated on ',
      format(Sys.Date(), format="%B %d, %Y")
    ),
    style = "font-size:0.8rem;")
  )
}

save_raw <- function(text, path) {
    fileConn <- file(path)
    writeLines(text, fileConn)
    close(fileConn)
}
