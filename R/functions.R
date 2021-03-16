library(tidyverse)

clean_sites <- function(sites) {
    sites <- sites %>%
        mutate(
            name_clean = clean_name(name),
            path_png = file.path("images", "sites", paste0(name_clean, ".png"))
        ) %>%
        arrange(name)
    return(sites)
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

update_screenshots <- function(sites) {
    for (i in seq_len(nrow(sites))) {
        site <- sites[i,]
        if (!file.exists(site$path_png)) {
            screenshot <- webshot2::webshot(
                site$url,
                vwidth = 1200,
                vheight = floor(1200*0.65),
                cliprect = "viewport",
                file = site$path_png)
        }
    }
}

make_rmd_chunks <- function(sites, image_width = 600) {
    chunks <- list()
    for (i in seq_len(nrow(sites))) {
        site <- sites[i,]
        chunks[[i]] <- make_showcase_chunk(site, image_width)
    }
    return(save_temp_chunks(chunks))
}

make_showcase_chunk <- function(site, image_width = 600) {
    chunk <- paste0(
        '### ', site$name, '\n\n',
        '<center>\n',
        '<img src="', site$path_png, '" width=', image_width, '>\n',
        '</center>\n\n',
        '<aside>',
        icon_link(
          icon = "fas fa-external-link-alt", text = "Site", url = site$url),
        '<br>',
        icon_link(
          icon = "fab fa-github", text = "Source", url = site$source),
        '</aside>'
    )
    return(chunk)
}

save_temp_chunks <- function(x) {
    temp_folder <- tempdir()
    paths <- list()
    for (i in seq_len(length(x))) {
        path <- file.path(temp_folder, paste0(i, ".Rmd"))
        paths[[i]] <- path
        save_raw(x[[i]], path)
    }
    return(unlist(paths))
}

icon_link <- function(
  icon = NULL,
  text = NULL,
  url = NULL
) {
  if (!is.null(icon)) {
    text <- htmltools::HTML(paste0('<i class="', icon, '"></i> ', text))
  }
  return(htmltools::a(href = url, text, class = "icon-link"))
}

create_footer <- function() {

  fill <- '#ededeb'
  height <- 14

  footer <- htmltools::HTML(paste0(
  fontawesome::fa('wrench', fill = fill, height = height), ' Made with ',
  fontawesome::fa('heart', fill = fill, height = height), ', [',
  fontawesome::fa('code-branch', fill = fill, height = height),
  '](https://github.com/jhelvy/distillery), and the [',
  fontawesome::fa('r-project', fill = fill, height = height),
  '](https://cran.r-project.org/) ',
  '[distill](https://github.com/rstudio/distill) package\n',
  htmltools::br(),
  '<span style="font-size:0.8rem;">Last updated ',
  'on ', format(Sys.Date(), format="%B %d, %Y"), '</span>\n\n',
  '<!-- Add function to open links to external links in new tab, from: -->',
  '<!-- https://yihui.name/en/2018/09/target-blank/ -->\n\n',
  '<script src="js/external-link.js"></script>'
  ))

  save_raw(footer, "_footer.html")
}

save_raw <- function(text, path) {
  fileConn <- file(path)
  writeLines(text, fileConn)
  close(fileConn)
}
