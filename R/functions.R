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

make_rmd_chunks <- function(sites, image_width = NULL) {
    chunks <- list()
    for (i in seq_len(nrow(sites))) {
        site <- sites[i,]
        if (is.null(image_width)) {
            chunks[[i]] <- make_readme_chunk(site)
        } else {
            chunks[[i]] <- make_showcase_chunk(site, image_width)  
        }
    }
    return(save_temp_chunks(chunks))
}

make_readme_chunk <- function(site) {
    chunk <- paste0(
        '- ', site$name, ': [site](', site$url, 
        ') | [source](', site$source, ')'
    )
    return(chunk)
}

make_showcase_chunk <- function(site, image_width = 600) {
    chunk <- paste0(
        '### ', site$name, '\n\n',
        '<center>\n',
        '<img src="', site$path_png, '" width=', image_width, '>\n',
        '</center>\n\n',
        '<aside>',
        link_button(
          icon = "fas fa-external-link-alt", text = "Site", url = site$url),
        '<br>',
        link_button(
          icon = "fab fa-github", text = "Source", url = site$url), 
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
        save_chunk(x[[i]], path)
    }
    return(unlist(paths))
}

save_chunk <- function(chunk, path) {
    fileConn <- file(path)
    writeLines(chunk, fileConn)
    close(fileConn)
}

link_button <- function(
  icon = NULL,
  text = NULL,
  url = NULL
) {
  if (!is.null(icon)) {
    text <- htmltools::HTML(paste0('<i class="', icon, '"></i> ', text))
  }
  return(htmltools::a(href = url, text, class = "link-button"))
}

create_footer <- function() {

  footer <- htmltools::HTML(paste0(
    '<i class="fas fa-wrench"></i> Made with <i class="far fa-heart"></i>, <a href="https://github.com/jhelvy/distillery"><i class="fas fa-code-branch"></i></a>, and the <a href="https://cran.r-project.org/"><i class="fab fa-r-project"></i></a><a href="https://github.com/rstudio/distill"> distill</a> package.\n\n',
    '<span style="font-size:0.8rem;">Last updated on ',
    format(Sys.Date(), format="%B %d, %Y"), '</span>\n\n',
  
    '<!-- Add function to open links to external links in new tab, from: -->\n',
    '<!-- https://yihui.name/en/2018/09/target-blank/ -->\n\n',
  
    '<script src="js/external-link.js"></script>'
  ))

  fileConn <- file("_footer.html")
  writeLines(footer, fileConn)
  close(fileConn)

}