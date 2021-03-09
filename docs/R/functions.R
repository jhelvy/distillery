clean_sites <- function(sites) {
    sites <- sites %>% 
        mutate(
            name_clean = clean_name(name), 
            path_png = file.path("images", "sites", paste0(name_clean, ".png")),
            path_rmd = file.path("rmd_chunks", paste0(name_clean, ".Rmd"))
        )
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
            screenshot <- webshot(
                site$url,
                vwidth = 1200, 
                vheight = floor(1200*0.65), 
                cliprect = "viewport", 
                file = site$path_png)
        }
    }
}

update_child_docs <- function(sites) {
    for (i in seq_len(nrow(sites))) {
        site <- sites[i,]
        if (!file.exists(site$path_rmd)) {
            make_child_doc(site)
        }
    }
}

make_child_doc <- function(site) {
    
    doc <- paste0(
'### ', site$name, '

[site](', site$url, ') | [source](', site$source, ')

<center>
<img src="', site$path_png, '" width=600>
</center>
'
    )
    
    fileConn <- file(site$path_rmd)
    writeLines(doc, fileConn)
    close(fileConn)
}
