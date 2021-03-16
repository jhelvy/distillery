source(file.path("R", "functions.R"))

# First build the footer to capture today's date
create_footer()

# Then render the README.Rmd file
rmarkdown::render(
    input = "README.Rmd",
    output_file = "README.md",
    output_format = "github_document")

# Then render the site
rmarkdown::render_site(encoding = 'UTF-8')
