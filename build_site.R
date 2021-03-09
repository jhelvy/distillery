source(file.path("R", "functions.R"))

# Fist build the footer to capture today's date
create_footer()

# Then render the site
rmarkdown::render_site(encoding = 'UTF-8')
