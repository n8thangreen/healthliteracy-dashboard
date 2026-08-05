# Health Literacy Dashboard

R dashboard for the UCL Newham Fellowship health literacy project.

This repository contains the code and resources for an interactive R dashboard used to explore and present findings from the UCL Newham Fellowship health literacy study in Newham.

Published on RPubs / Posit Connect:
https://rpubs.com/n8than/healthliteracy

## Contents

- R scripts and R Markdown / flexdashboard files that make up the dashboard
- Supporting data (where included) and helper scripts
- Documentation and usage instructions

## Features

- Interactive visualisations of health literacy metrics
- Filters and breakdowns by demographic groups and survey items
- Exportable summary tables and plots

## Requirements

- R (>= 4.0)
- RStudio or Posit
- Recommended R packages: shiny, rmarkdown, flexdashboard, tidyverse, DT, plotly

Install commonly used packages with:

```r
install.packages(c("shiny", "rmarkdown", "flexdashboard", "tidyverse", "DT", "plotly"))
```

If the project uses renv, restore packages with:

```r
if (file.exists("renv.lock")) {
  install.packages("renv")
  renv::restore()
}
```

## Running the dashboard locally

Open this project in RStudio and run one of the following depending on the project layout:

- For an R Markdown / flexdashboard (e.g. `dashboard.Rmd`):

```r
rmarkdown::run("path/to/dashboard.Rmd")
```

- For a Shiny app with `app.R` or an app directory:

```r
shiny::runApp("path/to/app_directory_or_app.R")
```

Replace the paths above with the actual filenames in this repository.

## Data

If data files are included, be mindful of privacy and licensing; do not commit personally identifying information. If the full dataset cannot be published, include a small example dataset or instructions to obtain the data.

## Development & Contributing

Contributions, issues and feature requests are welcome. Suggested workflow:

1. Fork the repository
2. Create a branch: `git checkout -b my-feature`
3. Make changes and add tests where appropriate
4. Commit and push your branch
5. Open a pull request describing your changes

Please open an issue to discuss major changes before submitting large PRs.

## License

If no license is present, consider adding one (for example MIT for code or CC-BY for documentation). Add a `LICENSE` file to this repository with the selected terms.

## Contact

Author: n8thangreen
Published RPubs: https://rpubs.com/n8than/healthliteracy

If you want the README tailored further (exact run commands, package list taken from the project, screenshots, or a link to a live deployment), tell me and I'll update it to match the repo contents.
