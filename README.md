
<!-- README.md is generated from README.Rmd. Please edit that file -->

# US.covid.dashboard

<!-- badges: start -->
<!-- badges: end -->

App available at <https://yash-kalebere.shinyapps.io/uscovid/>

## Description

The goal of US.covid.dashboard is to provide users an information tool
to track key metrics on the coronavirus pandemic using hospital data
published the US government. Currently, this information is published as
[Community Profile
Reports](https://beta.healthdata.gov/Health/COVID-19-Community-Profile-Report/gqxm-d9w9)
online. Although these reports are invaluable to decision makers, they
might experience “report fatigue” from reading large pdf documents. This
web application was created keeping the type of experience these end
users may want in mind.

## Running the Application

You can run this dashboard on your own computer if you have Docker
installed. It’s even easier if you have `make`. If you don’t, simply run
the targets listed in `makefile`

    git clone https://github.com/yashkal/US.covid.dashboard.git
    cd US.covid.dashboard

    make build    # Builds container and downloads latest datasets
    make run      # Run application 

If these steps are run successfully, you can see the resulting
application on <http://127.0.0.1/>

<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/master/examples>. -->
