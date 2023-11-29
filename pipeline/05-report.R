#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Wrap this script in a `try` block so that the pipeline continues execution
# even if report generation fails
tryCatch(
  {
    # Load libraries and scripts
    suppressPackageStartupMessages({
      library(here)
      library(magrittr)
      library(quarto)
      library(yaml)
    })

    # Load the parameters file containing the run settings
    params <- read_yaml("params.yaml")


    #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # 2. Generate performance report -------------------------------------------
    #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    message("Generating performance report")

    here("reports", "performance.qmd") %>%
      quarto_render(
        execute_params = list(
          year = params$assessment$year
        )
      )
  },


  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 3. Error handling ----------------------------------------------------------
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  error = function(c) {
    # Print the error message
    print("Error in report generation:")
    print(conditionMessage(c))

    # Save an empty report so that this pipeline step produces the required
    # output
    print("Saving an empty report file in order to continue execution")
    sink(here("reports", "performance.html"))
    cat("Error in report generation:")
    cat()
    cat(conditionMessage(c))
    sink()
  }
)
