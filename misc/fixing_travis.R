# Download pandoc 2.7.1 built with ghc-8.6.4, and instruct
# RStudio + rmarkdown to use it.

local({

  # The directory where Pandoc will be extracted. Feel free
  # to adjust this path as appropriate.
  dir <- "~/rstudio-pandoc"

  # The version of Pandoc to be installed.
  version <- "2.7.1"

  # Create and move to the requested directory.
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  owd <- setwd(dir)
  on.exit(setwd(owd), add = TRUE)

  # Construct path to pandoc.
  root <- "https://s3.amazonaws.com/rstudio-buildtools"
  suffix <- sprintf("pandoc-%s-windows-x86_64.zip", version)
  url <- file.path(root, "pandoc-rstudio", version, suffix)

  # Download and extract pandoc.
  file <- basename(url)
  utils::download.file(url, destfile = file)
  utils::unzip(file)
  unlink(file)

  # Write .Renviron to update the version of Pandoc used.
  entry <- paste("RSTUDIO_PANDOC", shQuote(path.expand(dir)), sep = " = ")
  contents <- if (file.exists("~/.Renviron")) readLines("~/.Renviron")
  filtered <- grep("^RSTUDIO_PANDOC", contents, value = TRUE, invert = TRUE)
  amended <- union(filtered, entry)
  writeLines(amended, "~/.Renviron")

  # Report change to the user.
  writeLines("Updated .Renviron:\n")
  writeLines(amended)
  writeLines("\nPlease restart RStudio for these changes to take effect.")

})
