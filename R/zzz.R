#' Print package startup message
#'
#' The function is called when the package is loaded through library or require.
#'
#' @param libname Library name
#' @param pkgname Package name
#'
#' @return Nothing
.onAttach <- function(libname, pkgname) {
  installed_version <- utils::packageDescription("spdesign", fields = "Version")

  description <- tryCatch({
    readLines(
      "https://raw.githubusercontent.com/edsandorf/spdesign/master/DESCRIPTION"
    )
  }, warning = function(w) {
    return("NA")
  }, error = function(e) {
    return("NA")
  })

  if (length(description) == 1) {
    remote_version <- description
  } else {
    remote_version <- gsub(
      "Version:\\s*",
      "",
      description[grep("Version:", description)]
    )
  }

  pad_width <- 33

  packageStartupMessage(
    boxx(
      c(
        col_green(str_pad("THANK YOU FOR USING SPDESIGN!", 60, "left", " ")),
        "",
        paste0(
          col_green(
            str_pad(
              "You are currently using version: ", pad_width, "right", " "
            )
          ),
          col_white(installed_version)
        ),
        paste0(
          col_green(
            str_pad(
              "The latest stable version is: ",
              pad_width,
              "right",
              " "
            )
          ),
          col_white(remote_version)
        ),
        paste0(
          col_green(
            str_pad(
              "Package website with documentation and examples: ",
              pad_width,
              "right",
              " ")
          ),
          col_white("https://spdesign.edsandorf.me")
        ),
        paste0(
          col_green(
            str_pad(
              "To cite the package: ",
              pad_width,
              "right",
              " ")
          ),
          col_white("utils::citation('spdesign')")
        )
      ),
      padding = 1,
      float = "center",
      border_col = col_silver
    )
  )
}
