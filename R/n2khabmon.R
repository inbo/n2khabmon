#' @details
#' See the \href{00Index.html}{Index}
#' for documentation on functions and datasets delivered with the package.
#'
#' For contributing, see the README on
#' \href{https://github.com/inbo/n2khabmon/blob/master/README.md}{GitHub}.
#'
#' @keywords internal
"_PACKAGE"

utils::globalVariables(c("."))

#' @importFrom utils packageVersion packageDescription
#' @importFrom curl nslookup
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Attaching n2khabmon ",
    packageVersion("n2khabmon"),
    "."
  )
  if (!is.null(nslookup("api.github.com", error = FALSE))) {
    tryCatch(
      {
        ref <- remotes::github_remote(
          "inbo/n2khabmon",
          ref = remotes::github_release()
        )$ref
        release <- package_version(gsub("\\p{L}*", "", ref, perl = TRUE))
        if (packageVersion("n2khabmon") < release) {
          packageStartupMessage(
            "\n",
            rep("=", getOption("width")),
            "\nIt is advised to upgrade n2khabmon to its current version ",
            release,
            ". Run:\n\n",
            'detach("package:n2khabmon", unload = TRUE)\n',
            'install.packages("n2khabmon", repos = c(inbo = "https://inbo.r-universe.dev",
                                        CRAN = "https://cloud.r-project.org"))',
            "\n",
            "library(n2khabmon)\n",
            rep("=", getOption("width"))
          )
        }
      },
      error = function(e) {}
    )
  }
}
