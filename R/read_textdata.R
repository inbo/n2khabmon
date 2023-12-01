#' Return the 'namelist' data source as a tibble
#'
#' Returns the included data source \code{\link{namelist}} as a
#' \code{\link[tibble:tbl_df-class]{tibble}},
#' by default filtered according to English names and shortnames.
#'
#' \code{\link{namelist}} is a data source in the
#' \href{https://ropensci.github.io/git2rdata}{vc-format} which provides
#' names and (optionally) shortnames for IDs/codes used in other data sources.
#'
#' \code{read_namelistmon()} reads it and returns it as a
#' \code{\link[tibble:tbl_df-class]{tibble}}.
#' A tibble is a data frame that makes working in the tidyverse a little
#' \href{https://r4ds.had.co.nz/tibbles.html}{easier}.
#' By default, the data version delivered with the package is used and only English
#' names (\code{lang = "en"}) are returned.
#'
#' @param path Location of the data source.
#' The default is to use the location of the data source as delivered by
#' the installed package.
#' @param file The filename of the data source, without extension.
#' The default is to use the file delivered by the installed package.
#' @param lang An
#'   \href{https://www.w3.org/International/articles/language-tags/index.en}{IETF BCP
#'   47 language tag}, such as \code{"en"} or \code{"nl"}, to specify
#'   the language of \code{name} and \code{shortname} to be returned in the tibble.
#'   If \code{lang = "all"}, the full \code{namelist} tibble is returned, i.e.
#'   containing all languages.
#'
#' @return
#' The \code{namelist} data frame as a \code{\link[tibble:tbl_df-class]{tibble}},
#' filtered according to the \code{lang} argument.
#' See \code{\link{namelist}} for documentation of the tibble's contents.
#'
#' @section Recommended usage:
#'
#'   \code{read_namelistmon()}
#'
#'   \code{read_namelistmon(lang = "nl")}
#'
#' @seealso
#' \code{\link{namelist}}
#' \code{\link[n2khab:read_namelist]{n2khab::read_namelist}}
#'
#' @family reading functions for n2khabmon-referencelists
#'
#' @examples
#' read_namelistmon()
#' read_namelistmon(lang = "nl")
#'
#' @export
#' @importFrom git2rdata read_vc
#' @importFrom assertthat
#' assert_that
#' is.string
#' @importFrom dplyr %>% filter as_tibble
read_namelistmon <-
  function(path = pkgdatasource_path("textdata/namelist", ".yml"),
           file = "namelist",
           lang = "en") {
    assert_that(is.string(lang))

    if (lang == "all") {
      result <-
        read_vc(file = file, root = path)
    } else {
      result <-
        read_vc(file = file, root = path) %>%
        filter(lang == !!lang)
    }

    attr(result, "source") <- NULL

    result %>%
      as_tibble()
  }







#' Return the path of a package data source
#'
#' Returns the path for a given file
#' basename in a package. The developer can optionally
#' provide the file's extension separately.
#'
#' @param file The filename of the data source. Can be without extension.
#' Can be a character vector.
#' @param extension The extension of a file if not provided by \code{file}.
#' The dot is to be included, e.g. \code{.csv}.
#'
#' @return A character vector.
#' @importFrom dplyr %>% filter
#' @importFrom stringr str_c
#' @keywords internal
pkgdatasource_path <-
  function(file, extension = "") {
    system.file(str_c(file, extension),
      package = "n2khabmon"
    ) %>%
      dirname()
  }






#' Return the 'schemes' data source as a tibble with names & shortnames
#'
#' Returns the included data source \code{\link{schemes}} as a
#' \code{\link[tibble:tbl_df-class]{tibble}}.
#' Names and shortnames from \code{\link{namelist}} are added,
#' in English by default.
#'
#' \code{\link{schemes}} is a data source in the
#' \href{https://ropensci.github.io/git2rdata}{vc-format} which provides
#' a list of (monitoring) schemes for N2KHAB monitoring programmes or
#' other N2KHAB projects, together
#' with defining attributes and optional information.
#' A 'scheme' refers to a monitoring or research setup that determines
#' which types (habitat/RIBs) are to be investigated for a question or for
#' a bunch of related questions.
#'
#' \code{read_schemes()} reads the \code{\link{schemes}} data source, adds
#' names + shortnames and returns it as a
#' \code{\link[tibble:tbl_df-class]{tibble}}.
#' A tibble is a data frame that makes working in the tidyverse a little
#' \href{https://r4ds.had.co.nz/tibbles.html}{easier}.
#' By default, the data version delivered with the package is used and English
#' names (\code{lang = "en"}) are returned for scheme, programme, attributes
#' and tags.
#'
#' @param path Location of the data sources \code{schemes} and \code{namelist}.
#' The default is to use the location of the data sources as delivered by
#' the installed package.
#' @param file The filename of the \code{schemes} data source, without extension.
#' The default is to use the file delivered by the installed package.
#' @param file_namelist The filename of the \code{namelist} data source,
#' without extension.
#' The default is to use the file delivered by the installed package.
#' @param lang An
#'   \href{https://www.w3.org/International/articles/language-tags/index.en}{
#'   IETF BCP 47 language tag}, such as \code{"en"} or \code{"nl"}, to specify
#'   the language of names & shortnames to be returned in the tibble.
#'
#' @return
#' The \code{schemes} data frame as a \code{\link[tibble:tbl_df-class]{tibble}},
#' with names & shortnames added for scheme, programme, attributes and tags
#' according to the \code{lang} argument.
#' The tibble has 25 variables.
#' See \code{\link{schemes}} for documentation of the data-source's contents.
#' See \code{\link{namelist}} for the link between codes or other identifiers
#' and the corresponding names (and shortnames).
#'
#' The added names and shortnames are represented by the following variables:
#' \itemize{
#'   \item \code{scheme_name}
#'   \item \code{scheme_shortname}
#'   \item \code{programme_name}
#'   \item \code{attribute_1_name}
#'   \item \code{attribute_1_shortname}
#'   \item \code{attribute_2_name}
#'   \item \code{attribute_2_shortname}
#'   \item \code{attribute_3_name}
#'   \item \code{attribute_3_shortname}
#'   \item \code{tag_1_name}
#'   \item \code{tag_1_shortname}
#'   \item \code{tag_2_name}
#'   \item \code{tag_2_shortname}
#'   \item \code{tag_3_name}
#'   \item \code{tag_3_shortname}
#' }
#'
#' The added names and shortnames for scheme, programme and attributes are
#' \emph{factors} with their level order according to that of the
#' scheme, programme or attribute variable.
#'
#' @section Recommended usage:
#'
#'   \code{read_schemes()}
#'
#'   \code{read_schemes(lang = "nl")}
#'
#' @seealso
#' \code{\link{schemes}}
#'
#' @family reading functions for n2khabmon-referencelists
#'
#' @examples
#' read_schemes()
#' read_schemes(lang = "nl")
#'
#' @export
#' @importFrom git2rdata read_vc
#' @importFrom assertthat
#' assert_that
#' is.string
#' @importFrom dplyr
#' %>%
#' filter
#' select
#' distinct
#' mutate
#' left_join
#' as_tibble
#' contains
#' pull
#' rename
#' @importFrom rlang .data
read_schemes <-
  function(path = pkgdatasource_path("textdata/schemes", ".yml"),
           file = "schemes",
           file_namelist = "namelist",
           lang = "en") {
    assert_that(is.string(lang))

    langs <-
      read_namelistmon(
        path = path,
        file = file_namelist,
        lang = "all"
      ) %>%
      distinct(.data$lang) %>%
      pull(lang)

    assert_that(any(lang %in% langs),
      msg = "Your setting of lang is not supported."
    )

    namelist <-
      read_namelistmon(
        path = path,
        file = file_namelist,
        lang = lang
      ) %>%
      select(
        .data$code,
        .data$name,
        .data$shortname
      ) |>
      rbind(
        n2khab::read_namelist(lang = lang) %>%
          select(
            .data$code,
            .data$name,
            .data$shortname
          )
      )

    suppressWarnings(
      read_vc(file = file, root = path) %>%
        mutate(
          scheme_name = n2khab:::namelist_factor(.data$scheme,
            codelist = namelist
          ),
          scheme_shortname = n2khab:::namelist_factor(.data$scheme,
            "shortname",
            codelist = namelist
          ),
          programme_name = n2khab:::namelist_factor(.data$programme,
            codelist = namelist
          ),
          attribute_1_name = n2khab:::namelist_factor(.data$attribute_1,
            codelist = namelist
          ),
          attribute_1_shortname = n2khab:::namelist_factor(.data$attribute_1,
            "shortname",
            codelist = namelist
          ),
          attribute_2_name = n2khab:::namelist_factor(.data$attribute_2,
            codelist = namelist
          ),
          attribute_2_shortname = n2khab:::namelist_factor(.data$attribute_2,
            "shortname",
            codelist = namelist
          ),
          attribute_3_name = n2khab:::namelist_factor(.data$attribute_3,
            codelist = namelist
          ),
          attribute_3_shortname = n2khab:::namelist_factor(.data$attribute_3,
            "shortname",
            codelist = namelist
          )
        ) %>%
        left_join(namelist,
          by = c("tag_1" = "code")
        ) %>%
        rename(
          tag_1_name = .data$name,
          tag_1_shortname = .data$shortname
        ) %>%
        left_join(namelist,
          by = c("tag_2" = "code")
        ) %>%
        rename(
          tag_2_name = .data$name,
          tag_2_shortname = .data$shortname
        ) %>%
        left_join(namelist,
          by = c("tag_3" = "code")
        ) %>%
        rename(
          tag_3_name = .data$name,
          tag_3_shortname = .data$shortname
        ) %>%
        select(
          contains("scheme"),
          contains("programme"),
          contains("attribute"),
          .data$spatial_restriction,
          .data$notes,
          contains("tag")
        ) %>%
        as_tibble()
    )
  }




















#' Return the 'scheme_types' data source as a tibble
#'
#' Returns the included data source \code{\link{scheme_types}} as a
#' \code{\link[tibble:tbl_df-class]{tibble}}.
#' Names and shortnames from \code{\link{namelist}} are optionally added,
#' in English by default.
#'
#' \code{\link{scheme_types}} is a data source in the
#' \href{https://ropensci.github.io/git2rdata}{vc-format} which lists
#' the types (using the type-code from \code{\link{types}}) that belong to
#' each N2KHAB (monitoring or research) scheme (using the scheme-code from
#' \code{\link{schemes}}).
#' It also defines typegroup memberships of the types within specific schemes,
#' if applicable.
#'
#' \code{read_scheme_types()} reads the \code{\link{scheme_types}} data
#' source, optionally adds names + shortnames (always done for the typegroup)
#' and returns it as a
#' \code{\link[tibble:tbl_df-class]{tibble}}.
#' A tibble is a data frame that makes working in the tidyverse a little
#' \href{https://r4ds.had.co.nz/tibbles.html}{easier}.
#' By default, the data version delivered with the package is used and English
#' names (\code{lang = "en"}) are returned.
#'
#' @param path Location of the data sources \code{scheme_types},
#' \code{schemes}, \code{types} and \code{namelist}.
#' The default is to use the location of the data sources as delivered by
#' the installed package.
#' @param file The filename of the \code{scheme_types} data source, without extension.
#' The default is to use the file delivered by the installed package.
#'
#' @inheritParams read_schemes
#'
#' @param extended Should names & shortnames be added for scheme, programme,
#' scheme attributes, type, typeclass and tags of scheme and type?
#'
#' @return
#' The \code{scheme_types} data frame as a \code{\link[tibble:tbl_df-class]{tibble}},
#' with names & shortnames added for the typegroup variable and optionally for
#' scheme, programme,
#' scheme attributes, type and attributes & tags of scheme and type, all
#' according to the \code{lang} argument.
#' The tibble has either 5 or many variables, depending on the \code{extended}
#' argument.
#' See \code{\link{scheme_types}} for documentation of the data-source's contents.
#' See \code{\link{namelist}} for the link between codes or other identifiers
#' and the corresponding names (and shortnames).
#'
#' The \emph{optionally} added names and shortnames are represented by the
#' following variables:
#' \itemize{
#'   \item \code{scheme_name}
#'   \item \code{scheme_shortname}
#'   \item \code{programme_name}
#'   \item \code{attribute_1_name}
#'   \item \code{attribute_1_shortname}
#'   \item \code{attribute_2_name}
#'   \item \code{attribute_2_shortname}
#'   \item \code{attribute_3_name}
#'   \item \code{attribute_3_shortname}
#'   \item \code{schemetag_1_name}
#'   \item \code{schemetag_1_shortname}
#'   \item \code{schemetag_2_name}
#'   \item \code{schemetag_2_shortname}
#'   \item \code{schemetag_3_name}
#'   \item \code{schemetag_3_shortname}
#'   \item \code{type_name}
#'   \item \code{type_shortname}
#'   \item \code{typeclass_name}
#'   \item \code{hydr_class_name}
#'   \item \code{hydr_class_shortname}
#'   \item \code{groundw_dep_name}
#'   \item \code{groundw_dep_shortname}
#'   \item \code{flood_dep_name}
#'   \item \code{flood_dep_shortname}
#'   \item \code{typetag_1_name}
#'   \item \code{typetag_1_shortname}
#'   \item \code{typetag_2_name}
#'   \item \code{typetag_2_shortname}
#'   \item \code{typetag_3_name}
#'   \item \code{typetag_3_shortname}
#' }
#'
#' The added names and shortnames for scheme, programme,
#' attributes and typegroup are \emph{factors} with their level order according
#' to that of the scheme, programme, attribute or typegroup variable.
#'
#' @section Recommended usage:
#'
#'   \code{read_scheme_types()}
#'
#'   \code{read_scheme_types(lang = "nl")}
#'
#' @seealso
#' \code{\link{scheme_types}}
#'
#' @family reading functions for n2khabmon-referencelists
#'
#' @examples
#' read_scheme_types()
#' read_scheme_types(lang = "nl")
#'
#' @export
#' @importFrom git2rdata read_vc
#' @importFrom assertthat
#' assert_that
#' is.string
#' @importFrom dplyr
#' %>%
#' select
#' mutate
#' left_join
#' as_tibble
#' contains
#' pull
#' distinct
#' @importFrom tidyr gather spread
#' @importFrom stringr str_c
#' @importFrom rlang .data
read_scheme_types <- function(path = pkgdatasource_path("textdata/scheme_types", ".yml"),
                              file = "scheme_types",
                              file_namelist = "namelist",
                              lang = "en",
                              extended = FALSE) {
  assert_that(is.string(lang))

  langs <-
    read_namelistmon(
      path = path,
      file = file_namelist,
      lang = "all"
    ) %>%
    distinct(.data$lang) %>%
    pull(lang)

  assert_that(any(lang %in% langs),
    msg = "Your setting of lang is not supported."
  )

  namelist <-
    read_namelistmon(
      path = path,
      file = file_namelist,
      lang = lang
    ) %>%
    select(
      .data$code,
      .data$name,
      .data$shortname
    ) |>
    rbind(
      n2khab::read_namelist(lang = lang) %>%
        select(
          .data$code,
          .data$name,
          .data$shortname
        )
    )

  scheme_types <- read_vc(file = file, root = path)

  if (extended) {
    schemes <-
      read_schemes(
        path = path,
        file = "schemes",
        file_namelist = file_namelist,
        lang = lang
      ) %>%
      gather(
        key = "key",
        value = "value",
        contains("tag")
      ) %>%
      mutate(key = str_c("scheme", .data$key)) %>%
      spread(key = .data$key, value = .data$value)

    types <-
      n2khab::read_types(
        path = path,
        file = "types",
        file_namelist = file_namelist,
        lang = lang
      ) %>%
      gather(
        key = "key",
        value = "value",
        contains("tag")
      ) %>%
      mutate(key = str_c("type", .data$key)) %>%
      spread(key = .data$key, value = .data$value)

    scheme_types %>%
      mutate(
        typegroup_name = n2khab:::namelist_factor(.data$typegroup,
          codelist = namelist
        ),
        typegroup_shortname = n2khab:::namelist_factor(.data$typegroup,
          "shortname",
          codelist = namelist
        )
      ) %>%
      left_join(schemes,
        by = "scheme"
      ) %>%
      left_join(types,
        by = "type"
      ) %>%
      mutate(type = .data$type %>%
        factor(
          levels =
            read_vc(file = file, root = path) %>%
              pull(.data$type) %>%
              levels()
        )) %>%
      as_tibble()
  } else {
    scheme_types %>%
      mutate(
        typegroup_name = n2khab:::namelist_factor(.data$typegroup,
          codelist = namelist
        ),
        typegroup_shortname = n2khab:::namelist_factor(.data$typegroup,
          "shortname",
          codelist = namelist
        )
      ) %>%
      as_tibble()
  }
}
