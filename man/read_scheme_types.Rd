% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_textdata.R
\name{read_scheme_types}
\alias{read_scheme_types}
\title{Return the 'scheme_types' data source as a tibble}
\usage{
read_scheme_types(
  path = pkgdatasource_path("textdata/scheme_types", ".yml"),
  file = "scheme_types",
  file_namelist = "namelist",
  lang = "en",
  extended = FALSE
)
}
\arguments{
\item{path}{Location of the data sources \code{scheme_types},
\code{schemes}, \code{types} and \code{namelist}.
The default is to use the location of the data sources as delivered by
the installed package.}

\item{file}{The filename of the \code{scheme_types} data source, without extension.
The default is to use the file delivered by the installed package.}

\item{file_namelist}{The filename of the \code{namelist} data source,
without extension.
The default is to use the file delivered by the installed package.}

\item{lang}{An
\href{https://www.w3.org/International/articles/language-tags/index.en}{
IETF BCP 47 language tag}, such as \code{"en"} or \code{"nl"}, to specify
the language of names & shortnames to be returned in the tibble.}

\item{extended}{Should names & shortnames be added for scheme, programme,
scheme attributes, type, typeclass and tags of scheme and type?}
}
\value{
The \code{scheme_types} data frame as a \code{\link[tibble:tbl_df-class]{tibble}},
with names & shortnames added for the typegroup variable and optionally for
scheme, programme,
scheme attributes, type and attributes & tags of scheme and type, all
according to the \code{lang} argument.
The tibble has either 5 or many variables, depending on the \code{extended}
argument.
See \code{\link{scheme_types}} for documentation of the data-source's contents.
See \code{\link{namelist}} for the link between codes or other identifiers
and the corresponding names (and shortnames).

The \emph{optionally} added names and shortnames are represented by the
following variables:
\itemize{
\item \code{scheme_name}
\item \code{scheme_shortname}
\item \code{programme_name}
\item \code{attribute_1_name}
\item \code{attribute_1_shortname}
\item \code{attribute_2_name}
\item \code{attribute_2_shortname}
\item \code{attribute_3_name}
\item \code{attribute_3_shortname}
\item \code{schemetag_1_name}
\item \code{schemetag_1_shortname}
\item \code{schemetag_2_name}
\item \code{schemetag_2_shortname}
\item \code{schemetag_3_name}
\item \code{schemetag_3_shortname}
\item \code{type_name}
\item \code{type_shortname}
\item \code{typeclass_name}
\item \code{hydr_class_name}
\item \code{hydr_class_shortname}
\item \code{groundw_dep_name}
\item \code{groundw_dep_shortname}
\item \code{flood_dep_name}
\item \code{flood_dep_shortname}
\item \code{typetag_1_name}
\item \code{typetag_1_shortname}
\item \code{typetag_2_name}
\item \code{typetag_2_shortname}
\item \code{typetag_3_name}
\item \code{typetag_3_shortname}
}

The added names and shortnames for scheme, programme,
attributes and typegroup are \emph{factors} with their level order according
to that of the scheme, programme, attribute or typegroup variable.
}
\description{
Returns the included data source \code{\link{scheme_types}} as a
\code{\link[tibble:tbl_df-class]{tibble}}.
Names and shortnames from \code{\link{namelist}} are optionally added,
in English by default.
}
\details{
\code{\link{scheme_types}} is a data source in the
\href{https://ropensci.github.io/git2rdata}{vc-format} which lists
the types (using the type-code from \code{\link{types}}) that belong to
each N2KHAB (monitoring or research) scheme (using the scheme-code from
\code{\link{schemes}}).
It also defines typegroup memberships of the types within specific schemes,
if applicable.

\code{read_scheme_types()} reads the \code{\link{scheme_types}} data
source, optionally adds names + shortnames (always done for the typegroup)
and returns it as a
\code{\link[tibble:tbl_df-class]{tibble}}.
A tibble is a data frame that makes working in the tidyverse a little
\href{https://r4ds.had.co.nz/tibbles.html}{easier}.
By default, the data version delivered with the package is used and English
names (\code{lang = "en"}) are returned.
}
\section{Recommended usage}{


\code{read_scheme_types()}

\code{read_scheme_types(lang = "nl")}
}

\section{Anticipating conflicts with n2khab (where this function is deprecated)}{

An efficient way with base R to avoid function masking and conflict warnings
when attaching both \code{{n2khab}} and \code{{n2khabmon}}, regardless of the order
in which they're loaded, is by specifying something as below in your
script, at least \emph{before} loading \code{{n2khab}}:

\code{conflictRules("n2khab", exclude = c("read_schemes", "read_scheme_types"))}
}

\examples{
read_scheme_types()
read_scheme_types(lang = "nl")

}
\seealso{
\code{\link{scheme_types}}

Other reading functions for n2khabmon-referencelists: 
\code{\link{read_namelistmon}()},
\code{\link{read_schemes}()}
}
\concept{reading functions for n2khabmon-referencelists}
