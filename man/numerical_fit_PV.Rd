% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_numerical_fit_PV.R
\name{numerical_fit_PV}
\alias{numerical_fit_PV}
\title{Numerically fit the Breakthrough Curves - Pore Volume Methodology}
\usage{
numerical_fit_PV(
  time,
  PV,
  CCo,
  saturated_volumetric_water_content,
  flask_volume,
  column_length,
  column_diameter
)
}
\arguments{
\item{PV}{Pore Volume measured between each experimental observation - LIST.}

\item{CCo}{Relative concentration between each experimental observation - LIST.}

\item{saturated_volumetric_water_content}{Measured Saturated Volumetric Water Content - Numerical Input.}

\item{column_length}{total length of soil column (cm) - Numerical Input.}

\item{column_diameter}{total diameter of soil column (cm) - Numerical Input.}
}
\value{
Returns a table with the experimental information, numerical fit of the relative concentration, transport parameters, and RMSE.
}
\description{
Performs the numerical adjustment of the relative concentration of solutes through the Pore Volume based methodology - check inputs; and returns a dataframe with the fitted values and transport parameters.
}
\examples{
MRE <- xlsx::read.xlsx("./inst/app/www/MRE.xlsx",sheetIndex = 1) \%>\% tidyr::as_tibble()
fitted_MRE <- numerical_fit_PV(MRE$Pore_Volume, MRE$CCo, 0.42, 20, 5.2)

}
