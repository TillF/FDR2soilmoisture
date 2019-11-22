\name{compare_eps2theta_equations}
\alias{compare_eps2theta_equations}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
compare_eps2theta_equations
}
\description{
Compare different equations for converting epsilon to theta by creating a plot and showing R2-values.
}
\usage{
compare_eps2theta_equations(common_set)
}

  \item{common_set}{
	Data frame with observed data. See details. 
}
}
\details{
The dataframe \code{common_set} holds measured values of epsilon and observed values of soil moisture (theta, e.g. obtained by gravimetric method). 
Additionally, other explanatory variables (e.g. bulk density) can be contained (see details in \code{\link{eps2theta}}).
The function applies all equations supported by \code{\link{eps2theta}}. It also adjusts the coefficients of these equations to better match the data, yielding equations variants with suffix "_adj".

The dataframe \code{common_set} may hold a logical column "training". If present, the custom model fits (see above) are based on this subset only; the remaining data are also displayed and R2-values reported.


}

\value{
A list with the elements \code{r2_train, r2_test, eps2theta_function}, each holding the computed R2-values and the conversion functions, repsectively.

Additionally, the function generates a matrix of plots showing the tested equations and the respective R2-values (training and testing).
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Till Francke
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{eps2theta}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
