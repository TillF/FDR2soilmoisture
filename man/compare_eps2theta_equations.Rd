\name{compare_eps2theta_equations}
\alias{compare_eps2theta_equations}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
compare_eps2theta_equations
}
\description{
Compare various equations for converting epsilon to theta by creating a plot and showing R2-values. The function uses the equations included in \code{\link{eps2theta}}. It also customizes some of them with fitted coefficients, plus some GLM variants using different link functions.
}
\usage{
compare_eps2theta_equations(common_set, legend_args=NULL)
}
\arguments{
  \item{common_set}{
	Data frame with observed data. See details. 
}
\item{legend_args}{
	Arguments passed to \code{\link{legend}()} to create the legend for the plots. 
}

}
\details{
The dataframe \code{common_set} holds measured values of epsilon and observed values of soil moisture (theta, e.g. obtained by gravimetric method). 
Additionally, other explanatory variables (e.g. bulk density, clay contents, etc.) can be contained (see details in \code{\link{eps2theta}}).
The \code{compare_eps2theta_equations} applies all equations supported by \code{\link{eps2theta}}, where possible with the provided datsa. It also adjusts the coefficients of these equations to better match the data, yielding equations variants with suffix "_adj". Whenever some coefficients cannot be estimated (e.g. when the column of bulk density is all set to the same value), the value of the original equation is used.

The dataframe \code{common_set} may hold a logical column \code{training}. If present, the custom model fits (see above) are based on this subset only; the remaining data are also displayed and their R2-values ("testing") reported.

The dataframe \code{common_set} may hold a logical column \code{excluded}. If present, the custom model fits are based on all training data, but the function will also return an \code{R2_training_ex} with R2-values calculated from training data WITHOUT the excluded records. this may be useful when additional records are added to force plausible behaviour at the extremes.

For plotting, the dataframe \code{common_set} may hold columns \code{col} and \code{pch}. These are used in the plots as respective arguments.

}

\value{
A list with the elements \code{r2_train, r2_train_ex, r2_test, rmse_train, rmse_test, eps2theta_function}, each holding the computed R2-values and the conversion functions, respectively.

Additionally, the function generates three figures:

\enumerate{
\item matrix of plots showing the tested equations and the respective R2-values (training and testing); 

\item a plot showing the behaviour of the equations over the range of epsilon-values (other data averaged). This plot can help to identify equations with implausible behaviour in extrapolation.

\item a scatterplot of R2-values (trimmed to at least -0.1) for all equations
}

}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Till Francke
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{eps2theta}}
}
\examples{
#create some fake data
  set.seed(1)
  observations = data.frame(epsilon    = runif(n=100, min=5,   max=30), 
                            BD         = runif(n=100, min=0.8, max=1.7),
                            clay_perc  = runif(n=100, min=1, max=40),
                            om_perc    = runif(n=100, min=0, max=30)
                            )
  observations$theta = sqrt(observations$eps) * runif(n=100, min=0.8, max=1.2)
  observations$theta = observations$theta / max(observations$theta)
  observations$training   = c(rep(TRUE, 50), rep(FALSE, 50)) #use the fisrt 50 for training, the rest for validation

res = compare_eps2theta_equations(common_set=observations)

}

