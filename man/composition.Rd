% Generated by roxygen2 (4.0.1): do not edit by hand
\name{\%,\%}
\alias{\%,\%}
\title{Composition operator.}
\usage{
lhs \%,\% rhs
}
\arguments{
\item{lhs}{a function/expression}

\item{rhs}{a function/expression}
}
\value{
a composite function
}
\description{
The composition operator combined lhs and rhs using the compose function.
If several expressions are composed, e.g. a \%,\% b \%,\% c, then
the result will be \code{compose(a, b, c)}.
}

