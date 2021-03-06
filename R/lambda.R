#' compose a lambda function or chain of functions.
#'
#' This is an alternative syntax for generating anonymous functions.
#' When used in pipelines, the call should be enclosed in parentheses to
#' force evaluation of the function generation before the left-hand side
#' is inserted. If multiple lambda expressions are given they will be
#' nested in a single global function, and evaluated as a chain. Here
#' each input will be available to the next lambda expression (with its
#' initial value).
#'
#' \code{lambda}s have a special syntax, where the expression is defined as
#' \code{symbol ~ expression}. The alias \code{l} is shorthand for \code{lambda}.
#' Previous versions used symbol -> expression syntax, but this caused
#' problems with compiling packages. There is currently a warning if the
#' old syntax is used.
#'
#' @param ... A special kind of expressions for the anonymous function(s).
#'   The syntax is \code{symbol ~ expression}, see the examples.
#' @param .args a list of the form \code{list(a, b = c)} to
#'   specify the remainder of the signature of the composed function.
#' @return a function.
#' @rdname lambda
#' @export
#' @examples
#' compose(x ~ x^2 + 2*x)
#'
#' sapply(1:10, compose(x ~ x^2))
#'
#' Filter(compose(x ~ x > 0), rnorm(100))
#'
#' iris %>%
#'   (compose(dfr ~ rbind(dfr %>% head, dfr %>% tail)))
#'
#' 1:10 %>%
#'   sin %>%
#'   (compose(x ~ {
#'     d <- abs(x) > 0.5
#'     x*d
#'   }))
compose <- function(..., .args = NULL)
{
  dots <- lapply(as.list(substitute(list(...))[-1]), function(dot) {
    if (is.symbol(dot)) {
      dot <- call(as.character(dot), quote(.))
    }
    if (is.call(dot) && !identical(dot[[1]], quote(`~`))) {
      dot <- call("~", quote(.), dot)
    }
    dot
  })

  .args <- substitute(.args)
  if (!is.null(.args)) {
    if (!is.call(.args) ||
        (is.call(.args) && !identical(.args[[1]], quote(list))))
      stop(".args should be a list")

    .args <- as.list(.args[-1])

    .args <- sapply(1:length(.args), function(i) {
      if (is.null(names(.args[i]))) {
        setNames(list(quote(expr = )), .args[i])
      } else {
        eval(.args[i], parent.frame(), parent.frame())
      }
    })
  }

  # Utility function to generate the different function expressions.
  generate <- function(expr, rhs = NULL, parens = FALSE, wrap = FALSE,
                       .args = NULL)
  {
    # Check that lambdas are of the right form x ~ expression_of(x)
    if (!is.call(expr) || !identical(expr[[1]], quote(`~`))) {
      stop("Malformed expression. Expected format is symbol ~ expression.",
           call. = FALSE)
    }

    if (!is.symbol(expr[[2]])) {
      stop("Malformed expression. Expecting one variable name on LHS",
           call. = FALSE)
    }

    # Construct the function inputs
    arg_name <- as.character(expr[[2]])
    args <- c(setNames(list(quote(expr = )), arg_name), .args)
    body <- expr[[3]]

    # Construct a function with or without wrapper/parens
    cl <- if (wrap) {
      inner.cl <-
        call("%>%", expr[[2]], call("(", call("function", as.pairlist(args), body)))
      if (!is.null(rhs))
        inner.cl <- call("%>%", inner.cl, rhs)
      inner.cl
    } else {
      call("function", as.pairlist(args), body)
    }

    if (parens) call("(", cl) else cl
  }

  if (length(dots) == 1) {
    # If only a single lambda is provided; regular lambda function.
    cl <- generate(dots[[1]], .args = .args)

  } else {
    # Multiple lambdas are given. Nest them, and create a single overall lambda.
    cl <-
      Reduce(function(l, r) {
        substitute(a ~ b, list(
          a = l[[2]],
          b = generate(l, wrap = TRUE, rhs = generate(r, parens = TRUE))))
        },
        dots,
        right = TRUE)
    cl <- generate(cl, .args = .args)

  }

  # Evaluate the final function, and return.
  composite <- eval(cl, parent.frame(), parent.frame())
  attr(composite, "parts") <- dots
  class(composite) <- c("composite", "function")
  composite
}

#' Composition operator.
#'
#' The composition operator combined lhs and rhs using the compose function.
#' If several expressions are composed, e.g. a \%,\% b \%,\% c, then
#' the result will be \code{compose(a, b, c)}.
#'
#' @usage lhs \%,\% rhs
#'
#' @param lhs a function/expression
#' @param rhs a function/expression
#'
#' @rdname composition
#'
#' @return a composite function
#' @export
`%,%` <- function(lhs, rhs)
{
  # Capture inputs
  lhs <- substitute(lhs)
  rhs <- substitute(rhs)

  # Utility function to split the call chain.
  call2list <- function(cl)
  {
    if (is.call(cl) && identical(cl[[1]], quote(`%,%`))) {
      lapply(as.list(cl)[-1], call2list)
    } else {
      cl
    }
  }

  parts <- lapply(c(unlist(call2list(lhs)), rhs), function(part) {
    if (is.call(part) && identical(part[[1]], quote(`(`)))
      part[[-1]]
    else
      part
  })

  cl <-
    do.call(compose, parts)

  eval(cl, parent.frame(), parent.frame())
}

#' @rdname lambda
#' @export
lambda <- compose

#' @rdname lambda
#' @export
l <- lambda


#' Print method for composite functions.
#'
#' Generic method for printing of composite functions generated with
#' either \code{\%,\%} or \code{compose}.
#'
#' @param x a composite function
#' @param ... not used.
#'
#' @rdname compose
#' @export
print.composite <- function(x, ...)
{
  cat("Function composed of the following parts:\n\n")
  lapply(attr(x, "parts"), function(p) cat(deparse(p), "\n"))
  invisible(x)
}
