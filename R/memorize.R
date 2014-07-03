#' Memorize variable in pipe operators
#'
#' Use to memorize temporal variable in a chain.
#'
#' @param lhs a value
#' @param rhs a symbol to assign
#' @return The result of evaluting the left-hand side
#' @usage lhs \%->\% rhs
#' @rdname memorize
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(ggplot2)
#'
#' x <- 1:10
#' x %>% sum %->% y %>% multiply_by(10)
#' y
#'
#' iris %>% group_by(Species) %>% summarise(mn=mean(Sepal.Length)) %->% res %>% qplot(data=.,x=Species,y=mn,geom="bar",stat="identity")
#' res
#' }

`%->%` <-  function(lhs, rhs)
{
  # Capture unevaluated arguments
  lhs <- substitute(lhs)
  rhs <- substitute(rhs)
  # Reference the parent frame
  parent <- parent.frame()

  # Check right-hand side
  if (!is.symbol(rhs))
    stop("RHS should be a symbol")

  # Get an environment for evaluation of left-hand side.
  if (exists("__env__", parent, mode = "environment", inherits = FALSE)) {
    # get the existing environment and flag this as not being top-level
    env <- get("__env__", parent)
    # If it is locked, make a new one. Since compound is passed along in this
    # case, we require that toplevel still be FALSE.
    if (env[["__locked__"]])
      env <- pipe_env(parent, compound = env[["__compound__"]])
    toplevel <- FALSE
  } else {
    # Create a new environment, and set top-level
    env <- pipe_env(parent)
    toplevel <- TRUE
  }
  # Find an appropriate name for lhs to use. If at top-level,
  # then try to mimic call, otherwise use "."
  if (!toplevel) {
    nm <- "."
  } else {
    nm <- paste(deparse(lhs), collapse = "")
    nm <- if ((is.call(lhs) || is.name(lhs)) && nchar(nm) < 9900) nm else "."
  }
  # carry out assignment. The name "." also points the resulting
  # value, to allow accessing the left-hand side in nested expressions.
  e <- env[[nm]] <- env[["."]] <- eval(lhs, env)
  env[["__memorize__"]] <- append(env[["__memorize__"]], call("<-", rhs, e))

  # From now on, this environment cannot be re-used.
  env[["__locked__"]] <- TRUE

  # Evaluate the call
  res       <- withVisible(eval(e, env, env))
  visibly   <- res$visible
  to.return <- res$value

  # clean the environment to keep it light in long chains.
  if (nm != ".")
    rm(list = nm, envir = env)

  if (toplevel && !is.null(env[["__memorize__"]])) {
    lapply(get("__memorize__", env), function(m) eval(m, parent, parent))
  }

  if (visibly) to.return else invisible(to.return)
}
