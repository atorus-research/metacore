# pretty sure the R6 class generator reassigns the function environment to the class environment
# so it forgets about the function factory's execution environment

library(R6)
readonly <- function(name) {
   on.exit(e <<- environment())
   inside <- function(value) {
      browser()
      if (missing(value)) {
         private[[name]]
      } else {
         stop(paste0(name, " is read only"), call. = FALSE)
      }
   }
}

Person <- R6Class("Person",
                  private = list(
                     .name = NA
                  ),
                  active = list(
                     name = readonly(".name")
                  ),
                  public = list(
                     initialize = function(name) {
                        private$.name <- name
                     }
                  )
)

# show the execution environment
e
#> <environment: 0x00000000143f6c60>
# currently same
identical(e, environment(Person$active$name))
#> [1] TRUE
maya <- Person$new("maya")
# how about now?
identical(e, environment(activeBindingFunction("name", maya)))
#> [1] FALSE
# Nope!
environment(activeBindingFunction("name", maya))
#> <environment: 0x00000000147c3d00>

# if you look at Person$new you will see that the initializer calls
if (!is.null(active)) active <- assign_func_envs(active, enclos_env)

# So instead, lets workaround with using attributes!

library(R6)
readonly <- function(name) {
   inside <- function(value) {
      name <- attr(sys.function(sys.parent()), "name")
      if (missing(value)) {
         private[[name]]
      } else {
         stop(paste0(name, " is read only"), call. = FALSE)
      }
   }
   attributes(inside) <- list(name = name)
   inside
}
Person <- R6Class("Person",
                  private = list(
                     .age = NA,
                     .name = NA
                  ),
                  active = list(
                     age = readonly(".age"),
                     name = readonly(".name")
                  ),
                  public = list(
                     initialize = function(name, age = NA) {
                        private$.name <- name
                        private$.age <- age
                     }
                  )
)
maya <- Person$new("maya")
maya$name
#> [1] "maya"
maya$name <- "maya2"
#> Error: .name is read only
maya$age
#> [1] NA
maya$age <- 26L
#> Error: .age is read only

# OR if we want to "cheat" and use an environment
readonly <- function(name) {
   e <- environment()
   inside <-
      function(value, .env = e) {
         name <- .env[["name"]]
         if (missing(value)) {
            private[[name]]
         } else {
            stop(paste0(name, " is read only"), call. = FALSE)
         }
      }
   inside
}


# mikes idea
readonly <- function(name) {
   inside <-
      function(value) {
         if (missing(value)) {
            name <- rlang::env_get(env = rlang::env_parent(), "name", NULL)
            private[[name]]
         } else {
            remove_dot <- gsub("\\.","", name)
            stop(paste0(remove_dot, " is read only"), call. = FALSE)
         }
      }
   inside
}


#rlang way
readonly <- function(name) {
   args <- list(value = name)
   body <-
      rlang::expr({
         if (missing(value)) {
            private[[name]]
         } else {
            remove_dot <- gsub("\\.","", name)
            stop(paste0(remove_dot, " is read only"), call. = FALSE)
         }
      })
   rlang::new_function(args, body)
}
