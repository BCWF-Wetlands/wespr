## Validators
## These ensure that the values being input from the questions are of
## the right type and length, and within the right range. They are used in the
## `record_values()` function
validators <- list(

  multi_choice = function(n) {
    function(x) {
      x <- as.numeric(x)
      valid <- all(!is.na(x)) &&
        all(x %in% 0:1) &&
        sum(x) == 1 &&
        length(x) == n
      if (!valid) {
        stop("Value must be length ", n, " and be all 0s and one 1", call. = FALSE)
      }
      as.logical(x)
    }
  },

  multiresponse_binary = function(n) {
    function(x) {
      x <- as.numeric(x)
      valid <- all(!is.na(x)) &&
        all(x %in% 0:1) &&
        length(x) == n
      if (!valid) {
        stop("Value must be length ", n, " and be all 0s and 1s", call. = FALSE)
      }
      as.logical(x)
    }
  },

  multiresponse_numeric = function(n, min = -Inf, max = Inf) {
    function(x) {
      x <- as.numeric(x)
      valid <- all(!is.na(x)) &&
        all(x %in% min:max) &&
        length(x) == n
      if (!valid) {
        stop("Value must be length ", n,
             " and be an integer from ", min, " to ", max,
             call. = FALSE)
      }
      x
    }
  },

  numeric = function(n = 1, min = -Inf, max = Inf) {
    function(x) {
      x <- as.numeric(x)
      valid <- length(x) == 1 &&
        x >= min && x <= max
      if (!valid) {
        stop("Value must be a single number between ", min, " and ", max,
             call. = FALSE)
      }
      x
    }
  },

  binary = function(n) {
    function(x) {
      x <- as.numeric(x)
      valid <- length(x) == n &&
        !is.na(x) &&
        x %in% 0:1
      if (!valid) stop("Value must be a single logical", call. = FALSE)
      as.logical(x)
    }
  },
  # This one is a special case for pH, where the first value
  # should be a pH measurement (0-14) OR one of the other elements must be 1
  multi_choice_numeric = function(n, min = 0, max = 14) {
    function(x) {
      x <- as.numeric(x)
      valid <- length(x) == n &&
        sum(!is.na(x) == 1)

      if (!is.na(x[1])) {
        valid <- valid && x[1] >= min && x[1] <= max
      } else {
        valid <- valid && sum(x[-1]) == 1
      }

      if (!valid) {
        stop("Value must be a single number between ", min, " and ", max,
             call. = FALSE)
      }
      x
    }
  },
  category = function(n) {
    function(x) {
      x <- as.character(x)
      valid <- length(x) == n

      if (!valid) {
        stop("Value must be a single string",
             call. = FALSE)
      }
      x
    }
  }
)