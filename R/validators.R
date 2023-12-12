## Validators
## These ensure that the values being input from the questions are of
## the right type and length, and within the right range. They are used in the
## `record_values()` function.
## Each validator must have n as its second argument, even if the question is a
## single response, to keep the pattern generalizable
validators <- list(

  multi_choice = function(q_no, n) {
    function(x) {
      x <- as.numeric(x)
      valid <- all(!is.na(x)) &&
        all(x %in% 0:1) &&
        sum(x) == 1 &&
        length(x) == n
      if (!valid) {
        if (sum(x) == 0) {
          warning("Question ", q_no, " does not appear to have been filled out.")
        } else {
          stop("Question ", q_no,
               ": Value must be length ", n, " and be all 0s and one 1", call. = FALSE)
        }
      }
      as.logical(x)
    }
  },

  multiresponse_binary = function(q_no, n) {
    function(x) {
      x <- as.numeric(x)
      valid <- all(!is.na(x)) &&
        all(x %in% 0:1) &&
        length(x) == n
      if (!valid) {
        stop("Question ", q_no,
             ": Value must be length ", n, " and be all 0s and 1s", call. = FALSE)
      }
      as.logical(x)
    }
  },

  multiresponse_numeric = function(q_no, n, min = -Inf, max = Inf) {
    function(x) {
      x <- as.numeric(x)
      valid <- all(!is.na(x)) &&
        all(x >= min & x <= max) &&
        length(x) == n
      if (!valid) {
        stop("Question ", q_no,
             ": Value must be length ", n,
             " and be an integer from ", min, " to ", max,
             call. = FALSE)
      }
      x
    }
  },

  numeric = function(q_no, n = 1, min = -Inf, max = Inf) {
    function(x) {
      x <- as.numeric(x)
      valid <- length(x) == 1 &&
        x >= min && x <= max
      if (!valid) {
        stop("Question ", q_no,
             ": Value must be a single number between ", min, " and ", max,
             call. = FALSE)
      }
      x
    }
  },

  binary = function(q_no, n) {
    function(x) {
      x <- as.numeric(x)
      valid <- length(x) == n &&
        !is.na(x) &&
        x %in% 0:1
      if (!valid) stop("Question ", q_no,
                       ": Value must be a single logical", call. = FALSE)
      as.logical(x)
    }
  },
  # This one is a special case for pH, where the first value
  # should be a pH measurement (0-14) OR one of the other elements must be 1
  multi_choice_numeric = function(q_no, n, min = 0, max = 14) {
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
        stop("Question ", q_no,
             ": Value must be a single number between ", min, " and ", max,
             call. = FALSE)
      }
      x
    }
  },
  category = function(q_no, n) {
    function(x) {
      x <- as.character(x)
      valid <- length(x) == n

      if (!valid) {
        stop("Question ", q_no,
             ": Value must be a single string",
             call. = FALSE)
      }
      x
    }
  }
)
