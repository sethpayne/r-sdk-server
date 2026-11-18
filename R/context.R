#' Evaluation context model (parity with python-server-sdk Context)
#'
#' Immutable evaluation contexts for flag evaluations and analytics events.
#' Build with [ld_context()], [context_builder()], [context_create()],
#' [context_create_multi()], or [context_from_list()] / [context_from_json()].
#'
#' @name context
NULL

LD_CONTEXT_DEFAULT_KIND <- "user"
LD_CONTEXT_MULTI_KIND <- "multi"

.ld_invalid_kind_re <- "[^-a-zA-Z0-9._]"

.ld_escape_key_for_fqk <- function(key) {
  key <- gsub("%", "%25", key, fixed = TRUE)
  gsub(":", "%3A", key, fixed = TRUE)
}

.ld_validate_kind <- function(kind) {
  if (!is.character(kind) || length(kind) != 1L || is.na(kind)) {
    return("context kind must be a single string")
  }
  if (identical(kind, "")) {
    return("context kind must not be empty")
  }
  if (identical(kind, "kind")) {
    return("\"kind\" is not a valid context kind")
  }
  if (identical(kind, "multi")) {
    return("context of kind \"multi\" must be created with create_multi or multi_builder")
  }
  if (grepl(.ld_invalid_kind_re, kind)) {
    return("context kind contains disallowed characters")
  }
  NULL
}

.ld_make_invalid <- function(error) {
  structure(
    list(
      kind = "",
      key = "",
      name = NULL,
      anonymous = FALSE,
      attributes = NULL,
      private_attributes = NULL,
      multi = NULL,
      fully_qualified_key = "",
      error = error
    ),
    class = "ld_context"
  )
}

.ld_new_single <- function(kind, key, name = NULL, anonymous = FALSE,
                           attributes = NULL, private_attributes = NULL,
                           allow_empty_key = FALSE) {
  if (is.null(kind)) {
    kind <- LD_CONTEXT_DEFAULT_KIND
  }
  kind_error <- .ld_validate_kind(kind)
  if (!is.null(kind_error)) {
    return(.ld_make_invalid(kind_error))
  }
  if (!is.character(key) || length(key) != 1L || is.na(key)) {
    return(.ld_make_invalid("context key must be a single non-NA string"))
  }
  if (identical(key, "") && !isTRUE(allow_empty_key)) {
    return(.ld_make_invalid("context key must not be None or empty"))
  }
  fqk <- if (identical(kind, LD_CONTEXT_DEFAULT_KIND)) {
    key
  } else {
    paste0(kind, ":", .ld_escape_key_for_fqk(key))
  }
  structure(
    list(
      kind = kind,
      key = key,
      name = name,
      anonymous = isTRUE(anonymous),
      attributes = attributes,
      private_attributes = private_attributes,
      multi = NULL,
      fully_qualified_key = fqk,
      error = NULL
    ),
    class = "ld_context"
  )
}

.ld_new_multi <- function(contexts) {
  if (length(contexts) == 0L) {
    return(.ld_make_invalid("multi-context must contain at least one kind"))
  }
  kinds <- vapply(contexts, function(c) c$kind, character(1))
  ord <- order(kinds)
  contexts <- contexts[ord]
  last_kind <- NULL
  errors <- character()
  full_key <- ""
  for (c in contexts) {
    if (!is.null(c$error)) {
      errors <- c(errors, c$error)
      next
    }
    if (!is.null(last_kind) && identical(c$kind, last_kind)) {
      return(.ld_make_invalid("multi-kind context cannot have same kind more than once"))
    }
    last_kind <- c$kind
    if (!identical(full_key, "")) {
      full_key <- paste0(full_key, ":")
    }
    full_key <- paste0(full_key, c$kind, ":", .ld_escape_key_for_fqk(c$key))
  }
  if (length(errors) > 0L) {
    return(.ld_make_invalid(paste(errors, collapse = ", ")))
  }
  structure(
    list(
      kind = LD_CONTEXT_MULTI_KIND,
      key = "",
      name = NULL,
      anonymous = FALSE,
      attributes = NULL,
      private_attributes = NULL,
      multi = contexts,
      fully_qualified_key = full_key,
      error = NULL
    ),
    class = "ld_context"
  )
}

#' Create a single-kind context with only key and kind
#'
#' @param key Context key (string).
#' @param kind Context kind; defaults to `"user"`.
#' @return An `ld_context` object.
#' @export
context_create <- function(key, kind = NULL) {
  .ld_new_single(kind, as.character(key))
}

#' Create a multi-context from single-kind contexts
#'
#' @param ... One or more `ld_context` objects.
#' @return An `ld_context` (multi or the single context if only one was given).
#' @export
context_create_multi <- function(...) {
  contexts <- list(...)
  if (length(contexts) == 1L && is.list(contexts[[1]]) &&
      !inherits(contexts[[1]], "ld_context")) {
    contexts <- contexts[[1]]
  }
  b <- ContextMultiBuilder$new()
  for (c in contexts) {
    b$add(c)
  }
  b$build()
}

#' Convenience constructor for a single-kind context
#'
#' @param key Context key.
#' @param kind Context kind (default `"user"`).
#' @param name Optional name.
#' @param anonymous Logical.
#' @param ... Additional custom attributes as named arguments.
#' @param private Character vector of private attribute references.
#' @return An `ld_context`.
#' @export
ld_context <- function(key, kind = "user", name = NULL, anonymous = FALSE,
                       ..., private = NULL) {
  b <- context_builder(key)
  b$kind(kind)
  if (!is.null(name)) b$name(name)
  b$anonymous(isTRUE(anonymous))
  extras <- list(...)
  for (nm in names(extras)) {
    b$set(nm, extras[[nm]])
  }
  if (!is.null(private) && length(private) > 0L) {
    do.call(b$mark_private, as.list(private))
  }
  b$build()
}

#' @export
print.ld_context <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}

#' @export
format.ld_context <- function(x, ...) {
  if (!context_valid(x)) {
    return(sprintf("[invalid context: %s]", x$error))
  }
  context_to_json(x)
}

#' @export
`[[.ld_context` <- function(x, i, ...) {
  context_get(x, i)
}

#' @export
`$.ld_context` <- function(x, name) {
  # Prefer structural fields for internal use; attribute lookup via context_get
  if (name %in% names(unclass(x))) {
    return(unclass(x)[[name]])
  }
  context_get(x, name)
}

#' Whether the context is valid
#' @param context An `ld_context`.
#' @return Logical.
#' @export
context_valid <- function(context) {
  is.null(context$error)
}

#' Error message for an invalid context, or `NULL`
#' @param context An `ld_context`.
#' @export
context_error <- function(context) {
  context$error
}

#' Whether this is a multi-context
#' @param context An `ld_context`.
#' @export
context_multiple <- function(context) {
  !is.null(context$multi)
}

#' Look up an attribute by name
#' @param context An `ld_context`.
#' @param attribute Attribute name.
#' @return Attribute value or `NULL`.
#' @export
context_get <- function(context, attribute) {
  if (identical(attribute, "key")) return(context$key)
  if (identical(attribute, "kind")) return(context$kind)
  if (identical(attribute, "name")) return(context$name)
  if (identical(attribute, "anonymous")) return(context$anonymous)
  if (is.null(context$attributes)) return(NULL)
  context$attributes[[attribute]]
}

#' Number of individual context kinds
#' @param context An `ld_context`.
#' @export
context_individual_count <- function(context) {
  if (!is.null(context$error)) return(0L)
  if (is.null(context$multi)) return(1L)
  length(context$multi)
}

#' Get an individual single-kind context by index or kind string
#' @param context An `ld_context`.
#' @param kind Integer index (0-based) or kind string.
#' @export
context_get_individual <- function(context, kind) {
  if (!is.null(context$error)) return(NULL)
  if (is.character(kind)) {
    if (is.null(context$multi)) {
      return(if (identical(kind, context$kind)) context else NULL)
    }
    for (c in context$multi) {
      if (identical(c$kind, kind)) return(c)
    }
    return(NULL)
  }
  idx <- as.integer(kind)
  if (is.null(context$multi)) {
    return(if (identical(idx, 0L)) context else NULL)
  }
  if (idx < 0L || idx >= length(context$multi)) return(NULL)
  context$multi[[idx + 1L]]
}

#' Fully-qualified key
#' @param context An `ld_context`.
#' @export
context_fully_qualified_key <- function(context) {
  context$fully_qualified_key
}

#' Custom (non-built-in) attribute names
#' @param context An `ld_context`.
#' @export
context_custom_attributes <- function(context) {
  if (is.null(context$attributes)) character() else names(context$attributes)
}

#' Private attribute references for this context
#' @param context An `ld_context`.
#' @export
context_private_attributes <- function(context) {
  if (is.null(context$private_attributes)) character() else context$private_attributes
}

#' Drop anonymous individual contexts (parity with Python)
#' @param context An `ld_context`.
#' @export
context_without_anonymous <- function(context) {
  parts <- if (!is.null(context$multi)) context$multi else list(context)
  parts <- Filter(function(c) !isTRUE(c$anonymous), parts)
  do.call(context_create_multi, parts)
}

.ld_to_list_single <- function(context, with_kind) {
  ret <- list(key = context$key)
  if (isTRUE(with_kind)) ret$kind <- context$kind
  if (!is.null(context$name)) ret$name <- context$name
  if (isTRUE(context$anonymous)) ret$anonymous <- TRUE
  if (!is.null(context$attributes)) {
    for (nm in names(context$attributes)) {
      ret[[nm]] <- context$attributes[[nm]]
    }
  }
  if (!is.null(context$private_attributes)) {
    ret[["_meta"]] <- list(privateAttributes = as.list(context$private_attributes))
  }
  ret
}

#' Convert context to a list (JSON-shaped)
#' @param context An `ld_context`.
#' @export
context_to_list <- function(context) {
  if (!context_valid(context)) return(list())
  if (!is.null(context$multi)) {
    ret <- list(kind = "multi")
    for (c in context$multi) {
      ret[[c$kind]] <- .ld_to_list_single(c, FALSE)
    }
    return(ret)
  }
  .ld_to_list_single(context, TRUE)
}

#' JSON string representation of a context
#' @param context An `ld_context`.
#' @export
context_to_json <- function(context) {
  jsonlite::toJSON(context_to_list(context), auto_unbox = TRUE, null = "null")
}

#' Parse a context from a list (JSON object)
#' @param props Named list.
#' @export
context_from_list <- function(props) {
  if (is.null(props)) {
    return(.ld_make_invalid("Cannot use None as a context"))
  }
  kind <- props[["kind"]]
  if (!is.character(kind) || length(kind) != 1L) {
    return(.ld_make_invalid('invalid data type for "kind"'))
  }
  if (identical(kind, "multi")) {
    b <- ContextMultiBuilder$new()
    for (nm in names(props)) {
      if (identical(nm, "kind")) next
      v <- props[[nm]]
      if (!is.list(v) || is.null(names(v))) {
        return(.ld_make_invalid(sprintf('invalid data type for "%s"', nm)))
      }
      b$add(.ld_from_list_single(v, nm))
    }
    return(b$build())
  }
  .ld_from_list_single(props, kind)
}

#' Parse a context from a JSON string
#' @param json Character JSON.
#' @export
context_from_json <- function(json) {
  props <- tryCatch(
    jsonlite::fromJSON(json, simplifyVector = FALSE),
    error = function(e) e
  )
  if (inherits(props, "error")) {
    return(.ld_make_invalid(conditionMessage(props)))
  }
  context_from_list(props)
}

.ld_from_list_single <- function(props, kind) {
  b <- ContextBuilder$new("")
  if (!is.null(kind)) b$kind(kind)
  for (k in names(props)) {
    v <- props[[k]]
    if (identical(k, "_meta")) {
      if (is.null(v)) next
      if (!is.list(v)) {
        return(.ld_make_invalid(sprintf('invalid data type for "%s"', k)))
      }
      p <- v[["privateAttributes"]]
      if (!is.null(p)) {
        if (!is.list(p) && !is.character(p)) {
          return(.ld_make_invalid('invalid data type for "privateAttributes"'))
        }
        for (pa in p) {
          if (!is.character(pa) || length(pa) != 1L) {
            return(.ld_make_invalid('invalid data type for "privateAttributes"'))
          }
          b$mark_private(pa)
        }
      }
    } else {
      if (!b$try_set(k, v)) {
        return(.ld_make_invalid(sprintf('invalid data type for "%s"', k)))
      }
    }
  }
  b$build()
}

#' Create a single-kind context builder
#' @param key Context key.
#' @export
context_builder <- function(key) {
  ContextBuilder$new(key)
}

#' Create a multi-context builder
#' @export
context_multi_builder <- function() {
  ContextMultiBuilder$new()
}

#' @rdname context_builder
#' @export
ContextBuilder <- R6::R6Class(
  "ContextBuilder",
  public = list(
    #' @description Create a builder.
    #' @param key Context key.
    #' @param copy_from Optional existing `ld_context` to copy.
    initialize = function(key, copy_from = NULL) {
      private$key_ <- as.character(key)
      if (is.null(copy_from)) {
        private$kind_ <- LD_CONTEXT_DEFAULT_KIND
        private$name_ <- NULL
        private$anonymous_ <- FALSE
        private$attributes_ <- NULL
        private$private_ <- NULL
        private$cow_attrs_ <- FALSE
        private$cow_private_ <- FALSE
      } else {
        private$kind_ <- copy_from$kind
        private$name_ <- copy_from$name
        private$anonymous_ <- copy_from$anonymous
        private$attributes_ <- copy_from$attributes
        private$private_ <- copy_from$private_attributes
        private$cow_attrs_ <- !is.null(private$attributes_)
        private$cow_private_ <- !is.null(private$private_)
      }
      private$allow_empty_key_ <- FALSE
      invisible(self)
    },
    #' @description Build an immutable context.
    build = function() {
      private$cow_attrs_ <- !is.null(private$attributes_)
      private$cow_private_ <- !is.null(private$private_)
      .ld_new_single(
        private$kind_, private$key_, private$name_, private$anonymous_,
        private$attributes_, private$private_, private$allow_empty_key_
      )
    },
    #' @param key New key.
    key = function(key) {
      private$key_ <- as.character(key)
      invisible(self)
    },
    #' @param kind New kind.
    kind = function(kind) {
      if (is.null(kind) || identical(kind, "")) {
        private$kind_ <- LD_CONTEXT_DEFAULT_KIND
      } else {
        private$kind_ <- as.character(kind)
      }
      invisible(self)
    },
    #' @param name Name or `NULL`.
    name = function(name) {
      private$name_ <- name
      invisible(self)
    },
    #' @param anonymous Logical.
    anonymous = function(anonymous) {
      private$anonymous_ <- isTRUE(anonymous)
      invisible(self)
    },
    #' @param attribute Attribute name.
    #' @param value Value (or `NULL` to unset).
    set = function(attribute, value) {
      self$try_set(attribute, value)
      invisible(self)
    },
    #' @param attribute Attribute name.
    #' @param value Value.
    try_set = function(attribute, value) {
      if (identical(attribute, "") || identical(attribute, "_meta")) {
        return(FALSE)
      }
      if (identical(attribute, "key")) {
        if (is.character(value) && length(value) == 1L) {
          private$key_ <- value
          return(TRUE)
        }
        return(FALSE)
      }
      if (identical(attribute, "kind")) {
        if (is.character(value) && length(value) == 1L) {
          private$kind_ <- value
          return(TRUE)
        }
        return(FALSE)
      }
      if (identical(attribute, "name")) {
        if (is.null(value) || (is.character(value) && length(value) == 1L)) {
          private$name_ <- value
          return(TRUE)
        }
        return(FALSE)
      }
      if (identical(attribute, "anonymous")) {
        if (is.logical(value) && length(value) == 1L) {
          private$anonymous_ <- isTRUE(value)
          return(TRUE)
        }
        return(FALSE)
      }
      if (private$cow_attrs_) {
        private$cow_attrs_ <- FALSE
        private$attributes_ <- if (is.null(private$attributes_)) NULL else as.list(private$attributes_)
      }
      if (is.null(private$attributes_)) private$attributes_ <- list()
      if (is.factor(value)) value <- as.character(value)
      if (is.null(value)) {
        private$attributes_[[attribute]] <- NULL
      } else {
        private$attributes_[[attribute]] <- value
      }
      TRUE
    },
    #' @param ... Attribute names/paths to mark private.
    #'   Named `mark_private` because R6 reserves `private`.
    mark_private = function(...) {
      attrs <- unlist(list(...), use.names = FALSE)
      if (length(attrs) > 0L) {
        if (private$cow_private_) {
          private$cow_private_ <- FALSE
          private$private_ <- if (is.null(private$private_)) NULL else as.character(private$private_)
        }
        if (is.null(private$private_)) private$private_ <- character()
        private$private_ <- c(private$private_, as.character(attrs))
      }
      invisible(self)
    }
  ),
  private = list(
    key_ = NULL,
    kind_ = NULL,
    name_ = NULL,
    anonymous_ = FALSE,
    attributes_ = NULL,
    private_ = NULL,
    cow_attrs_ = FALSE,
    cow_private_ = FALSE,
    allow_empty_key_ = FALSE
  )
)

#' @rdname context_multi_builder
#' @export
ContextMultiBuilder <- R6::R6Class(
  "ContextMultiBuilder",
  public = list(
    #' @description Create an empty multi builder.
    initialize = function() {
      private$contexts_ <- list()
      private$cow_ <- FALSE
      invisible(self)
    },
    #' @description Build the multi-context (or single if only one added).
    build = function() {
      if (length(private$contexts_) == 1L) {
        return(private$contexts_[[1L]])
      }
      private$cow_ <- TRUE
      .ld_new_multi(private$contexts_)
    },
    #' @param context An `ld_context` to add (flattens nested multi).
    add = function(context) {
      if (context_multiple(context)) {
        n <- context_individual_count(context)
        for (i in seq_len(n) - 1L) {
          c <- context_get_individual(context, i)
          if (!is.null(c)) self$add(c)
        }
      } else {
        if (private$cow_) {
          private$cow_ <- FALSE
          private$contexts_ <- as.list(private$contexts_)
        }
        private$contexts_ <- c(private$contexts_, list(context))
      }
      invisible(self)
    }
  ),
  private = list(
    contexts_ = NULL,
    cow_ = FALSE
  )
)
