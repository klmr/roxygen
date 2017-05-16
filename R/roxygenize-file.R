#' Roxygenise a single file
#'
#' This function parses the documentation of a single file and returns the
#' parsed objects. Unlike [roxygenize()], the result of this function are not
#' immediately useful; they need to be further transformed.
#' @param file Path of the file to process.
#' @param env An environment containing the result of parsing and evaluating
#'   the file.
#' @param wrap Should Roxygen output be wrapped? `FALSE` by default.
#' @param roclets Character vector of roclet names to use with file. The
#'   default is `"rd"`.
#' @param markdown Logical value indicating whether to parse Markdown tags.
#'   This can be overridden locally by the tags '@rd` and `@NoRd` inside a
#'   Roxygen comment.
#' @param file_encoding THe file encoding. Default: `"UTF-8"`.
#' @param write_output If `"TRUE"`, use [roclet_output()] to write the result
#'   to output files. Otherwise the result of processing the roclets will be
#'   returned in a list.
#' @export
roxygenize_file <- function(file,
                            env,
                            wrap = FALSE,
                            roclets = "rd",
                            markdown = markdown_global_default,
                            file_encoding = "UTF-8",
                            write_output = TRUE) {
  if (length(roclets) == 0)
    return(invisible())

  roclets <- lapply(roclets, roclet_find)

  # Generate registry of all roclet tags
  tags <- c(lapply(roclets, roclet_tags), list(list(include = tag_value)))
  registry <- unlist(tags, recursive = FALSE)

  options <- list(wrap = wrap, roclets = roclets, markdown = markdown)
  parsed <- list(
    env = env,
    blocks = parse_blocks(file, env, registry = registry,
                          global_options = options,
                          fileEncoding = file_encoding)
  )

  roc_out <- function(roc) {
    results <- roclet_process(roc, parsed, dirname(file))
    if (write_output) {
      roclet_output(roc, results, base_path, is_first = is_first)
    } else {
      results
    }
  }
  invisible(unlist(lapply(roclets, roc_out), recursive = FALSE))
}

#' @rdname roxygenize_file
#' @export
roxygenise_file <- roxygenize_file
