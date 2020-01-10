#' huxtable_to_png
#' @param tab Huxtable
#' @param file Resultant .png file
#' @export
huxtable_to_png <- function(tab, file) {
  name <- fs::path_file(stringr::str_remove(file, ".png"))
  dir_end <- fs::path_dir(file)
  dir_tmp <- fhi::temp_dir()

  file_tex <- fs::path(dir_tmp, glue::glue("{name}.tex"))
  file_dvi <- fs::path(dir_tmp, glue::glue("{name}.dvi"))
  file_ <- fs::path(dir_tmp, glue::glue("{name}"))

  output <- glue::glue("
\\documentclass{{standalone}}
\\usepackage{{booktabs}}
\\usepackage{{array}}
\\usepackage{{caption}}
\\usepackage{{graphicx}}
\\usepackage{{siunitx}}
\\usepackage{{colortbl}}
\\usepackage{{multirow}}
\\usepackage{{hhline}}
\\usepackage{{calc}}
\\usepackage{{tabularx}}
\\usepackage{{threeparttable}}
\\usepackage{{wrapfig}}

\\begin{{document}}
{huxtable::to_latex(tab,tabular_only=T)}
\\end{{document}}")
  cat(output, file = file_tex)

  withr::with_dir(dir_tmp, tools::texi2dvi(file = file_tex))

  cmd <- paste("cd", shQuote(dir_tmp), "; dvipng -T tight -D 1200 -z 9", shQuote(file_dvi))
  invisible(system(cmd))

  cleaner <- c(".tex", ".aux", ".log", ".dvi")
  invisible(file.remove(paste(file_, cleaner, sep = "")))

  old <- glue::glue("{file_}1.png")
  new <- file

  unlink(new)
  processx::run("mv", c(old, new))

  return(invisible(file))
}
