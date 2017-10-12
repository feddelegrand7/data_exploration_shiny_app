file_ext2 <- function(x)
{
  pos <- regexpr("\\.([[:alnum:]]+)(\\.(gz|bz2|xz|zip))*$", x)
  if(pos > -1L) substring(x, pos + 1L) else ""
}

char_or_fac <- function(x) is.character(x) || is.factor(x)

category.vars <- function(cn, dat) cn[map_lgl(dat, char_or_fac)]

zipped <- function(...)
{
  unlist(lapply(list(...), paste0, c("", ".gz", ".bz2", ".xz", ".zip")))
}

read_my_file <- function(fp)
{
  ext <- tools::file_ext(fp)
  if(ext %in% zipped("csv"))
  {
    out <- read_csv(fp, col_names = TRUE, col_types = cols())
  } else if(ext %in% zipped("txt"))
  {
    out <- read_table2(fp, col_names = TRUE, col_types = cols())
  } else if(ext %in% zipped("tab", "tsv"))
  {
    out <- read_tsv(fp, col_names = TRUE, col_types = cols())
  } else if(ext %in% "sas7bdat")
  {
    out <- haven::read_sas(fp)
  } else if(ext %in% c("xlsx", "xls"))
  {
    out <- readxl::read_excel(fp)
  } else if(ext %in% "rds")
  {
    out <- readRDS(fp)
  }
  attr(out, "extension") <- ext
  out
}

#################################################################################################################

count_unique <- function(vars, dat)
{
  vars <- vars[vars %in% colnames(dat)]
  map_int(dat[vars], function(x) if(char_or_fac(x)) length(unique(x)) else 0L)
}

do_the_tableby <- function(y, x, dat)
{
  x <- x[x != " "]
  if(is.null(dat) || length(y) * length(x) * nrow(dat) == 0 || !is.character(y) || !is.character(x))
  {
    cat("")
  } else if(any(count_unique(c(y, x), dat) > 20))
  {
    cat("Sorry, this app only supports categorical variables with <= 20 levels.")
  } else
  {
    summary(tableby(formulize(y, x[x != " "]), data = dat), text = TRUE)
  }
  invisible(NULL)
}

#################################################################################################################

PLOTTYPES <- c("Scatter Plot" = "geom_point",
               "Histogram" = "geom_histogram",
               "Boxplot" = "geom_boxplot",
               "Line Plot" = "geom_line")

do_the_ggplot <- function(..., facet, type, dat)
{
  args <- list(...)
  FUN <- match.fun(type)
  if((args$y == " " && type != "geom_histogram") || args$x == " ")
  {
    return(list(text = "Please select x- and y-variables."))
  } else
  {
    if(type == "geom_histogram")
    {
      args$y <- NULL
      if(char_or_fac(dat[[args$x]]))
      {
        return(list(text = "Sorry, histograms require a continuous x-variable!"))
      }

    }
    args <- args[map_lgl(args, function(x) x != " ")]

    p <- ggplot(dat, do.call(aes_string, args)) +
      FUN()
    if(facet != " ") p <- p + facet_wrap(formulize("", facet))
    return(list(plot = p))
  }
}

#################################################################################################################

do_the_survplot <- function(time, event, x, dat)
{
  if(is.null(time) || time == " ") return(NULL)
  lhs <- paste0("Surv(", time, if(!is.null(event) && event != " ") paste0(", ", event) else "", ")")
  x <- x[x != " "]
  rhs <- if(!is.null(x) && length(x) > 0) x[1] else "1"

  autoplot(survfit(formulize(lhs, rhs), data = dat))
}

