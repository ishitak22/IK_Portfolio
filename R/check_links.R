# Checks every link and embed on the rendered site for broken targets.
# Run after `quarto render`, from the project root:
#   Rscript R/check_links.R

library(curl)

site <- "_site"
pages <- list.files(site, pattern = "\\.html$", recursive = TRUE, full.names = TRUE)
pages <- pages[!grepl("site_libs", pages)]

extract <- function(page) {
  html <- paste(readLines(page, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  m <- regmatches(html, gregexpr('(href|src)="[^"#]+', html))[[1]]
  unique(sub('^(href|src)="', "", m))
}

links <- do.call(rbind, lapply(pages, function(p) {
  l <- extract(p)
  if (!length(l)) return(NULL)
  data.frame(page = sub(paste0(site, "/"), "", p, fixed = TRUE), link = l)
}))
links <- links[!grepl("^(mailto:|javascript:|data:)", links$link), ]

# Internal links: does the file exist in _site?
internal <- links[!grepl("^https?://", links$link), ]
clean <- sub("\\?.*$", "", internal$link)
# The 404 page uses absolute paths under the GitHub Pages folder, e.g. /IK_Portfolio/about.html
rooted <- grepl("^/IK_Portfolio/", clean)
target <- ifelse(rooted,
                 file.path(site, sub("^/IK_Portfolio/(\\./)?", "", clean)),
                 file.path(dirname(file.path(site, internal$page)), clean))
target <- ifelse(grepl("/$", target), paste0(target, "index.html"), target)
internal$ok <- file.exists(target)

# External links: does the URL answer? (checked once each)
external <- unique(links$link[grepl("^https?://", links$link)])
status <- vapply(external, function(u) {
  h <- new_handle(nobody = FALSE, followlocation = TRUE, timeout = 30,
                  useragent = "Mozilla/5.0 (portfolio link check)")
  tryCatch(curl_fetch_memory(u, handle = h)$status_code, error = function(e) NA_integer_)
}, integer(1))

# The site's own absolute URLs (e.g. the social-card image) only exist once published
own_site <- grepl("^https://ishitak22.github.io/IK_Portfolio/", external)
bad_ext <- external[(is.na(status) | status >= 400) & !own_site]
bad_int <- internal[!internal$ok, ]

cat(sprintf("Checked %d pages: %d internal links, %d external URLs\n",
            length(pages), nrow(internal), length(external)))
if (nrow(bad_int)) { cat("\nBroken internal links:\n"); print(bad_int[, c("page", "link")], row.names = FALSE) }
if (length(bad_ext)) {
  cat("\nExternal URLs that did not answer OK:\n")
  for (u in bad_ext) {
    note <- if (identical(status[[u]], 999L)) "  (LinkedIn blocks automated checks; open it in a browser to confirm)" else ""
    cat(sprintf("  [%s] %s%s\n", status[[u]], u, note))
  }
}
if (!nrow(bad_int) && !length(bad_ext)) cat("No broken links found.\n")
