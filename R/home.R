# Builds the data-driven parts of the home page (index.qmd):
#   - a network graphic of portfolio projects linked by shared skills
#   - the impact numbers and credential badges
#   - the "my toolkit vs the market" heatmap
# Everything is recomputed on each `quarto render`.

suppressPackageStartupMessages({
  library(yaml)
  library(readr)
  library(dplyr)
  library(ggplot2)
})

esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

# ---- Portfolio projects -------------------------------------------------------

# Front matter of every project page under projects/
read_projects <- function(dir = "projects") {
  files <- list.files(dir, pattern = "^index\\.qmd$", recursive = TRUE, full.names = TRUE)
  lapply(files, function(f) {
    m <- rmarkdown::yaml_front_matter(f)
    cats <- tolower(trimws(unlist(m$categories %||% character())))
    list(
      folder = basename(dirname(f)),
      title = m$title %||% basename(dirname(f)),
      description = m$description %||% "",
      image = m$image %||% "",
      date = as.Date(m$date %||% NA, tryFormats = c("%Y-%m-%d", "%m-%d-%Y")),
      categories = cats,
      text = tolower(paste(c(m$title, m$description, cats), collapse = ", "))
    )
  })
}

# "More projects" cards at the bottom of a project page: the projects sharing
# the most tags with this one (Jaccard similarity), newest first on ties.
# Run from inside projects/<folder>/, which is where knitr renders the page.
related_projects_html <- function(n = 3, ignore = c("r")) {
  here <- basename(getwd())
  projects <- read_projects("..")
  me <- Filter(function(p) p$folder == here, projects)[[1]]
  others <- Filter(function(p) p$folder != here, projects)

  mine <- setdiff(me$categories, ignore)
  score <- vapply(others, function(p) {
    theirs <- setdiff(p$categories, ignore)
    length(intersect(mine, theirs)) / max(length(union(mine, theirs)), 1)
  }, numeric(1))
  dates <- vapply(others, function(p) as.numeric(p$date), numeric(1))
  picks <- others[order(-score, -dates)][seq_len(min(n, length(others)))]

  cards <- vapply(picks, function(p) {
    href <- sprintf("../%s/index.html", p$folder)
    img <- if (nzchar(p$image)) sprintf(
      '<div class="card-img-top"><img loading="lazy" src="../%s/%s" alt="" class="card-img" style="height: 170px;"></div>',
      p$folder, p$image) else ""
    sprintf(
      '<a class="card quarto-grid-item related-card" href="%s">%s<div class="card-body"><h5 class="card-title listing-title">%s</h5><p class="card-text">%s</p></div></a>',
      href, img, esc(p$title), esc(p$description))
  }, "")

  paste0('<section class="related-projects"><h2 class="anchored"><i class="bi bi-collection"></i>More projects</h2>',
         '<div class="related-grid">', paste(cards, collapse = ""), "</div></section>")
}

# One-line summary for the top of the Projects page, from the project tags
projects_summary_html <- function(projects,
                                  industries = c("finance", "healthcare", "retail", "science & research"),
                                  tools = c(r = "R", shiny = "Shiny", "d3.js" = "D3.js")) {
  tags <- unlist(lapply(projects, `[[`, "categories"))
  n_ind <- sum(industries %in% tags)
  used <- unname(tools[names(tools) %in% tags])
  tool_txt <- if (length(used) > 1) {
    paste(paste(head(used, -1), collapse = ", "), "and", tail(used, 1))
  } else used
  sprintf('<p class="projects-summary"><strong>%d projects</strong> across <strong>%d industries</strong>, built in %s.</p>',
          length(projects), n_ind, esc(tool_txt))
}

# ---- Hero background: projects linked through the skills they share ----------

# Fruchterman-Reingold force layout (small graphs, no extra packages)
fr_layout <- function(n, edges, iter = 400, seed = 22) {
  set.seed(seed)
  pos <- matrix(runif(n * 2, -1, 1), n)
  k <- sqrt(4 / n)
  temp <- 0.25
  for (it in seq_len(iter)) {
    disp <- matrix(0, n, 2)
    for (v in seq_len(n)) {
      delta <- sweep(-pos, 2, pos[v, ], `+`)           # pos[v] - pos[u]
      dist <- pmax(sqrt(rowSums(delta^2)), 1e-3)
      push <- k^2 / dist
      push[v] <- 0
      disp[v, ] <- colSums(delta / dist * push)
    }
    for (e in seq_len(nrow(edges))) {
      a <- edges$from[e]; b <- edges$to[e]
      delta <- pos[a, ] - pos[b, ]
      dist <- max(sqrt(sum(delta^2)), 1e-3)
      pull <- (dist^2 / k) * delta / dist
      disp[a, ] <- disp[a, ] - pull
      disp[b, ] <- disp[b, ] + pull
    }
    len <- pmax(sqrt(rowSums(disp^2)), 1e-9)
    pos <- pos + disp / len * pmin(len, temp)
    temp <- temp * 0.985
  }
  pos
}

project_network_svg <- function(projects, out = "files/hero-network.svg") {
  p <- project_network_plot(projects)
  dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
  svg(out, width = 9, height = 7, bg = "transparent")
  print(p)
  invisible(dev.off())
  out
}

project_network_plot <- function(projects) {
  # Tags too generic to say anything about how projects relate
  generic <- c("r", "rstats", "analytics", "eda", "data analysis", "html",
               "data visualisation", "data visualization", "visualisation")
  tags <- lapply(projects, function(p) setdiff(p$categories, generic))
  skills <- sort(unique(unlist(tags)))

  n_proj <- length(projects)
  nodes <- data.frame(
    type = c(rep("project", n_proj), rep("skill", length(skills))),
    label = c(vapply(projects, `[[`, "", "title"), skills)
  )
  edges <- do.call(rbind, lapply(seq_len(n_proj), function(i) {
    if (!length(tags[[i]])) return(NULL)
    data.frame(from = i, to = n_proj + match(tags[[i]], skills))
  }))

  pos <- fr_layout(nrow(nodes), edges)
  nodes$x <- pos[, 1]; nodes$y <- pos[, 2]
  seg <- data.frame(x = pos[edges$from, 1], y = pos[edges$from, 2],
                    xend = pos[edges$to, 1], yend = pos[edges$to, 2])

  ggplot() +
    geom_segment(data = seg, aes(x, y, xend = xend, yend = yend),
                 colour = "#4f46e5", linewidth = 0.35, alpha = 0.55) +
    geom_point(data = nodes[nodes$type == "skill", ], aes(x, y),
               colour = "#4f46e5", size = 1.6, alpha = 0.7) +
    geom_point(data = nodes[nodes$type == "project", ], aes(x, y),
               colour = "#4f46e5", fill = "#8b93ff", shape = 21, size = 4.2, stroke = 0.8) +
    coord_equal() +
    theme_void() +
    theme(plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA))
}

# ---- Impact numbers -----------------------------------------------------------

projects_total <- function(stats, csv = "data/all_projects.csv") {
  if (identical(stats$projects_total, "auto") && file.exists(csv)) {
    n <- nrow(read_csv(csv, show_col_types = FALSE))
    list(value = n, suffix = "")
  } else {
    txt <- as.character(stats$projects_total)
    list(value = as.numeric(gsub("[^0-9]", "", txt)), suffix = gsub("[0-9]", "", txt))
  }
}

stat_html <- function(value, label, prefix = "", suffix = "") {
  shown <- paste0(prefix, format(value, big.mark = ",", scientific = FALSE), suffix)
  sprintf(
    '<div class="stat"><span class="stat-num" data-count="%s" data-prefix="%s" data-suffix="%s">%s</span><span class="stat-label">%s</span></div>',
    value, esc(prefix), esc(suffix), esc(shown), esc(label)
  )
}

impact_html <- function(path = "data/site_stats.yml") {
  s <- read_yaml(path)
  cards <- vapply(s$impact, function(i) {
    stat_html(i$value, i$label, i$prefix %||% "", i$suffix %||% "")
  }, "")
  pt <- projects_total(s)
  cards <- c(cards, stat_html(pt$value, s$projects_label, suffix = pt$suffix))
  badges <- paste0('<span class="cred">', esc(unlist(s$credentials)), "</span>", collapse = "")
  paste0('<div class="stats reveal">', paste(cards, collapse = ""), "</div>",
         '<div class="creds reveal">', badges, "</div>")
}

# ---- Toolkit vs market heatmap -----------------------------------------------

skills_table <- function(projects,
                         skills_csv = "data/skills.csv",
                         market_csv = "data/market_demand.csv",
                         top_n = 12) {
  skills <- read_csv(skills_csv, show_col_types = FALSE, na = "") |>
    mutate(across(c(work_evidence, project_pattern, extra_projects, certification),
                  ~ coalesce(.x, "")))
  market <- read_csv(market_csv, show_col_types = FALSE)

  # The most requested tools, plus any of mine the market data also covers
  keep <- union(head(arrange(market, desc(pct))$skill, top_n),
                intersect(skills$skill, market$skill))

  texts <- vapply(projects, `[[`, "", "text")
  titles <- vapply(projects, `[[`, "", "title")

  market |>
    filter(skill %in% keep) |>
    left_join(skills, by = "skill") |>
    rowwise() |>
    mutate(
      label = coalesce(label, if (nchar(skill) <= 3) toupper(skill) else tools::toTitleCase(skill)),
      in_toolkit = coalesce(in_toolkit, "no") == "yes",
      work_roles = coalesce(work_roles, 0),
      work_evidence = coalesce(work_evidence, ""),
      certification = coalesce(certification, ""),
      portfolio = list(if (nzchar(coalesce(project_pattern, "")))
        titles[grepl(project_pattern, texts, perl = TRUE)] else character()),
      extras = list(if (nzchar(coalesce(extra_projects, "")))
        trimws(strsplit(extra_projects, ";")[[1]]) else character()),
      n_projects = length(portfolio) + length(extras),
      status = case_when(
        work_roles > 0 || n_projects >= 2 ~ "Strength",
        n_projects == 1 || nzchar(certification) ~ "Building",
        in_toolkit ~ "In my toolkit",
        TRUE ~ "Next to learn"
      )
    ) |>
    ungroup() |>
    arrange(desc(pct))
}

cell <- function(v, text, tip) {
  cls <- if (v > 0.55) "hm-cell hm-strong" else "hm-cell"   # white text on dark cells
  sprintf('<td class="%s" style="--v:%.2f" tabindex="0" data-tip="%s"><span>%s</span></td>',
          cls, v, esc(tip), esc(text))
}

skills_heatmap_html <- function(projects,
                                meta_path = "data/market_demand_meta.yml",
                                top_n = 12) {
  meta <- read_yaml(meta_path)
  tbl <- skills_table(projects, top_n = top_n)
  max_pct <- max(tbl$pct)

  rows <- vapply(seq_len(nrow(tbl)), function(i) {
    r <- tbl[i, ]
    proj <- c(r$portfolio[[1]], r$extras[[1]])
    paste0(
      "<tr>",
      '<th scope="row">', esc(r$label), "</th>",
      cell(r$pct / max_pct, paste0(round(r$pct * 100), "%"),
           sprintf("Asked for in %d%% of %s postings", round(r$pct * 100), meta$role)),
      cell(min(r$work_roles, 2) / 2, if (r$work_roles > 0) r$work_roles else "",
           if (nzchar(r$work_evidence)) r$work_evidence else "Not yet used in a paid role"),
      cell(min(r$n_projects, 5) / 5, if (r$n_projects > 0) r$n_projects else "",
           if (length(proj)) paste(proj, collapse = " · ") else "No projects yet"),
      cell(as.numeric(nzchar(r$certification)), if (nzchar(r$certification)) "✓" else "",
           if (nzchar(r$certification)) r$certification else "No certification"),
      '<td class="hm-status" data-status="', esc(r$status), '">', esc(r$status), "</td>",
      "</tr>"
    )
  }, "")

  top <- head(tbl, top_n)
  used <- sum(top$status != "Next to learn")
  headline <- sprintf("I already use <strong>%d of the %d</strong> tools %s employers ask for most.",
                      used, nrow(top), esc(meta$role))

  source_note <- if (isTRUE(meta$is_sample)) {
    '<p class="hm-source hm-sample">Sample numbers for layout only. Real market data coming soon.</p>'
  } else {
    sprintf('<p class="hm-source">Employer demand: share of %s job postings in %s that mention each tool (%s postings, %s). Source: <a href="%s">%s</a>. Evidence from my work history and portfolio, computed in R.</p>',
            esc(meta$role), esc(meta$country), format(meta$n_postings, big.mark = ","),
            esc(meta$period), esc(meta$source_url), esc(meta$source))
  }

  paste0(
    '<p class="hm-headline">', headline, "</p>",
    '<div class="hm-wrap reveal"><table class="skill-heatmap">',
    "<thead><tr><th>Tool</th><th>Employer demand</th><th>Used at work</th>",
    "<th>In projects</th><th>Certified</th><th>Status</th></tr></thead>",
    "<tbody>", paste(rows, collapse = ""), "</tbody></table></div>",
    source_note
  )
}
