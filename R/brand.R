# Builds the site's brand images with R (run from the project root after changes):
#   files/monogram.svg      "IK" monogram used on the About page
#   files/favicon.png       browser-tab icon
#   files/social-card.png   preview image when the site is shared (LinkedIn etc.)
#
#   Rscript R/brand.R

source("R/home.R")
library(magick)

indigo <- "#4f46e5"
paper  <- "#fbfaf7"
ink    <- "#14171f"
muted  <- "#6b7080"

# Draws the monogram (indigo circle, white "IK") into the current device
draw_monogram <- function() {
  par(mar = c(0, 0, 0, 0))
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)
  theta <- seq(0, 2 * pi, length.out = 200)
  polygon(cos(theta) * 0.97, sin(theta) * 0.97, col = indigo, border = NA)
  text(0, -0.02, "IK", col = "white", font = 2, family = "serif", cex = par("pin")[1] * 1.6)
}

# --- Monogram (SVG for the web page) --------------------------------------------
svg("files/monogram.svg", width = 2, height = 2, bg = "transparent")
draw_monogram()
invisible(dev.off())

# --- Favicon ------------------------------------------------------------------
png("files/favicon.png", width = 256, height = 256, bg = "transparent", type = "cairo", res = 128)
draw_monogram()
invisible(dev.off())

# --- Social preview card (1200 x 630) ------------------------------------------
tmp_net <- tempfile(fileext = ".png")
png(tmp_net, width = 900, height = 700, bg = "transparent", type = "cairo", res = 110)
print(project_network_plot(read_projects()))
invisible(dev.off())

tmp_mono <- tempfile(fileext = ".png")
png(tmp_mono, width = 192, height = 192, bg = "transparent", type = "cairo", res = 96)
draw_monogram()
invisible(dev.off())

net <- image_read(tmp_net) |> image_fx(expression = "a*0.45", channel = "alpha")
card <- image_blank(1200, 630, color = paper) |>
  image_composite(net, offset = "+590-40") |>
  image_composite(image_scale(image_read(tmp_mono), "96x96"), offset = "+80+80") |>
  image_annotate("DATA ANALYST  ·  INSIGHTS & REPORTING  ·  MELBOURNE",
                 location = "+82+230", size = 22, color = indigo, font = "Segoe UI", weight = 600) |>
  image_annotate("Ishita Khanna", location = "+78+262", size = 84, color = ink, font = "Georgia", weight = 600) |>
  image_annotate("Messy data in. Decisions out.", location = "+82+380", size = 38, color = ink, font = "Georgia") |>
  image_annotate("Dashboards, analysis and projects in R, SQL, Tableau and Power BI",
                 location = "+82+450", size = 26, color = muted, font = "Segoe UI") |>
  image_annotate("ishitak22.github.io/IK_Portfolio", location = "+82+540", size = 24, color = indigo,
                 font = "Segoe UI", weight = 600)

image_write(card, "files/social-card.png", format = "png")
message("Wrote files/monogram.svg, files/favicon.png, files/social-card.png")
