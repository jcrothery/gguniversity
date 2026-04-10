#' theme_exeter()
#'
#' @return ggplot theme based on University of Exeter branding
#' @export
#'
#' @examples
#' ggplot(iris, aes(x = Petal.Length, fill = Species)) +
#'  geom_density(alpha = 0.8)+
#'  scale_fill_manual(values=exeter_palette(3))+
#'  labs(title="Sepal length vs width", subtitle = "University of Exeter")+
#'  theme_exeter()
#'
#'
theme_exeter <- function() {

  PrimaryColor <- "#003c3c"
  SecondaryColor <- "#00dca5"
  HighlightColor <- "#702082"

  if(!require(showtext)){
    stop("Package 'showtext' not installed")
  }

  font_add_google("Outfit", family="Exeter")
  showtext_auto()



    # Update default geoms

    ggplot2::update_geom_defaults("area", list(colour = PrimaryColor, fill=SecondaryColor))
    ggplot2::update_geom_defaults("abline", list(colour = PrimaryColor))
    ggplot2::update_geom_defaults("bar", list(colour = PrimaryColor, fill=PrimaryColor))
    ggplot2::update_geom_defaults("boxplot", list(colour = PrimaryColor, fill=PrimaryColor))
    ggplot2::update_geom_defaults("col", list(colour = PrimaryColor, fill=PrimaryColor))
    ggplot2::update_geom_defaults("density", list(colour = PrimaryColor, fill=PrimaryColor))
    ggplot2::update_geom_defaults("hex", list(colour = PrimaryColor, fill=PrimaryColor))
    ggplot2::update_geom_defaults("hline", list(colour = PrimaryColor))
    ggplot2::update_geom_defaults("line", list(colour = PrimaryColor))
    ggplot2::update_geom_defaults("path", list(colour = PrimaryColor))
    ggplot2::update_geom_defaults("point", list(colour = SecondaryColor))
    ggplot2::update_geom_defaults("rect", list(colour = SecondaryColor, fill=SecondaryColor))
    ggplot2::update_geom_defaults("smooth", list(colour = PrimaryColor))
    ggplot2::update_geom_defaults("text", list(colour = PrimaryColor))
    ggplot2::update_geom_defaults("vline", list(colour = PrimaryColor))
    ggplot2::update_geom_defaults("violin", list(colour = PrimaryColor, fill=PrimaryColor))

    # Customise plot
    theme <- theme_classic()+
                theme(text = element_text(family = "Exeter", size = 14),
                  plot.title = element_text(family = "Exeter", size = 24,
                                            color = "black", hjust = 0),
                  plot.subtitle = element_text(family = "Exeter", size = 18,
                                               color = "black", hjust = 0),
                  plot.background = element_blank()
                  )

    # Customise axes
    theme <- theme +
                theme(
                  axis.title = element_text(family = "Exeter", size = 16,
                                            color = "black"),
                  axis.text = element_text(family = "Exeter", size = 16,
                                            color = "black"),
                  axis.line.x.bottom =element_line(color = "black",
                                                  linewidth = 0.75),
                  axis.line.y.left = element_line(color = "black",
                                                  linewidth = 0.75),
                  axis.ticks = element_line(color = "black")
                  )

    # Customise panel
    theme <- theme +
                theme(
                  panel.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank()
                )

  }


#' exeter_palette
#'
#' @param n number of colours to generate in palette
#'
#' @return vector of colour
#' @export
#'
#'
exeter_palette <- function(n){

  if (missing(n)){
    n <- 5
  }

  colours <- c("#003c3c", "#007d69", "#00a87e","#00c896", "#00dca5")
  pal <- grDevices::colorRampPalette(colours)(n)

  return(pal)

}

#' exeter_palette2
#'
#' @param n number of colours to generate in palette
#'
#' @return vector of colour
#' @export
#'
#'
exeter_palette2 <- function(n){

  if (missing(n)){
    n <- 8
  }

  colours <- c("#93272c", "#b46a55", "#e60000", "#f9423a", "#702082", "#9569be", "#fc4c02", "#ff7f41")
  pal <- grDevices::colorRampPalette(colours)(n)

  return(pal)

}

#' exeter_palette3
#'
#' @param n number of colours to generate in palette
#'
#' @return vector of colour
#' @export
#'
#'
exeter_palette3 <- function(n){

  if (missing(n)){
    n <- 8
  }

  colours <- c("#ffc72c", "#f3d54e", "#250e62", "#69b3e7", "#e68699", "#f4c3cc", "#000000", "#888b8d")
  pal <- grDevices::colorRampPalette(colours)(n)

  return(pal)

}

#' exeter_palette_full
#'
#' @param n number of colours to generate in palette
#'
#' @return vector of colour
#' @export
#'
#'
exeter_palette_full <- function(n){

  if (missing(n)){
    n <-21
  }

  colours <- c("#003c3c", "#007d69", "#00a87e","#00c896", "#00dca5",
  "#93272c", "#b46a55", "#e60000", "#f9423a", "#702082", "#9569be", "#fc4c02", "#ff7f41",
  "#ffc72c", "#f3d54e", "#250e62", "#69b3e7", "#e68699", "#f4c3cc", "#000000", "#888b8d")
  pal <- grDevices::colorRampPalette(colours)(n)

  return(pal)

}


exeter_logo <- function(plot){

  path <- "~/Documents/gguniversity/figures/exeter.jpg"
  img <- readJPEG(path, native = TRUE)
  img <- rasterGrob(img, interpolate=TRUE)

  plt+
  annotation_custom(img, xmin=20, xmax=25,ymin=.5,ymax=1)
}
