library(shiny)
library(shinyjs)
library(ggplot2)
library(viridisLite)

set.seed(666L)
n <- 200L
dat <- data.frame(
  x = 1:n,
  y = runif(n)
)

# font color given background color:
black_or_white <- function(color) {
  rgb <- col2rgb(color)
  if(rgb[1]*0.299 + rgb[2]*0.587 + rgb[3]*0.114 > 186)
    "black" # background color is light
  else
    "white" # background color is dark
}

ramp <- colorRamp(viridis(255L))

bkgs    <- rgb(ramp(dat$y), maxColorValue = 255)
colors  <- vapply(bkgs, black_or_white, character(1L))
classes <- sprintf("color%03d", 1:n)
dat$class <- classes

CSSclass <- function(i) {
  paste0(
    ".", classes[i], " {",
    "color: ", colors[i], "; ",
    "background-color: ", bkgs[i], ";",
    "}"
  )
}
CSS <- paste0(vapply(1:n, CSSclass, character(1L)), collapse = "\n")

qTipTemplate <- "
$('#hoverinfo').qtip({
  overwrite: true,
  content: {
    text: $('#tooltiptext').clone()
  },
  position: {
    my: '%s',
    at: '%s',
    target: [%s, %s],
    container: $('#ggplot')
  },
  show: {
    ready: true
  },
  hide: {
    event: false,
    inactive: 4000
  },
  style: {
    classes: 'myqtip %s'
  }
});
"

ui <- basicPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", href = "jquery.qtip.css"),
    tags$script(src = "jquery.qtip.js"),
    tags$style("
      .myqtip {
        font-size: 15px;
        line-height: 18px;
        color: white;
        border-color: rgb(54, 57, 64);
      }
    "),
    tags$style(CSS)
  ),
  div(
    id = "tooltiptext", style = "display: none;"
  ),
  br(),
  div(
    style = "position: relative;",
    plotOutput("ggplot", hover = hoverOpts("plot_hover")),
    div(id = "hoverinfo", style = "position: absolute;")
  )
)

server <- function(input, output, session) {
  output$ggplot <- renderPlot(
    ggplot(dat, aes(x = x, y = y, color = y)) + geom_point(size = 3) +
      scale_color_viridis_c() +
      theme(
        panel.background = element_rect(fill = "gainsboro"),
        axis.title = element_text(size = 19),
        axis.text = element_text(size = 16),
        legend.title = element_text(size = 18)
      )
  )

  observeEvent(input[["plot_hover"]], {
    if(is.null(hover <- input[["plot_hover"]])) {
      return(NULL)
    }
    point <- nearPoints(dat, hover, threshold = 15, maxpoints = 1L)
    if(nrow(point) == 0L) {
      return(NULL)
    }
    left_pct <-
      (point[["x"]] - hover$domain$left) /
        (hover$domain$right - hover$domain$left)
    top_pct <-
      (hover$domain$top - point[["y"]]) /
        (hover$domain$top - hover$domain$bottom)
    left_px <-
      (hover$range$left + left_pct * (hover$range$right - hover$range$left)) /
        hover$img_css_ratio$x
    top_px <-
      (hover$range$top + top_pct * (hover$range$bottom - hover$range$top)) /
        hover$img_css_ratio$y
    pos <- ifelse(left_pct < 0.5,
      ifelse(top_pct < 0.5,
        "top left",
        "bottom left"
      ),
      ifelse(top_pct < 0.5,
        "top right",
        "bottom right"
      )
    )
    cssClass <- point[["class"]]
    tooltip <- paste0(
      "<b> x: </b>", formatC(point[["x"]]), "<br/>",
      "<b> y: </b>", formatC(point[["y"]]), "<br/>"
    )
    runjs(
      paste0(
        sprintf(
          "$('#tooltiptext').html('%s');", tooltip
        ),
        sprintf(qTipTemplate, pos, pos, left_px, top_px, cssClass)
      )
    )
  })
}

shinyApp(ui, server)
