library(shiny)
library(ggplot2)
library(shinyjs)

set.seed(666)
dat <- data.frame(
  x = rnorm(10),
  y = rnorm(10),
  f = gl(2, 5, labels = c("A", "B"))
)

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
    classes: 'myqtip'
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
        background-color: rgba(245, 245, 245, 0.8);
        border-color: rgb(54, 57, 64);
      }")
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
    ggplot(dat, aes(x = x, y = y)) +
      geom_point(size = 6) +
      theme(
        panel.background = element_rect(fill = "#FFCF9E"),
        axis.title = element_text(size = 19),
        axis.text = element_text(size = 16)
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
    tooltip <- paste0(
      "<b> x: </b>", formatC(point[["x"]]), "<br/>",
      "<b> y: </b>", formatC(point[["y"]]), "<br/>",
      "<b> f: </b>", as.character(point[["f"]])
    )
    runjs(
      paste0(
        sprintf(
          "$('#tooltiptext').html('%s');", tooltip
        ),
        sprintf(qTipTemplate, pos, pos, left_px, top_px)
      )
    )
  })
}

shinyApp(ui, server)
