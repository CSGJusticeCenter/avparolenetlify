# https://csgjc-research-knowledge.netlify.app/r-posts/2023-03-22-highcharts-image/highcharts-image

render_image <- JS("
  function() {
    this.renderer.image('https://csg-state-violent-crime.netlify.app/img/csgjc-logo.png',
                        30, this.chartHeight - 37, 140.1, 30)
    .add();
  }")

hc <- hchart(mtcars, "point", hcaes(x = disp, y = mpg, group = cyl))

hc |>
  hc_chart(events = list(render = render_image))




hc |>
  hc_exporting(
    enabled = TRUE,
    chartOptions = list(
      chart = list(
        backgroundColor = "#FFFFFF",  # white is better than default transparent
        events = list(
          load = render_image
        )
      )
    )
  ) |>
  hc_add_dependency(name = "modules/exporting.js")
