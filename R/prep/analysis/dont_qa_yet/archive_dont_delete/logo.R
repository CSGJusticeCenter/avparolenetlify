# CSG logo
render_image <- JS("
  function(){
    this.renderer.image('https://csg-state-violent-crime.netlify.app/img/csgjc-logo.png', 30, this.chartHeight - 37, 140.1, 30)
    .add();
  }")


hc_chart(type="area",
         events = list(render = render_image),
         marginBottom = 80,
         marginRight = 30)


save_state_png <- function(hc_obj, folderpath, id, title){

  admin$mylog(glue("Save plot: {title} for {id}"))
  saveWidget(hc_obj, file = "temp.html", selfcontained = TRUE)
  webshot2::webshot(
    url = "temp.html"
    , file = file.path(folderpath, glue("{id}_{title}.png"))
    , zoom = 4
    , vwidth = 500
    , vheight = 500
    , delay = 1
  )

}


admin$mylog("PRISON ADMISSIONS")
walk(
  states_list,
  ~save_state_png(
    add_st_name(all_state_area_adm[[.x]], .x)
    , folderpath = savefolder
    , id = .x
    , title = "Prison_Admissions")
)
