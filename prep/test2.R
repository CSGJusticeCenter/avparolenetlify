
df <- parole_eligibility_rate_by_admtype %>%
  filter(rptyear == 2020 &
           state == "Georgia" &
           parelig_status == "Current") %>%
  mutate(prop_label = paste0(round(prop, 0), "%"))

df_pct <- df1 %>%
  filter(admtype == "Parole return/revocation")

highchart() %>%

  hc_add_series(type = "pie",
                data = df_pct,
                hcaes(admtype, prop),
                size = "100%",
                center = c(50, 50),
                innerSize="60%",
                dataLabels = list(
                  style = list(fontSize = "7em",
                               color = orange),
                  enabled = TRUE,
                  distance= -260,
                  format = "{point.prop_label}")) %>%
  hc_add_series(type = "pie",
                data = df,
                hcaes(admtype, prop),
                size = "100%",
                center = c(50, 50),
                innerSize="60%",
                dataLabels = list(enabled = FALSE)) %>%

  hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
  hc_add_theme(hc_theme_jc) %>%
  hc_plotOptions(innersize = "50%",
                 startAngle = 90,
                 endAngle = 90,
                 center = list('50%', '75%'),
                 size = '110%',
                 series = list(animation = FALSE,
                               cursor = "pointer",
                               borderWidth = 3),
                 accessibility = list(enabled = TRUE,
                                      keyboardNavigation = list(enabled = TRUE),
                                      linkedDescription = "TBD",
                                      landmarkVerbosity = "one"),
                 area = list(accessibility = list(description = "TBD")))
