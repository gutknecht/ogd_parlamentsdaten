# prepare data
d <-
  googlesheets4::read_sheet(ss = "1cHF9qOHYoOcumw03d29mb90T_Elc8aHQrMCEsML88lk") |>
  dplyr::filter(nchar(Parlament) == 2L) |>
  dplyr::select(-c(starts_with("OGD "),
                   ends_with(" - Link"),
                   Kommentar,
                   `Leiter:in Parlamentsdienste`,
                   `Technische Umsetzung Webseite`)) |>
  tidyr::pivot_longer(cols = -Parlament) |>
  dplyr::mutate(value_num = dplyr::case_match(value,
                                              "REST-API/RSS"    ~ 5L,
                                              "CSV/Excel"       ~ 4L,
                                              "HTML"            ~ 3L,
                                              "PDF"             ~ 2L,
                                              "Nicht Vorhanden" ~ 1L),
                value = factor(value,
                               levels = c("Nicht Vorhanden",
                                          "PDF",
                                          "HTML",
                                          "CSV/Excel",
                                          "REST-API/RSS"),
                               ordered = TRUE))
# create charts
heatmap <-
  d |>
  dplyr::mutate("Open Data Score" = value_num) |>
  plotly::plot_ly(type = "heatmap",
                  x = ~name,
                  y = ~Parlament,
                  z = ~`Open Data Score`,
                  colors = viridis::inferno(n = 5L)) |>
  plotly::layout(xaxis = list(title = list(text = NULL)),
                 yaxis = list(title = list(text = NULL)),
                 legend = list(orientation = 'h')) |>
  plotly::config(displayModeBar = FALSE,
                 displaylogo = FALSE, 
                 showLink = FALSE,
                 cloud = FALSE,
                 scrollZoom = FALSE, 
                 doubleClick = "reset+autosize")
barchart <-
  d |>
  dplyr::filter(!is.na(value)) |>
  # dplyr::mutate(value = forcats::fct_explicit_na(value,
  #                                                na_level = "?")) |>
  dplyr::group_by(name) |>
  dplyr::count(value) |>
  dplyr::mutate(share = n / sum(n) * 100,
                cum_share = cumsum(share)) |>
  plotly::plot_ly(type = 'bar',
                  orientation = 'h',
                  x = ~share,
                  y = ~name,
                  color = ~value,
                  colors = viridis::inferno(n = 5L),
                  text = ~paste0("<b>", round(share, digits = 0), "\u202F%</b>"),
                  textposition = "inside",
                  hoverinfo = "y+text") |>
  # plotly::add_annotations(x = ~cum_share,
  #                         y = ~name,
  #                         text = ~paste0("<b>", round(share, digits = 0), "\u202F%</b>"),
  #                         align = "center",
  #                         showarrow = FALSE) |>
  plotly::layout(barmode = 'stack',
                 xaxis = list(title = list(text = NULL),
                              ticksuffix = "\u202F%"),
                 yaxis = list(title = list(text = NULL)),
                 legend = list(orientation = 'h')) |>
  plotly::config(displayModeBar = FALSE,
                 displaylogo = FALSE, 
                 showLink = FALSE,
                 cloud = FALSE,
                 scrollZoom = FALSE, 
                 doubleClick = "reset+autosize")

# export
## workaround, cf. https://stackoverflow.com/a/74446794/7196903
reticulate::py_run_string("import sys")

plotly::save_image(p = heatmap,
                   file = "heatmap.png",
                   height = 800,
                   width = 1000)

plotly::save_image(p = barchart,
                   file = "barchart.png",
                   height = 800,
                   width = 1000)

readr::write_rds(x = heatmap,
                 file = "heatmap.rds")

readr::write_rds(x = barchart,
                 file = "barchart.rds")
