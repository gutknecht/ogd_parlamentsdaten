d <-
  googlesheets4::read_sheet(ss = "1cHF9qOHYoOcumw03d29mb90T_Elc8aHQrMCEsML88lk") |>
  dplyr::filter(nchar(Parlament) == 2L) |>
  dplyr::select(-c(starts_with("OGD "),
                   ends_with(" - Link"),
                   Kommentar,
                   `Kontakt Parlamentsdienste`,
                   `Technische Umsetzung Webseite`)) |>
  tidyr::pivot_longer(cols = -Parlament) |>
  dplyr::mutate(value_num = dplyr::case_match(value,
                                              "REST-API/RSS"    ~ 5L,
                                              "CSV/Excel"       ~ 4L,
                                              "HTML"            ~ 3L,
                                              "PDF"             ~ 2L,
                                              "Nicht Vorhanden" ~ 1L))
heatmap <-
  d |>
  dplyr::mutate("Open Data Score" = value_num) |>
  plotly::plot_ly(type = "heatmap",
                  x = ~name,
                  y = ~Parlament,
                  z = ~`Open Data Score`,
                  colors = viridis::inferno(n = 5L)) |>
  plotly::layout(xaxis = list(title = list(text = NULL)),
                 yaxis = list(title = list(text = NULL)))

barchart <-
  d |>
  plotly::plot_ly(type = 'bar',
                  orientation = 'h',
                  y = ~name,
                  colors = viridis::inferno(n = 5L),
                  textposition = "inside")
