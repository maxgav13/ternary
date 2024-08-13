ternary_folk_qfm = function(output = c('ggplot','plotly'), 
                            language = c('en','es'),
                            opacity = .5) {

  library(magrittr)
  
  tb.Folk = tibble::tribble(
    ~Q,    ~F,    ~M,                    ~Label,                 ~Label.es,
    100,     0,     0,           "Quartzarenite",          "Quarzo arenita",
    95,     5,     0,           "Quartzarenite",          "Quarzo arenita",
    91.6,   4.2,     4.2,           "Quartzarenite",          "Quarzo arenita",
    95,     0,     5,           "Quartzarenite",          "Quarzo arenita",
    100,     0,     0,           "Quartzarenite",          "Quarzo arenita",
    95,     5,     0,               "Subarkose",               "Subarcosa",
    75,    25,     0,               "Subarkose",               "Subarcosa",
    66,  24,  10,               "Subarkose",               "Subarcosa",
    80,   10,   10,               "Subarkose",               "Subarcosa",
    91.6,   4.2,     4.2,           "Subarkose",               "Subarcosa",
    95,     5,     0,               "Subarkose",               "Subarcosa",
    95,     0,     5,          "Subgraywacke",           "Subgrauvaca",
    75,     0,    25,          "Subgraywacke",           "Subgrauvaca",
    66,     10,    24,          "Subgraywacke",           "Subgrauvaca",
    80,     10,     10,          "Subgraywacke",           "Subgrauvaca",
    91.6,   4.2,     4.2,          "Subgraywacke",           "Subgrauvaca",
    95,     0,     5,          "Subgraywacke",           "Subgrauvaca",
    66,  24,  10,          "Feldspathic subgraywacke",           "Subgrauvaca feldespática",
    80,   10,   10,          "Feldspathic subgraywacke",           "Subgrauvaca feldespática",
    66,     10,    24,          "Feldspathic subgraywacke",           "Subgrauvaca feldespática",
    52.5,   23.75,  23.75,          "Feldspathic subgraywacke",           "Subgrauvaca feldespática",
    66,  24,  10,          "Feldspathic subgraywacke",           "Subgrauvaca feldespática",
    75,    25,     0,                  "Arkose",                  "Arcosa",
    0,   100,     0,                  "Arkose",                  "Arcosa",
    0,    90,    10,                  "Arkose",                  "Arcosa",
    66,  24,  10,                  "Arkose",                  "Arcosa",
    75,    25,     0,                  "Arkose",                  "Arcosa",
    0,    90,    10,           "Impure Arkose",           "Arcosa impura",
    66,    24,    10,           "Impure Arkose",           "Arcosa impura",
    52.5,   23.75,  23.75,           "Impure Arkose",           "Arcosa impura",
    0,  50,  50,           "Impure Arkose",           "Arcosa impura",
    0,    90,    10,           "Impure Arkose",           "Arcosa impura",
    52.5,   23.75,  23.75, "Feldspathic graywacke", "Grauvaca feldespática",
    66,     10,    24, "Feldspathic graywacke", "Grauvaca feldespática",
    0,    10,    90, "Feldspathic graywacke", "Grauvaca feldespática",
    0,  50,  50, "Feldspathic graywacke", "Grauvaca feldespática",
    52.5,   23.75,  23.75, "Feldspathic graywacke", "Grauvaca feldespática",
    66,     10,    24,             "Graywacke",              "Grauvaca",
    75,    0,    25,             "Graywacke",              "Grauvaca",
    0,     0,   100,             "Graywacke",              "Grauvaca",
    0,    10,    90,             "Graywacke",              "Grauvaca",
    66,     10,    24,             "Graywacke",              "Grauvaca"
  ) %>% 
    dplyr::mutate(dplyr::across(Label:Label.es,forcats::as_factor))
  
  
  # reusable function for creating annotation object
  label <- function(txt) {
    list(
      text = txt, 
      x = 0.1, y = 1,
      ax = 0, ay = 0,
      xref = "paper", yref = "paper", 
      align = "center",
      font = list(size = 15, color = "white"),
      bgcolor = "#b3b3b3", bordercolor = "black", borderwidth = 2
    )
  }
  
  # reusable function for axis formatting
  axis <- function(txt) {
    list(
      title = txt, tickformat = ".0%", tickfont = list(size = 10)
    )
  }
  
  Folk.ternaryAxes <- list(
    aaxis = axis("Q"), 
    baxis = axis("F"), 
    caxis = axis("M")
  )
  
  Folk.pal = c("#777777", "#AACDC9", "#C0CF98", "#B2CEAC",
               "#2F4996", "#D3C6E2", "#EBD1C0", "#564E37")
  
  if (any(output == 'ggplot' & language == 'en')) {
    Folk <- ggtern::ggtern(data=tb.Folk,ggtern::aes(F,Q,M)) +
      ggplot2::geom_polygon(ggplot2::aes(fill=Label,color=Label,
                                         group=Label),
                            alpha=opacity) +
      ggtern::theme_bw() + 
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = Folk.pal) +
      ggplot2::scale_color_manual(values = Folk.pal) +
      ggplot2::labs(fill = "Sedimentary rock",
                    color = "Sedimentary rock",
                    T="Q",
                    L="F",
                    R="M")
  } else if (any(output == 'ggplot' & language == 'es')) {
    Folk <- ggtern::ggtern(data=tb.Folk,ggtern::aes(F,Q,M)) +
      ggplot2::geom_polygon(ggplot2::aes(fill=Label.es,color=Label.es,
                                         group=Label.es),
                            alpha=opacity) +
      ggtern::theme_bw() + 
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = Folk.pal) +
      ggplot2::scale_color_manual(values = Folk.pal) +
      ggplot2::labs(fill = "Roca sedimentaria",
                    color = "Roca sedimentaria",
                    T="Q",
                    L="F",
                    R="M")
  } else if (any(output == 'plotly' & language == 'en')) {
    Folk = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.Folk,
        a = ~Q, b = ~F, c = ~M, 
        color = ~Label, 
        colors = Folk.pal %>% purrr::set_names(levels(tb.Folk$Label)),
        opacity = opacity*2,
        type = "scatterternary",
        fill = "toself", 
        mode = "lines",
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        ternary = Folk.ternaryAxes,
        legend = list(title=list(text='<b> Sedimentary rock </b>')),
        margin = list(autoexpand=T,t=35)
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Folk',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(output == 'plotly' & language == 'es')) {
    Folk = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.Folk,
        a = ~Q, b = ~F, c = ~M, 
        color = ~Label.es, 
        colors = Folk.pal %>% purrr::set_names(levels(tb.Folk$Label.es)),
        opacity = opacity*2,
        type = "scatterternary",
        fill = "toself", 
        mode = "lines",
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        ternary = Folk.ternaryAxes,
        legend = list(title=list(text='<b> Roca sedimentaria </b>')),
        margin = list(autoexpand=T,t=35)
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Folk',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }
  
  return(Folk)
  
}
