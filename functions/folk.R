folk = function(output = c('ggplot','plotly'), 
                language = c('en','es')) {
  
  # library(ggplot2)
  # library(ggtern)
  library(magrittr)
  
  # load('data/all_tibbles.rdata')
  
  tb.Folk = tibble::tribble(
    ~Q,    ~F,    ~R,                    ~Label,                 ~Label.es,
    100,     0,     0,           "Quartzarenite",          "Quarzo arenita",
    95,     5,     0,           "Quartzarenite",          "Quarzo arenita",
    95,   2.5,   2.5,           "Quartzarenite",          "Quarzo arenita",
    95,     0,     5,           "Quartzarenite",          "Quarzo arenita",
    95,     5,     0,               "Subarkose",               "Subarcosa",
    75,    25,     0,               "Subarkose",               "Subarcosa",
    75,  12.5,  12.5,               "Subarkose",               "Subarcosa",
    95,   2.5,   2.5,               "Subarkose",               "Subarcosa",
    95,   2.5,   2.5,          "Sublitharenite",           "Sublitarenita",
    75,  12.5,  12.5,          "Sublitharenite",           "Sublitarenita",
    75,     0,    25,          "Sublitharenite",           "Sublitarenita",
    95,     0,     5,          "Sublitharenite",           "Sublitarenita",
    75,    25,     0,                  "Arkose",                  "Arcosa",
    0,   100,     0,                  "Arkose",                  "Arcosa",
    0,    75,    25,                  "Arkose",                  "Arcosa",
    75, 18.75,  6.25,                  "Arkose",                  "Arcosa",
    75, 18.75,  6.25,           "Lithik Arkose",           "Arcosa lítica",
    0,    75,    25,           "Lithik Arkose",           "Arcosa lítica",
    0,    50,    50,           "Lithik Arkose",           "Arcosa lítica",
    75,  12.5,  12.5,           "Lithik Arkose",           "Arcosa lítica",
    75,  12.5,  12.5, "Feldspathic Litharenite", "Litarenita feldespática",
    0,    50,    50, "Feldspathic Litharenite", "Litarenita feldespática",
    0,    27,    75, "Feldspathic Litharenite", "Litarenita feldespática",
    75,  6.25, 18.75, "Feldspathic Litharenite", "Litarenita feldespática",
    75,  6.25, 18.75,             "Litharenite",              "Litarenita",
    0,    27,    75,             "Litharenite",              "Litarenita",
    0,     0,   100,             "Litharenite",              "Litarenita",
    75,     0,    25,             "Litharenite",              "Litarenita"
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
      font = list(family = "serif", size = 15, color = "white"),
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
    caxis = axis("R")
  )
  
  Folk.pal = c("#777777", "#AACDC9", "#C0CF98", 
               "#2F4996", "#D3C6E2", "#EBD1C0", "#564E37")
  
  if (any(output == 'ggplot' & language == 'en')) {
    Folk <- ggtern::ggtern(data=tb.Folk,ggtern::aes(F,Q,R)) +
      ggplot2::geom_polygon(ggplot2::aes(fill=Label,group=Label),
                            color="black",alpha=0.5) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = Folk.pal) +
      ggplot2::labs(title="Folk",
                    fill = "Sandstone",
                    T="Q",
                    L="F",
                    R="R")
  } else if (any(output == 'ggplot' & language == 'es')) {
    Folk <- ggtern::ggtern(data=tb.Folk,ggtern::aes(F,Q,R)) +
      ggplot2::geom_polygon(ggplot2::aes(fill=Label.es,group=Label.es),
                            color="black",alpha=0.5) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = Folk.pal) +
      ggplot2::labs(title="Folk",
                    fill = "Arenisca",
                    T="Q",
                    L="F",
                    R="R")
  } else if (any(output == 'plotly' & language == 'en')) {
    Folk = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.Folk,
        a = ~Q, b = ~F, c = ~R, 
        color = ~Label, 
        colors = Folk.pal %>% purrr::set_names(levels(tb.Folk$Label)),
        type = "scatterternary",
        fill = "toself", 
        mode = "lines",
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        annotations = label("Folk"), ternary = Folk.ternaryAxes,
        legend = list(title=list(text='<b> Sandstone </b>'))
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
        a = ~Q, b = ~F, c = ~R, 
        color = ~Label.es, 
        colors = Folk.pal %>% purrr::set_names(levels(tb.Folk$Label.es)),
        type = "scatterternary",
        fill = "toself", 
        mode = "lines",
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        annotations = label("Folk"), ternary = Folk.ternaryAxes,
        legend = list(title=list(text='<b> Arenisca </b>'))
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
