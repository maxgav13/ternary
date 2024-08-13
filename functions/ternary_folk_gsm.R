ternary_folk_gsm = function(output = c('ggplot','plotly'), 
                            language = c('en','es'),
                            opacity = .5) {
  
  library(magrittr)
  
  tb.Folk = tibble::tribble(
    ~G,    ~S,    ~M,                    ~Label,                 ~Label.es,
    100,     0,     0,           "Conglomerate \n(Gravel)",            "Conglomerado \n(Grava)",
    80,     20,     0,           "Conglomerate \n(Gravel)",            "Conglomerado \n(Grava)",
    80,      0,    20,           "Conglomerate \n(Gravel)",            "Conglomerado \n(Grava)",
    100,     0,     0,           "Conglomerate \n(Gravel)",            "Conglomerado \n(Grava)",
    80,     20,     0,     "Sandy conglomerate \n(Sandy gravel)",    "Conglomerado arenoso \n(Grava arenosa)",
    80,     18,     2,     "Sandy conglomerate \n(Sandy gravel)",    "Conglomerado arenoso \n(Grava arenosa)",
    30,     63,     7,     "Sandy conglomerate \n(Sandy gravel)",    "Conglomerado arenoso \n(Grava arenosa)",
    30,     70,     0,     "Sandy conglomerate \n(Sandy gravel)",    "Conglomerado arenoso \n(Grava arenosa)",
    80,     20,     0,     "Sandy conglomerate \n(Sandy gravel)",    "Conglomerado arenoso \n(Grava arenosa)",
    80,     18,     2,"Muddy sandy conglomerate \n(Muddy sandy gravel)",           "Conglomerado arenoso lodoso \n(Grava arenosa lodosa)",
    80,     10,    10,          "Muddy sandy conglomerate \n(Muddy sandy gravel)",           "Conglomerado arenoso lodoso \n(Grava arenosa lodosa)",
    30,     35,    35,          "Muddy sandy conglomerate \n(Muddy sandy gravel)",           "Conglomerado arenoso lodoso \n(Grava arenosa lodosa)",
    30,     63,     7,          "Muddy sandy conglomerate \n(Muddy sandy gravel)",           "Conglomerado arenoso lodoso \n(Grava arenosa lodosa)",
    80,     18,     2,          "Muddy sandy conglomerate \n(Muddy sandy gravel)",           "Conglomerado arenoso lodoso \n(Grava arenosa lodosa)",
    80,     10,    10,       "Muddy conglomerate \n(Muddy gravel)",                  "Conglomerado lodoso \n(Grava lodosa)",
    80,   0,     20,                  "Muddy conglomerate \n(Muddy gravel)",                  "Conglomerado lodoso \n(Grava lodosa)",
    30,    0,    70,                  "Muddy conglomerate \n(Muddy gravel)",                  "Conglomerado lodoso \n(Grava lodosa)",
    30,     35,    35,                  "Muddy conglomerate \n(Muddy gravel)",                  "Conglomerado lodoso \n(Grava lodosa)",
    80,     10,    10,                  "Muddy conglomerate \n(Muddy gravel)",                  "Conglomerado lodoso \n(Grava lodosa)",
    30,     70,     0,           "Conglomeratic sandstone \n(Gravelly sand)",           "Arenisca conglomerática \n(Arena gravosa)",
    30,     63,     7,           "Conglomeratic sandstone \n(Gravelly sand)",           "Arenisca conglomerática \n(Arena gravosa)",
    5,    85.5,    9.5,           "Conglomeratic sandstone \n(Gravelly sand)",           "Arenisca conglomerática \n(Arena gravosa)",
    5,  95,  0,           "Conglomeratic sandstone \n(Gravelly sand)",           "Arenisca conglomerática \n(Arena gravosa)",
    30,     70,     0,           "Conglomeratic sandstone \n(Gravelly sand)",           "Arenisca conglomerática \n(Arena gravosa)",
    30,     63,     7, "Conglomeratic muddy sandstone \n(Gravelly muddy sand)", "Arenisca lodosa conglomerática \n(Arena lodosa gravosa)",
    30,     35,    35, "Conglomeratic muddy sandstone \n(Gravelly muddy sand)", "Arenisca lodosa conglomerática \n(Arena lodosa gravosa)",
    5,    47.5,    47.5, "Conglomeratic muddy sandstone \n(Gravelly muddy sand)", "Arenisca lodosa conglomerática \n(Arena lodosa gravosa)",
    5,    85.5,    9.5, "Conglomeratic muddy sandstone \n(Gravelly muddy sand)", "Arenisca lodosa conglomerática \n(Arena lodosa gravosa)",
    30,     63,     7, "Conglomeratic muddy sandstone \n(Gravelly muddy sand)", "Arenisca lodosa conglomerática \n(Arena lodosa gravosa)",
    30,     35,    35,             "Conglomeratic mudstone \n(Gravelly mud)",              "Lodolita conglomerática \n(Lodo gravoso)",
    30,    0,    70,             "Conglomeratic mudstone \n(Gravelly mud)",              "Lodolita conglomerática \n(Lodo gravoso)",
    5,     0,   95,             "Conglomeratic mudstone \n(Gravelly mud)",              "Lodolita conglomerática \n(Lodo gravoso)",
    5,    47.5,    47.5,             "Conglomeratic mudstone \n(Gravelly mud)",              "Lodolita conglomerática \n(Lodo gravoso)",
    30,     35,    35,             "Conglomeratic mudstone \n(Gravelly mud)",              "Lodolita conglomerática \n(Lodo gravoso)",
    5,    85.5,    9.5,    "Sandstone \n(Sand)",      "Arenisca \n(Arena)",
    0,    90,    10,    "Sandstone \n(Sand)",      "Arenisca \n(Arena)",
    0,    100,    0,    "Sandstone \n(Sand)",      "Arenisca \n(Arena)",
    5,    95,    0,    "Sandstone \n(Sand)",      "Arenisca \n(Arena)",
    5,    85.5,    9.5,    "Sandstone \n(Sand)",      "Arenisca \n(Arena)",
    5,    85.5,    9.5,    "Muddy sandstone \n(Muddy sand)",      "Arenisca lodosa \n(Arena lodosa)",
    5,    47.5,    47.5,    "Muddy sandstone \n(Muddy sand)",      "Arenisca lodosa \n(Arena lodosa)",
    0,    50,    50,    "Muddy sandstone \n(Muddy sand)",      "Arenisca lodosa \n(Arena lodosa)",
    0,    90,    10,    "Muddy sandstone \n(Muddy sand)",      "Arenisca lodosa \n(Arena lodosa)",
    5,    85.5,    9.5,    "Muddy sandstone \n(Muddy sand)",      "Arenisca lodosa \n(Arena lodosa)",
    5,    47.5,    47.5,    "Sandy mudstone \n(Sandy mud)",      "Lodolita arenosa \n(Lodo arenoso)",
    5,    9.5,    85.5,    "Sandy mudstone \n(Sandy mud)",      "Lodolita arenosa \n(Lodo arenoso)",
    0,    10,    90,    "Sandy mudstone \n(Sandy mud)",      "Lodolita arenosa \n(Lodo arenoso)",
    0,    50,    50,    "Sandy mudstone \n(Sandy mud)",      "Lodolita arenosa \n(Lodo arenoso)",
    5,    47.5,    47.5,    "Sandy mudstone \n(Sandy mud)",      "Lodolita arenosa \n(Lodo arenoso)",
    5,    9.5,    85.5,    "Mudstone \n(Mud)",      "Lodolita \n(Lodo)",
    5,    0,    95,    "Mudstone \n(Mud)",      "Lodolita \n(Lodo)",
    0,    0,    100,    "Mudstone \n(Mud)",      "Lodolita \n(Lodo)",
    0,    10,    90,    "Mudstone \n(Mud)",      "Lodolita \n(Lodo)",
    5,    9.5,    85.5,    "Mudstone \n(Mud)",      "Lodolita \n(Lodo)"
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
  
  Folk.ternaryAxes.en <- list(
    aaxis = axis("Gravel"), 
    baxis = axis("Sand"), 
    caxis = axis("Mud")
  )
  
  Folk.ternaryAxes.es <- list(
    aaxis = axis("Grava"), 
    baxis = axis("Arena"), 
    caxis = axis("Lodo")
  )
  
  Folk.pal = grDevices::colorRampPalette(RColorBrewer::brewer.pal(9,'OrRd'))(11)
  
  if (any(output == 'ggplot' & language == 'en')) {
    Folk <- ggtern::ggtern(data=tb.Folk,ggtern::aes(S,G,M)) +
      ggplot2::geom_polygon(ggplot2::aes(fill=Label,color=Label,
                                         group=Label),
                            alpha=opacity) +
      ggtern::theme_bw() + 
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = Folk.pal) +
      ggplot2::scale_color_manual(values = Folk.pal) +
      ggplot2::labs(fill = "Rock (sediment)",
                    color = "Rock (sediment)",
                    T="Gravel",
                    L="Sand",
                    R="Mud")
  } else if (any(output == 'ggplot' & language == 'es')) {
    Folk <- ggtern::ggtern(data=tb.Folk,ggtern::aes(S,G,M)) +
      ggplot2::geom_polygon(ggplot2::aes(fill=Label.es,color=Label.es,
                                         group=Label.es),
                            alpha=opacity) +
      ggtern::theme_bw() + 
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = Folk.pal) +
      ggplot2::scale_color_manual(values = Folk.pal) +
      ggplot2::labs(fill = "Roca (sedimento)",
                    color = "Roca (sedimento)",
                    T="Grava",
                    L="Arena",
                    R="Lodo")
  } else if (any(output == 'plotly' & language == 'en')) {
    Folk = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.Folk,
        a = ~G, b = ~S, c = ~M, 
        color = ~Label, 
        colors = Folk.pal %>% purrr::set_names(levels(tb.Folk$Label)),
        opacity = opacity*2,
        type = "scatterternary",
        fill = "toself", 
        mode = "lines",
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        ternary = Folk.ternaryAxes.en,
        legend = list(title=list(text='<b> Rock (sediment) </b>')),
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
        a = ~G, b = ~S, c = ~M, 
        color = ~Label.es, 
        colors = Folk.pal %>% purrr::set_names(levels(tb.Folk$Label.es)),
        opacity = opacity*2,
        type = "scatterternary",
        fill = "toself", 
        mode = "lines",
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        ternary = Folk.ternaryAxes.es,
        legend = list(title=list(text='<b> Roca (sedimento) </b>')),
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
