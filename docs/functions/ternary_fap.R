ternary_fap = function(type = c('plutonic','volcanic'),
                       output = c('ggplot','plotly'), 
                       language = c('en','es')) {
  
  opacity = .05
  
  # library(ggplot2)
  # library(ggtern)
  library(magrittr)
  
  # load('data/all_tibbles.rdata')
  
  tb.FAP = tibble::tribble(
    ~F,   ~P,   ~A, ~Label,                                              ~Plut.en,                                                  ~Plut.es,                                        ~Volc.en,                                       ~Volc.es,
    0,    0,    100,   "6'",                    "Foid-bearing alakali feldspar syenite",             "Sienita alcalifeldespática con foides",              "Foid-bearing alakali feldspar trachyte",             "Traquita alcalifeldespática con foides",
    10,   0,    90,   "6'",                     "Foid-bearing alakali feldspar syenite",             "Sienita alcalifeldespática con foides",              "Foid-bearing alakali feldspar trachyte",             "Traquita alcalifeldespática con foides",
    10,    9,   81,   "6'",                     "Foid-bearing alakali feldspar syenite",             "Sienita alcalifeldespática con foides",              "Foid-bearing alakali feldspar trachyte",             "Traquita alcalifeldespática con foides",
    0,    10,    90,   "6'",                    "Foid-bearing alakali feldspar syenite",             "Sienita alcalifeldespática con foides",              "Foid-bearing alakali feldspar trachyte",             "Traquita alcalifeldespática con foides",
    0,    0,    100,   "6'",                    "Foid-bearing alakali feldspar syenite",             "Sienita alcalifeldespática con foides",              "Foid-bearing alakali feldspar trachyte",             "Traquita alcalifeldespática con foides",
    0,   10,    90,   "7'",                     "Foid-bearing syenite",                               "Sienita con foides",                             "Foid-bearing trachyte",                                     "Traquita con foides",
    10,   9,    81,   "7'",                               "Foid-bearing syenite",                               "Sienita con foides",                   "Foid-bearing trachyte",                                     "Traquita con foides",
    10,    31.4,   58.6,   "7'",                               "Foid-bearing syenite",                               "Sienita con foides",              "Foid-bearing trachyte",                                     "Traquita con foides",
    0,    35,   65,   "7'",                               "Foid-bearing syenite",                               "Sienita con foides",                   "Foid-bearing trachyte",                                     "Traquita con foides",
    0,   10,    90,   "7'",                               "Foid-bearing syenite",                               "Sienita con foides",                   "Foid-bearing trachyte",                                     "Traquita con foides",
    10,   31.4,    58.6,    "8'",                 "Foid-bearing monzonite",                              "Monzonita con foides",                              "Foid-bearing latite",                                         "Latita con foides",
    0,   35,    65,    "8'",                      "Foid-bearing monzonite",                              "Monzonita con foides",                              "Foid-bearing latite",                                         "Latita con foides",
    0,   65,    35,    "8'",                      "Foid-bearing monzonite",                              "Monzonita con foides",                              "Foid-bearing latite",                                         "Latita con foides",
    10,   58.5,    31.5,    "8'",                 "Foid-bearing monzonite",                              "Monzonita con foides",                              "Foid-bearing latite",                                         "Latita con foides",
    10,   31.4,    58.6,    "8'",                 "Foid-bearing monzonite",                              "Monzonita con foides",                              "Foid-bearing latite",                                         "Latita con foides",
    10,   58.5,    31.5,   "9'",                  "Foid-bearing monzodiorite/monzogabbro",                "Monzogabro/monzodiorita con foides",                   "Basalt/Andesite",                           "Basalto/Andesita",
    0,   65,    35,   "9'",                       "Foid-bearing monzodiorite/monzogabbro",                "Monzogabro/monzodiorita con foides",                   "Basalt/Andesite",                           "Basalto/Andesita",
    0,   90,   10,   "9'",                        "Foid-bearing monzodiorite/monzogabbro",                "Monzogabro/monzodiorita con foides",                   "Basalt/Andesite",                           "Basalto/Andesita",
    10,   81,   9,   "9'",                        "Foid-bearing monzodiorite/monzogabbro",                "Monzogabro/monzodiorita con foides",                   "Basalt/Andesite",                           "Basalto/Andesita",
    10,   58.5,    31.5,   "9'",                  "Foid-bearing monzodiorite/monzogabbro",                "Monzogabro/monzodiorita con foides",                   "Basalt/Andesite",                           "Basalto/Andesita",
    0,   100,   0,   "10'",                       "Foid-bearing gabbro/diorite/anorthosite",              "Gabro/diorita/anortosita con foides",                  "Basalt/Andesite",                           "Basalto/Andesita",
    10,   90,   0,   "10'",                       "Foid-bearing gabbro/diorite/anorthosite",              "Gabro/diorita/anortosita con foides",                  "Basalt/Andesite",                           "Basalto/Andesita",
    10,   81,   9,   "10'",                       "Foid-bearing gabbro/diorite/anorthosite",              "Gabro/diorita/anortosita con foides",                  "Basalt/Andesite",                           "Basalto/Andesita",
    0,   90,   10,   "10'",                       "Foid-bearing gabbro/diorite/anorthosite",              "Gabro/diorita/anortosita con foides",                  "Basalt/Andesite",                           "Basalto/Andesita",
    0,   100,   0,   "10'",                       "Foid-bearing gabbro/diorite/anorthosite",              "Gabro/diorita/anortosita con foides",                  "Basalt/Andesite",                           "Basalto/Andesita",
    10,   0,   90,    "11",                                        "Foid syenite",                                            "Sienita foidítica",                                        "Phonolite",                                       "Fonolita",
    10,   9,   81,    "11",                                        "Foid syenite",                                            "Sienita foidítica",                                        "Phonolite",                                       "Fonolita",
    60,    4,   36,    "11",                                        "Foid syenite",                                            "Sienita foidítica",                                        "Phonolite",                                       "Fonolita",
    60,    0,   40,    "11",                                        "Foid syenite",                                            "Sienita foidítica",                                        "Phonolite",                                       "Fonolita",
    10,   0,   90,    "11",                                        "Foid syenite",                                            "Sienita foidítica",                                        "Phonolite",                                       "Fonolita",
    10,    9,   81,    "12",                                  "Foid monzosyenite",                                       "Monzosienita foidítica",                 "Tephritic phonolite",                                  "Fonolita tefrítica",
    10,    45,   45,    "12",                                 "Foid monzosyenite",                                       "Monzosienita foidítica",                 "Tephritic phonolite",                                  "Fonolita tefrítica",
    60,    20,   20,    "12",                                 "Foid monzosyenite",                                       "Monzosienita foidítica",                 "Tephritic phonolite",                                  "Fonolita tefrítica",
    60,    4,   36,    "12",                                  "Foid monzosyenite",                                       "Monzosienita foidítica",                 "Tephritic phonolite",                                  "Fonolita tefrítica",
    10,    9,   81,    "12",                                  "Foid monzosyenite",                                       "Monzosienita foidítica",                 "Tephritic phonolite",                                  "Fonolita tefrítica",
    10,   45,    45,   "13",                      "Foid monzodiorite/monzogabbro",                     "Monzodiorita/monzogabro foidítica",               "Phonolitic basanite/tephrite",           "Basanita/tefrita fonolítica",
    10,   81,    9,   "13",                      "Foid monzodiorite/monzogabbro",                     "Monzodiorita/monzogabro foidítica",               "Phonolitic basanite/tephrite",           "Basanita/tefrita fonolítica",
    60, 36,  4,   "13",                      "Foid monzodiorite/monzogabbro",                     "Monzodiorita/monzogabro foidítica",               "Phonolitic basanite/tephrite",           "Basanita/tefrita fonolítica",
    60,   20,    20,   "13",                      "Foid monzodiorite/monzogabbro",                     "Monzodiorita/monzogabro foidítica",              "Phonolitic basanite/tephrite",           "Basanita/tefrita fonolítica",
    10,   45,    45,   "13",                      "Foid monzodiorite/monzogabbro",                     "Monzodiorita/monzogabro foidítica",               "Phonolitic basanite/tephrite",           "Basanita/tefrita fonolítica",
    10,   81,    9,   "14",                            "Foid diorite/gabbro",                            "Diorita/gabro foidítica",                               "Basanite/tephrite",                            "Basanita/tefrita",
    10, 90,  0,   "14",                                "Foid diorite/gabbro",                            "Diorita/gabro foidítica",                               "Basanite/tephrite",                            "Basanita/tefrita",
    60,   40,   0,   "14",                             "Foid diorite/gabbro",                            "Diorita/gabro foidítica",                               "Basanite/tephrite",                            "Basanita/tefrita",
    60,   36,   4,   "14",                             "Foid diorite/gabbro",                            "Diorita/gabro foidítica",                               "Basanite/tephrite",                            "Basanita/tefrita",
    10,   81,    9,   "14",                            "Foid diorite/gabbro",                             "Diorita/gabro foidítica",                              "Basanite/tephrite",                            "Basanita/tefrita"
  ) %>% 
    dplyr::mutate(dplyr::across(Label:last_col(),forcats::as_factor))
  
  Labs.FAP = data.frame(
    F = c(5,5,5,5,5,28,28,28,28),
    P = c(5,21,47.5,72,90,3,22,50,68),
    A = c(90,74,47.5,23,5,69,50,22,4),
    Label = as.factor(c("6'","7'","8'","9'","10'","11","12","13","14"))
  )
  
  tb.fap.plut = tibble::tribble(
    ~F,   ~P,   ~A, ~Label,                     ~Plut.en,               ~Plut.es,
    100,    0,    0,   "15",                    "Foidolite",             "Foidolita",
    60,   40,    0,   "15",                     "Foidolite",             "Foidolita",
    60,    0,   40,   "15",                     "Foidolite",             "Foidolita",
    100,    0,    0,   "15",                    "Foidolite",             "Foidolita"
  )
  
  tb.fap.plut = dplyr::bind_rows(
    tb.FAP %>% dplyr::select(1:4,Plut.en,Plut.es),
    tb.fap.plut
  )
  
  Labs.fap.plut = data.frame(
    F = 75,
    P = 12.5,
    A = 12.5,
    Label = as.factor("15")
  )
  
  Labs.fap.plut = dplyr::bind_rows(
    Labs.FAP %>% dplyr::select(1:4),
    Labs.fap.plut
  )
  
  tb.fap.volc= tibble::tribble(
    ~F,   ~P,   ~A, ~Label,                     ~Volc.en,                     ~Volc.es,
    60,    0,    40,   "15a",           "Phonolitic foidite",       "Foidita fonolítica",
    60,   20,    20,   "15a",            "Phonolitic foidite",       "Foidita fonolítica",
    90,    5,   5,   "15a",            "Phonolitic foidite",       "Foidita fonolítica",
    90,    0,    10,   "15a",           "Phonolitic foidite",       "Foidita fonolítica",
    60,    0,    40,   "15a",           "Phonolitic foidite",       "Foidita fonolítica",
    90,    5,   5,   "15b",            "Tephritic foidite",       "Foidita tefrítica",
    60,    20,   20,   "15b",            "Tephritic foidite",       "Foidita tefrítica",
    60,    40,   0,   "15b",            "Tephritic foidite",       "Foidita tefrítica",
    90,    10,   0,   "15b",            "Tephritic foidite",       "Foidita tefrítica",
    90,    5,   5,   "15b",            "Tephritic foidite",       "Foidita tefrítica",
    90,    10,   0,   "15c",                     "Foidite",                 "Foidita",
    100,    0,   0,   "15c",                     "Foidite",                 "Foidita",
    90,    0,   10,   "15c",                     "Foidite",                 "Foidita",
    90,    10,   0,   "15c",                     "Foidite",                 "Foidita"
  )
  
  tb.fap.volc = dplyr::bind_rows(
    tb.FAP %>% dplyr::select(1:4,Volc.en,Volc.es),
    tb.fap.volc
  )
  
  Labs.fap.volc = data.frame(
    F = c(75,75,94),
    P = c(7,18,3),
    A = c(18,7,3),
    Label = as.factor(c("15a","15b","15c"))
  )
  
  Labs.fap.volc = dplyr::bind_rows(
    Labs.FAP %>% dplyr::select(1:4),
    Labs.fap.volc
  )
  
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
  
  FAP.ternaryAxes <- list(
    aaxis = axis("F"), 
    baxis = axis("P"), 
    caxis = axis("A")
  )
  
  FAP.pal = c('#BC97B7','#AD99AA','#998D9D','#897F8A','#746C74',
              '#91ABD5','#98A8C5','#9197A3','#6E6F73',
              '#5489C5','#5489C5','#3364A2')
  
  if (any(type == 'plutonic' & output == 'ggplot')) {
    FAP <- ggtern::ggtern(data=tb.fap.plut,ggtern::aes(P,F,A)) +
      ggplot2::geom_polygon(aes(group=Label),
                            fill='white',
                            color="black",alpha=opacity) +
      ggplot2::geom_text(data=Labs.fap.plut,aes(label=Label),
                         size=2.5,color="black",show.legend = T) +
      ggplot2::theme_bw() +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      theme_rotate(180) +
      # custom_percent("Percent") +
      # ggplot2::scale_fill_manual(values = FAP.pal) +
      # ggplot2::scale_color_manual(values = FAP.pal) +
      ggplot2::labs(T="F",
                    L="P",
                    R="A")
  } else if (any(type == 'volcanic' & output == 'ggplot')) {
    FAP <- ggtern::ggtern(data=tb.fap.volc,ggtern::aes(P,F,A)) +
      ggplot2::geom_polygon(aes(group=Label),
                            fill='white',
                            color="black",alpha=opacity) +
      ggplot2::geom_text(data=Labs.fap.volc,aes(label=Label),
                         size=2.5,color="black",show.legend = T) +
      ggplot2::theme_bw() +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      theme_rotate(180) +
      ggplot2::labs(T="F",
                    L="P",
                    R="A")
  } else if (any(type == 'plutonic' & output == 'plotly' & language == 'en')) {
    FAP = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.fap.plut,
        a = ~F, b = ~P, c = ~A, 
        color = ~Label,
        colors = 'transparent',
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black"),
        hoverinfo = 'text',
        text = ~paste('</br>', Label,
                      '</br>', Plut.en),
        hoveron = 'fills'
      ) %>%
      plotly::add_trace(
        data = Labs.fap.plut,
        a = ~F, b = ~P, c = ~A,
        text = ~Label,
        type = "scatterternary",
        mode = "text",
        name = 'ID',
        hoverinfo = 'none',
        textfont = list(family= "Arial", size= 12, color= "black"),
        showlegend = T
      ) %>% 
      plotly::layout(
        ternary = FAP.ternaryAxes
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'FAP',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(type == 'plutonic' & output == 'plotly' & language == 'es')) {
    FAP = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.fap.plut,
        a = ~F, b = ~P, c = ~A, 
        color = ~Label,
        colors = 'transparent',
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black"),
        hoverinfo = 'text',
        text = ~paste('</br>', Label,
                      '</br>', Plut.es),
        hoveron = 'fills'
      ) %>%
      plotly::add_trace(
        data = Labs.fap.plut,
        a = ~F, b = ~P, c = ~A,
        text = ~Label,
        type = "scatterternary",
        mode = "text",
        name = 'ID',
        hoverinfo = 'none',
        textfont = list(family= "Arial", size= 12, color= "black"),
        showlegend = T
      ) %>% 
      plotly::layout(
        ternary = FAP.ternaryAxes
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'FAP',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(type == 'volcanic' & output == 'plotly' & language == 'en')) {
    FAP = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.fap.volc,
        a = ~F, b = ~P, c = ~A, 
        color = ~Label,
        colors = 'transparent',
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black"),
        hoverinfo = 'text',
        text = ~paste('</br>', Label,
                      '</br>', Volc.en),
        hoveron = 'fills'
      ) %>%
      plotly::add_trace(
        data = Labs.fap.volc,
        a = ~F, b = ~P, c = ~A,
        text = ~Label,
        type = "scatterternary",
        mode = "text",
        name = 'ID',
        hoverinfo = 'none',
        textfont = list(family= "Arial", size= 12, color= "black"),
        showlegend = T
      ) %>% 
      plotly::layout(
        ternary = FAP.ternaryAxes,
        margin = list(autoexpand=T,t=35)
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'FAP',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(type == 'volcanic' & output == 'plotly' & language == 'es')) {
    FAP = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.fap.volc,
        a = ~F, b = ~P, c = ~A, 
        color = ~Label,
        colors = 'transparent',
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black"),
        hoverinfo = 'text',
        text = ~paste('</br>', Label,
                      '</br>', Volc.es),
        hoveron = 'fills'
      ) %>%
      plotly::add_trace(
        data = Labs.fap.volc,
        a = ~F, b = ~P, c = ~A,
        text = ~Label,
        type = "scatterternary",
        mode = "text",
        name = 'ID',
        hoverinfo = 'none',
        textfont = list(family= "Arial", size= 12, color= "black"),
        showlegend = T
      ) %>% 
      plotly::layout(
        ternary = FAP.ternaryAxes,
        margin = list(autoexpand=T,t=35)
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'FAP',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }
  
  return(FAP)
  
}
