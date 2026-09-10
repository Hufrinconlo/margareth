# ---------------------------------------------------------------------------
# app/app.R  --  Margaret dashboard
#
# Reads the artifact published by the weekly ETL to the 'data' branch.
# Never scrapes.
#
# NOTE ON LOADING: data is fetched inside server() via reactive(), not at the
# top level. Top-level code runs once per R PROCESS, and shinyapps.io keeps a
# process alive across many visitors -- so a top-level load would keep serving
# whatever the data looked like when the process booted. reactive() runs once
# per SESSION, so every visitor gets the current artifact.
# ---------------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(plotly)
library(DT)

source("load_data.R")

count_sheet <- function(prod, name, yr) {
  s <- prod[[name]]
  if (is.null(s) || !is.data.frame(s) || nrow(s) == 0) return(0L)
  if ("ano" %in% names(s)) {
    a <- suppressWarnings(as.numeric(s$ano))
    return(sum(!is.na(a) & a >= yr[1] & a <= yr[2]))
  }
  nrow(s)
}

pie_of <- function(df, col, title) {
  tab <- df |> count(.data[[col]], name = "n") |> arrange(desc(n))
  plot_ly(tab, labels = ~.data[[col]], values = ~n, type = "pie",
          textinfo = "percent", hoverinfo = "label+value") |>
    layout(title = title, showlegend = TRUE)
}

# --- UI --------------------------------------------------------------------
# A FUNCTION, not an object. Shiny re-evaluates a function UI on every page
# load; a plain object would be built once at process start.
ui <- function(req) {
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Margaret"),

    dashboardSidebar(
      uiOutput("year_ui"),          # built from data, so rendered server-side
      sidebarMenu(
        menuItem("Gráficas Generales", tabName = "general", icon = icon("chart-line")),
        menuItem("Grupos",             tabName = "grupos",  icon = icon("layer-group")),
        menuItem("Investigadores",     tabName = "inv",     icon = icon("users")),
        menuItem("Datos",              tabName = "datos",   icon = icon("table"))
      ),
      tags$div(style = "padding:10px; font-size:11px; color:#b8c7ce;",
               textOutput("stamp"))
    ),

    dashboardBody(
      tabItems(
        tabItem("general",
          fluidRow(
            valueBoxOutput("vb_grupos", width = 6),
            valueBoxOutput("vb_inv",    width = 6)
          ),
          fluidRow(
            valueBoxOutput("vb_art", width = 4),
            valueBoxOutput("vb_cap", width = 4),
            valueBoxOutput("vb_lib", width = 4)
          ),
          fluidRow(
            valueBoxOutput("vb_soft", width = 4),
            valueBoxOutput("vb_proy", width = 4),
            valueBoxOutput("vb_trab", width = 4)
          ),
          fluidRow(
            box(plotlyOutput("p_produccion"), width = 6),
            box(plotlyOutput("p_calidad"),    width = 6)
          ),
          fluidRow(box(plotlyOutput("p_clasif"), width = 12)),
          fluidRow(
            box(plotlyOutput("p_categorias"), width = 6),
            box(plotlyOutput("p_formacion"),  width = 6)
          )
        ),

        tabItem("grupos",
          fluidRow(box(plotlyOutput("p_grupo_prod", height = 420), width = 12)),
          fluidRow(box(DTOutput("t_grupos"), width = 12))
        ),

        tabItem("inv",
          fluidRow(box(plotlyOutput("p_inv_grupo", height = 420), width = 12)),
          fluidRow(box(DTOutput("t_inv"), width = 12))
        ),

        tabItem("datos",
          fluidRow(box(title = "Productos por categoría", width = 12,
                       DTOutput("t_prod")))
        )
      )
    )
  )
}

# --- server ----------------------------------------------------------------
server <- function(input, output, session) {

  # Runs once per session, cached for the rest of it. Everything downstream
  # calls dat() instead of a global.
  dat <- reactive({
    d <- load_margaret()
    prod <- lapply(d$products, function(x) if (is.data.frame(x)) ungroup(x) else x)

    art <- prod$articulos |>
      mutate(ano = suppressWarnings(as.numeric(ano))) |>
      filter(!is.na(ano), ano >= 1990,
             ano <= as.numeric(format(Sys.Date(), "%Y")))

    list(
      grupos = ungroup(d$groups),
      inv    = ungroup(d$researchers),
      prod   = prod,
      art    = art,
      health = load_health()
    )
  })

  # Slider depends on the data, so it's rendered rather than declared in UI.
  output$year_ui <- renderUI({
    yrs <- range(dat()$art$ano, na.rm = TRUE)
    sliderInput("years", "Años:", min = yrs[1], max = yrs[2],
                value = c(max(yrs[1], yrs[2] - 10), yrs[2]),
                step = 1, sep = "")
  })

  yr <- reactive({
    req(input$years)          # wait until the slider exists
    input$years
  })

  art_f <- reactive({
    dat()$art |> filter(ano >= yr()[1], ano <= yr()[2])
  })

  output$stamp <- renderText({
    h <- dat()$health
    paste("Datos:", substr(h$run_at[1], 1, 10),
          "|", h$researchers[1], "investigadores")
  })

  # --- value boxes ---
  output$vb_grupos <- renderValueBox(
    valueBox(nrow(dat()$grupos), "Grupos Actuales",
             icon = icon("layer-group"), color = "yellow"))
  output$vb_inv <- renderValueBox(
    valueBox(nrow(dat()$inv), "Investigadores Activos",
             icon = icon("users"), color = "yellow"))
  output$vb_art <- renderValueBox(
    valueBox(nrow(art_f()), "Total Artículos",
             icon = icon("file"), color = "blue"))
  output$vb_cap <- renderValueBox(
    valueBox(count_sheet(dat()$prod, "capitulos", yr()), "Total Capítulos",
             icon = icon("book"), color = "blue"))
  output$vb_lib <- renderValueBox(
    valueBox(count_sheet(dat()$prod, "libros", yr()), "Total Libros",
             icon = icon("book-open"), color = "blue"))
  output$vb_soft <- renderValueBox(
    valueBox(count_sheet(dat()$prod, "softwares", yr()), "Total Software",
             icon = icon("code"), color = "blue"))
  output$vb_proy <- renderValueBox(
    valueBox(count_sheet(dat()$prod, "proyectos", yr()), "Total Proyectos",
             icon = icon("lightbulb"), color = "blue"))
  output$vb_trab <- renderValueBox(
    valueBox(count_sheet(dat()$prod, "trabajos_dirigidos", yr()),
             "Total Trabajos Dirigidos",
             icon = icon("chalkboard-teacher"), color = "blue"))

  # --- charts ---
  output$p_produccion <- renderPlotly({
    tab <- art_f() |> count(ano, name = "n")
    plot_ly(tab, x = ~ano, y = ~n, type = "scatter", mode = "lines+markers",
            line = list(color = "#e74c3c")) |>
      layout(title = "Producción artículos",
             xaxis = list(title = "Año"), yaxis = list(title = "Producción"))
  })

  output$p_calidad <- renderPlotly({
    tab <- art_f() |>
      filter(SJR_Q %in% c("Q1", "Q2", "Q3", "Q4")) |>
      count(ano, SJR_Q, name = "n")
    validate(need(nrow(tab) > 0, "Sin datos de cuartil en este rango"))
    plot_ly(tab, x = ~ano, y = ~n, color = ~SJR_Q, type = "bar") |>
      layout(title = "Calidad de Producción de Artículos por Año",
             barmode = "group", xaxis = list(title = "Año"),
             yaxis = list(title = "Total de Artículos"))
  })

  output$p_clasif <- renderPlotly({
    tab <- dat()$grupos |> count(clasificacion, name = "n") |>
      arrange(clasificacion)
    plot_ly(tab, x = ~clasificacion, y = ~n, type = "bar",
            marker = list(color = "#2980b9")) |>
      layout(title = "Clasificación Grupos de investigación",
             xaxis = list(title = ""), yaxis = list(title = ""))
  })

  output$p_categorias <- renderPlotly(
    pie_of(dat()$inv, "clasification", "Categorías investigadores"))
  output$p_formacion <- renderPlotly(
    pie_of(dat()$inv, "posgrade", "Formación investigadores"))

  # --- grupos ---
  output$p_grupo_prod <- renderPlotly({
    tab <- art_f() |> count(grupo, name = "n") |> arrange(n)
    tab$corto <- substr(tab$grupo, 1, 45)
    plot_ly(tab, x = ~n, y = ~reorder(corto, n), type = "bar",
            orientation = "h", marker = list(color = "#2980b9")) |>
      layout(title = "Artículos por grupo",
             xaxis = list(title = "Artículos"), yaxis = list(title = ""),
             margin = list(l = 260))
  })

  output$t_grupos <- renderDT({
    dat()$grupos |>
      select(any_of(c("grupo", "clasificacion", "lider", "ciudad",
                      "fecha_creacion", "area_conocimiento_1", "sum_papers"))) |>
      datatable(options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE)
  })

  # --- investigadores ---
  output$p_inv_grupo <- renderPlotly({
    tab <- dat()$inv |>
      separate_rows(grupo, sep = "; ") |>
      count(grupo, posgrade, name = "n")
    tab$corto <- substr(tab$grupo, 1, 45)
    plot_ly(tab, x = ~n, y = ~corto, color = ~posgrade, type = "bar",
            orientation = "h") |>
      layout(title = "Formación por grupo", barmode = "stack",
             xaxis = list(title = "Investigadores"), yaxis = list(title = ""),
             margin = list(l = 260))
  })

  output$t_inv <- renderDT({
    dat()$inv |>
      select(any_of(c("integrantes", "posgrade", "clasification",
                      "vinculacion", "grupo"))) |>
      datatable(options = list(pageLength = 15, scrollX = TRUE),
                rownames = FALSE)
  })

  output$t_prod <- renderDT({
    product_summary(dat()$prod) |>
      rename(Categoría = tipo, Registros = filas) |>
      datatable(options = list(pageLength = 20), rownames = FALSE)
  })
}

shinyApp(ui, server)