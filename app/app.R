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

YEAR_MIN <- 1950L
YEAR_MAX <- 2100L

# Sheets disagree on how they record a year: `ano`, `Ano`, or only a start date
# ("fecha_inicio"/"desde"), whose values carry a leading space. Candidates are
# tried in order and the first that parses for most rows wins, so a column that
# holds something else entirely -- regulaciones_normas$ano is "Colombia" -- does
# not win and filter every row away. NULL means the sheet has no usable year, so
# callers show everything and say "todos los años" rather than filtering on NAs.
sheet_years <- function(s) {
  nm <- names(s)
  for (cl in c(nm[tolower(nm) == "ano"],
               nm[tolower(nm) %in% c("fecha_inicio", "desde")])) {
    y <- suppressWarnings(as.integer(substr(trimws(as.character(s[[cl]])), 1, 4)))
    ok <- !is.na(y) & y > YEAR_MIN & y < YEAR_MAX
    if (sum(ok) >= nrow(s) * 0.8) return(ifelse(ok, y, NA_integer_))
  }
  NULL
}

# A DOI is stored bare ("10.1000/xyz"), but the column also holds resolver URLs,
# "DOI:" labels and non-values like "No aplica". Anything that is not a DOI after
# cleaning returns NA, so the row simply does not open and the caption's count
# matches what is actually clickable.
doi_url <- function(x) {
  v <- trimws(as.character(x))
  v <- sub("^doi\\s*:\\s*", "", v, perl = TRUE, ignore.case = TRUE)
  v <- trimws(sub("^https?://(dx\\.)?doi\\.org/", "", v, perl = TRUE,
                  ignore.case = TRUE))
  ifelse(grepl("^10\\.\\S+/\\S+", v), paste0("https://doi.org/", v), NA_character_)
}

count_sheet <- function(prod, name, yr) {
  s <- prod[[name]]
  # An absent sheet is not the same claim as a count of zero: a 0-row sheet
  # rendered as "0 Software" reads as "produced none".
  if (is.null(s) || !is.data.frame(s) || nrow(s) == 0)
    return(list(value = "n/d", note = "sin datos"))
  y <- sheet_years(s)
  if (!is.null(y))
    return(list(value = sum(!is.na(y) & y >= yr[1] & y <= yr[2]), note = NULL))
  list(value = nrow(s), note = "todos los años")
}

sheet_box <- function(prod, name, label, yr, icon_name) {
  r <- count_sheet(prod, name, yr)
  valueBox(r$value,
           if (is.null(r$note)) label else paste0(label, " (", r$note, ")"),
           icon = icon(icon_name), color = "blue")
}

pretty_names <- function(x) {
  sub("^(.)", "\\U\\1", gsub("_", " ", x), perl = TRUE)
}

# A product box that doubles as a link into its own records on the Datos tab.
sheet_box_ui <- function(outputId, sheet, width = 4) {
  tags$div(
    class   = paste0("col-sm-", width, " vb-link"),
    onclick = sprintf("Shiny.setInputValue('go_sheet', '%s', {priority:'event'});", sheet),
    valueBoxOutput(outputId, width = 12)
  )
}

# Rows open the underlying GrupLAC/CvLAC page. The url rides along as the last
# column and is hidden, so the click handler can read it out of the row data.
# It is excluded from DT's HTML escaping: an escaped `&` would corrupt any
# query string before window.open ever saw it.
link_table <- function(df, cols, url_col, page_len, scroll_y = NULL) {
  d <- df |> select(any_of(c(cols, url_col)))
  uidx <- ncol(d) - 1L                       # 0-based, for the JS data array

  datatable(
    d,
    rownames  = FALSE,
    colnames  = pretty_names(names(d)),
    escape    = -(uidx + 1L),
    selection = "none",        # rows are links; a sticky highlight means nothing

    options  = c(
      list(pageLength = page_len, scrollX = TRUE,
           columnDefs = list(list(visible = FALSE, targets = uidx))),
      # Caps the body height so the horizontal bar stays in view.
      if (!is.null(scroll_y)) list(scrollY = scroll_y, scrollCollapse = TRUE)
    ),
    callback = JS(sprintf(
      "table.on('click', 'tbody tr', function() {
         var u = table.row(this).data()[%d];
         if (u) window.open(u, '_blank', 'noopener,noreferrer');
       });", uidx))
  )
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
    # Set explicitly: with HTML in the header title, dashboardPage would reuse
    # that markup for the browser <title> and the tab would show the tag itself.
    title = "Margaret",
    # The title doubles as a way home, reusing the go_tab observer.
    dashboardHeader(title = tags$span(
      "Margaret", class = "logo-home",
      onclick = "Shiny.setInputValue('go_tab', 'general', {priority:'event'});"
    )),

    dashboardSidebar(
      uiOutput("year_ui"),          # built from data, so rendered server-side
      sidebarMenu(
        id = "tabs",                  # needed by updateTabItems()
        menuItem("Gráficas Generales", tabName = "general", icon = icon("chart-line")),
        menuItem("Grupos",             tabName = "grupos",  icon = icon("layer-group")),
        menuItem("Investigadores",     tabName = "inv",     icon = icon("users")),
        menuItem("Datos",              tabName = "datos",   icon = icon("table"))
      ),
      tags$div(style = "padding:10px; font-size:11px; color:#b8c7ce;",
               textOutput("stamp"))
    ),

    dashboardBody(
      tags$head(tags$style(HTML("
        #t_grupos tbody tr, #t_inv tbody tr { cursor: pointer; }
        .vb-link { cursor: pointer; }
        .vb-link:hover .small-box { filter: brightness(1.07); }
        .logo-home { cursor: pointer; }
        .logo-home:hover { text-decoration: underline; }
        /* Datos: the sheets are wide, and with the scrollbar at the foot of a
           full-height table you had to scroll the page down to reach it. The
           body is capped instead, so the bar stays on screen -- forced to
           'scroll' so it is drawn even where the OS hides idle scrollbars. */
        #t_sheet .dataTables_scrollBody { overflow-x: scroll !important; }
        #t_sheet .dataTables_scrollBody::-webkit-scrollbar { height: 12px; }
        #t_sheet .dataTables_scrollBody::-webkit-scrollbar-track {
          background: #f0f0f0; border-radius: 6px; }
        #t_sheet .dataTables_scrollBody::-webkit-scrollbar-thumb {
          background: #adadad; border-radius: 6px; }
        #t_sheet .dataTables_scrollBody::-webkit-scrollbar-thumb:hover {
          background: #8c8c8c; }
      "))),
      tabItems(
        tabItem("general",
          fluidRow(
            tags$div(class = "col-sm-6 vb-link",
                     onclick = "Shiny.setInputValue('go_tab', 'grupos', {priority:'event'});",
                     valueBoxOutput("vb_grupos", width = 12)),
            tags$div(class = "col-sm-6 vb-link",
                     onclick = "Shiny.setInputValue('go_tab', 'inv', {priority:'event'});",
                     valueBoxOutput("vb_inv", width = 12))
          ),
          fluidRow(
            sheet_box_ui("vb_art", "articulos"),
            sheet_box_ui("vb_cap", "capitulos"),
            sheet_box_ui("vb_lib", "libros")
          ),
          fluidRow(
            sheet_box_ui("vb_even", "eventos_cientificos"),
            sheet_box_ui("vb_proy", "proyectos"),
            sheet_box_ui("vb_trab", "trabajos_dirigidos")
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
          fluidRow(
            box(title = "Productos por categoría", width = 4,
                tags$p(style = "color:#777; font-size:12px; margin-bottom:8px;",
                       "Clic en una categoría para ver sus registros."),
                DTOutput("t_prod")),
            # Rendered server-side so there is nothing on screen until a
            # category is picked; the box itself appears with the selection.
            uiOutput("sheet_panel")
          )
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
    loaded <- tryCatch(list(d = load_margaret(), health = load_health()),
                       error = function(e) e)
    # Surfaces as a notice in every panel rather than an unhandled error page.
    if (inherits(loaded, "error")) validate(need(FALSE, conditionMessage(loaded)))

    d <- loaded$d
    prod <- lapply(d$products, function(x) if (is.data.frame(x)) ungroup(x) else x)

    # One sanity bound, shared with sheet_years(). The old 1990 floor dropped
    # three real 1985-86 articulos from the charts while the Datos tab still
    # counted them, so the denominator named rows no slider position could show.
    art <- prod$articulos |>
      mutate(ano = suppressWarnings(as.numeric(ano))) |>
      filter(!is.na(ano), ano > YEAR_MIN,
             ano <= as.numeric(format(Sys.Date(), "%Y")))

    list(
      grupos = ungroup(d$groups),
      inv    = ungroup(d$researchers),
      prod   = prod,
      art    = art,
      health = loaded$health
    )
  })

  # The two headline boxes double as navigation into their detail tabs.
  observeEvent(input$go_tab, updateTabItems(session, "tabs", input$go_tab))

  # A product box jumps to Datos and selects that category. It drives the same
  # DT selection the row click uses, so there is one source of truth and the
  # highlight stays correct however the panel was opened.
  observeEvent(input$go_sheet, {
    updateTabItems(session, "tabs", "datos")
    # product_summary() lists only sheets with rows, so an emptied one yields
    # NA. Clear the selection then, or the previous category's panel stays up
    # as though it were the one just asked for.
    i <- match(input$go_sheet, product_summary(dat()$prod)$tipo)
    DT::selectRows(DT::dataTableProxy("t_prod"), if (is.na(i)) integer(0) else i)
  })

  # Slider depends on the data, so it's rendered rather than declared in UI.
  output$year_ui <- renderUI({
    # Spans every sheet that has a usable year, not just articulos: the slider
    # governs the whole app, so a position it cannot reach hides real rows.
    ys <- unlist(lapply(dat()$prod, function(p) {
      if (is.data.frame(p) && nrow(p)) sheet_years(p) else NULL
    }), use.names = FALSE)
    yrs <- range(c(ys, dat()$art$ano), na.rm = TRUE)
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
    sheet_box(dat()$prod, "capitulos", "Total Capítulos", yr(), "book"))
  output$vb_lib <- renderValueBox(
    sheet_box(dat()$prod, "libros", "Total Libros", yr(), "book-open"))
  output$vb_even <- renderValueBox(
    sheet_box(dat()$prod, "eventos_cientificos", "Total Eventos", yr(),
              "calendar-days"))
  output$vb_proy <- renderValueBox(
    sheet_box(dat()$prod, "proyectos", "Total Proyectos", yr(), "lightbulb"))
  output$vb_trab <- renderValueBox(
    sheet_box(dat()$prod, "trabajos_dirigidos", "Total Trabajos Dirigidos",
              yr(), "chalkboard-teacher"))

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
    link_table(dat()$grupos,
               cols = c("grupo", "clasificacion", "lider", "ciudad",
                        "fecha_creacion", "area_conocimiento_1", "sum_papers"),
               url_col = "url.x", page_len = 10)
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
    link_table(dat()$inv,
               cols = c("integrantes", "posgrade", "clasification",
                        "vinculacion", "grupo"),
               url_col = "url", page_len = 15)
  })

  # The highlight is the selection: it marks which category is on show, and
  # clicking it again clears both the highlight and the panel.
  output$t_prod <- renderDT({
    product_summary(dat()$prod) |>
      rename(Categoría = tipo, Registros = filas) |>
      datatable(options = list(pageLength = 20), rownames = FALSE,
                selection = list(mode = "single", selected = NA))
  })

  # Outputs on a hidden tab are suspended by default, so t_prod would not exist
  # yet when a product box fires selectRows at it. Keep it live from the start.
  outputOptions(output, "t_prod", suspendWhenHidden = FALSE)

  # rows_selected indexes the underlying data, so it survives sorting and paging.
  sel_sheet <- reactive({
    i <- input$t_prod_rows_selected
    if (!length(i)) return(NULL)
    product_summary(dat()$prod)$tipo[i]
  })

  # The year slider sits in the sidebar on every tab, so this view honours it
  # like the value boxes do -- otherwise clicking a box reading 149 lands on a
  # table of 253. Filtered once here and shared, so the caption cannot drift
  # from the rows on screen.
  sheet_data <- reactive({
    s <- sel_sheet(); req(s)
    d <- dat()$prod[[s]]
    total <- nrow(d)
    y <- sheet_years(d)
    filtered <- !is.null(y)
    if (filtered) {
      d <- d[!is.na(y) & y >= yr()[1] & y <= yr()[2], , drop = FALSE]
    }
    list(d = d, total = total, filtered = filtered,
         url = if ("DOI" %in% names(d)) doi_url(d$DOI) else NULL)
  })

  output$sheet_panel <- renderUI({
    s <- sel_sheet()
    if (is.null(s)) return(NULL)
    x <- sheet_data()
    box(width = 8, title = s,
        tags$p(style = "color:#777; font-size:12px; margin-bottom:8px;",
               if (x$filtered)
                 sprintf("%d de %d registros (%d-%d), %d columnas.",
                         nrow(x$d), x$total, yr()[1], yr()[2], ncol(x$d))
               else
                 sprintf("%d registros (todos los años), %d columnas.",
                         x$total, ncol(x$d)),
               if (!is.null(x$url))
                 sprintf(" %d tienen DOI; clic en esa fila para abrirlo.",
                         sum(!is.na(x$url)))
               else ""),
        DTOutput("t_sheet"))
  })

  output$t_sheet <- renderDT({
    x <- sheet_data()
    d <- x$d
    validate(need(nrow(d) > 0, if (x$filtered)
      sprintf("Ningún registro entre %d y %d. Ajuste el rango de años.", yr()[1], yr()[2])
      else "Esta categoría no tiene registros"))

    if (!is.null(x$url)) {
      d$.url <- x$url
      link_table(d, cols = setdiff(names(d), ".url"), url_col = ".url",
                 page_len = 15, scroll_y = "55vh")
    } else {
      datatable(d, rownames = FALSE, colnames = pretty_names(names(d)),
                selection = "none",
                options = list(pageLength = 15, scrollX = TRUE,
                               scrollY = "55vh", scrollCollapse = TRUE))
    }
  })
}

shinyApp(ui, server)