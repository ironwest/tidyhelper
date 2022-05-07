#' Addin for rename and select column(s)
#'
#' This function is for addin and should not run manually by user.
#'
#' @import shiny
#' @import shinyWidgets
#' @import purrr
#' @import stringr
#' @import clipr
#' @import dplyr
#' @import tibble
#' @keywords internal

renameR <- function(){

  #multi input ui
  selectcolui <- shinyWidgets::multiInput(
    inputId = "select_column",
    label = "Select col to rename:",
    choices = c(1),
    options = list(
      `actions-box` = TRUE
    ),
    width = "100%"
  )

  #UI-------------------
  ui <- fluidPage(
    column(width = 7,
           fluidRow(selectcolui),
           fluidRow(
             column(width=6, actionButton("selall","Select All",width="100%")),
             column(width=6, actionButton("deselall","Deselect All",width="100%"))
           ),
           hr(),
           fluidRow(column(width=12,uiOutput("renamer"))),
           radioGroupButtons("sel_ren",choices=c("select","rename"))),
    column(width = 5, verbatimTextOutput("code"), actionButton("ccopy","Copy to clipboard"))
  )

  #Server------------------------
  server <- function(input, output, session) {

    #get variable names from environment-------------------
    tgts <- ls(envir = globalenv())

    #modal ui for variable selection-------------------------
    modalui <- modalDialog(
      selectInput("selected_object","Select Object",choices = tgts),
      footer = list(
        actionButton("choose","Choose"),
        actionButton("quit","Quit",onclick = "setTimeout(function(){window.close();},500);")
      ),
      easyClose = FALSE
    )

    #show modal-----------------------------
    showModal(modalui)

    #quit app when quit button pressed---------------------------
    observeEvent(input$quit,{
      shiny::stopApp(NULL)
    })


    #update multi input and make dat() when appropriate variable name selected--------------
    dat <- reactive({
      if(input$selected_object == ""){
        res <- ""
      }else{
        res <- get(input$selected_object, envir = globalenv())
      }

      return(res)
    })

    observeEvent(input$choose,{
      var_class <- class(dat())

      if(any(var_class %in% c("data.frame","tibble"))){
        updateMultiInput(session,"select_column",choices = colnames(dat()))
        removeModal()
      }else{
        updateSelectInput(session, "selected_object", label = "Select Object(SELECT DATA FRAME!)")
      }


    })

    #select all column name when button clicked---------------------
    observeEvent(input$selall, {
      updateMultiInput(session,"select_column",choices = colnames(dat()), selected = colnames(dat()))
    })

    observeEvent(input$deselall, {
      updateMultiInput(session,"select_column",choices = colnames(dat()), selected = character(0))
    })

    #function for make textinput for each selected column----------------
    one_rename_row <- function(id_colnum, colname="", value=""){
      textInput(id_colnum,colname,placeholder = colname, value = value,width = "100%")
    }

    #function for show str of each selected column ----------------------
    one_str_row <- function(orig_dat,tgt_colname){

      res <- orig_dat |> pull({tgt_colname}) |> str_replace_na()

      res <- str_c(res[1:5],collapse = " | ")


      if(nchar(res) >= 50){
        res <- str_sub(res,1,50)
      }

      return(p(res))
    }

    #make table for ui----------------------------
    uitable <- reactive({

      req(input$select_column)

      tibble(cn =  colnames(dat())) |>
        mutate(id = str_c("id_", formatC(1:n(),width=4,flag="0"))) |>
        mutate(value = map(id, ~{
          v <- isolate(input[[.]])
          ret <- if_else(is.null(v),"",v)
          return(ret)
        }))
    })

    #make ui for rename column------------------------------
    output$renamer <- renderUI({
      render_these <- uitable() |>
        filter(cn %in% req(input$select_column))

      pmap(list(render_these$id, render_these$cn, render_these$value), ~{
        fluidRow(
          column(width=6,one_rename_row(..1, ..2,value = ..3)),
          column(width=6,one_str_row(dat(), ..2))
        )
      })
    })

    #generate code content--------------------------------
    code_content <- reactive({
      current_selection <- req(input$select_column)

      render_these <- uitable() |>
        filter(cn %in% current_selection) |>
        mutate(current_value = map_chr(id, ~{input[[.]]})) |>
        mutate(code = case_when(
          current_value == "" ~ str_glue("        `{cn}`"),
          TRUE                ~ str_glue("        `{current_value}` = `{cn}`"),
        ))

      objname <- input$selected_object
      code_body <- render_these$code |> str_c(collapse = ",\n")

      ret <- c(
        str_glue("{objname} <- {objname} |>"),
        str_glue("    {input$sel_ren}("),
        code_body,
        "    )"
      ) |>
        str_c(collapse="\n")

      return(ret)
    })

    #render code to ui----------------------------
    output$code <- renderText({
      code_content()
    })

    #copy code to clipboard---------------------------------------
    observeEvent(input$ccopy,{
      clipr::write_clip(code_content())
      showModal(modalDialog("Copied to Clipdoard!", easyClose = TRUE, footer=NULL))
    })

  }

  shinyApp(ui, server)

}
