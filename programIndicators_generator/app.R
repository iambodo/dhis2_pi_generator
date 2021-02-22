# this shiny app takes DHIS2 Tracker data elements & TEIA as input,
# and then outputs a JSON metadata file of program indicators

#Structure - 
## functions, ui, download from API and manipulate metadata,
## data element table and PI table. Proxy table for PI to edit.
## Download handler

### Next steps

##export to CSV and re-import

# done
#select OFF for the tables
#column selection for cross filters table
# more than two cross filter = shiny input to up to 5?
# switch between OR AND combo
#Negate filters



## app.R ##

#### Load Libraries and define custom functions

library(tidyverse)
library(jsonlite)
library(here)
library(shiny)
library(shinydashboard)
library(httr)
library(DT)



#fix baseurl if no trailing "/" entered
trailing_backslash<-function(x){
    if_else((str_sub(x, - 1, - 1)!="/"), 
            str_c(x,"/"), x)} 

#shorten PI name into code
shorten_name<-function(x, length){
  str_trunc(x, as.numeric(length), "right")
}
  
#login function
loginDHIS2<-function(baseurl,username,password) {
  url<-paste0(baseurl,"api/me")
  r<-GET(url,authenticate(username,password))
  if_else(r$status_code == 200L, TRUE, FALSE)}



#build PI filter
build_pi_filter <- function(ps, de, pi_value, type){
  

  #if value is predictable (boolean or option set)
  case_when(
      !is.na(pi_value) & ps=="" ~ str_c("A{", de, "} == ", pi_value),
      !is.na(pi_value) & ps!="" & type=="EVENT" ~ str_c("#{", ps, ".", de, "} == ", pi_value),
      !is.na(pi_value) & ps!=""  & type=="ENROLLMENT" ~str_c("d2:countIfValue(#{",
                                                             ps, ".", de,"}, ", pi_value, ") > 0"),
      is.na(pi_value) & ps!="" ~ str_c("d2:hasValue(#{", ps, ".", de, "})"),
      is.na(pi_value) & ps=="" ~ str_c("d2:hasValue(A{", de, "})" ))
}

# Use this if the data TYPE is numeric and you want to build ranges
build_pi_filter_num <- function(ps, de, rangev1, rangev2, type, valType, dateRanges){
  
      baseA <-str_c("A{", de, "} ")
      baseD <-str_c("#{", ps, ".", de, "} ")
      
      v1op<-str_c(">=",rangev1)
      v2op<-str_c("<", rangev2)
      
      baseA<-case_when(
        valType %in% c("DATE","AGE") ~  str_replace(dateRanges, fixed("[DATE]"), baseA),
        TRUE ~ baseA
      )

      baseD<-case_when(
        valType %in% c("DATE","AGE") ~  str_replace(dateRanges, fixed("[DATE]"), baseD),
        TRUE ~ baseD
         )


    out<- case_when(
        ps=="" & type=="EVENT" ~ str_c(baseA, v1op, " && ", baseA, v2op),
        ps!="" & type=="ENROLLMENT" ~str_c("d2:countIfCondition(", baseD, "'", v1op,"') > 0 && ",
                                           "d2:countIfCondition(", baseD,  "'", v2op,"') > 0"),
        ps!="" & type=="EVENT" ~ str_c(baseD, v1op, " && ", baseD, v2op),
        ps=="" & type=="ENROLLMENT" ~ str_c(baseA, v1op, " && ", baseA, v2op),
        )
    
    return(out)
} 


# How to build date ranges
dateRangeChoices<-list("d2:hasValue([DATE])",
                       "d2:daysBetween([DATE], V{analytics_period_start})",
                       "d2:daysBetween([DATE], V{event_date})",
                       "d2:daysBetween([DATE], V{enrollment_date})",
                       "d2:yearsBetween([DATE], V{analytics_period_start})",
                       "d2:yearsBetween([DATE], V{event_date})",
                       "d2:yearsBetween([DATE], V{enrollment_date})" )


# Negate all conditions of a PI filter
negate_filter <- function(x, go) {
  if (go==TRUE){
  str_replace_all(x, c("\\) > 0" = ") == 0", "\\} ==" = "} !=",  "d2:h" ="!d2:h"))
  } else { x }
}

# Build a range for numeric-based filters based on the inputed values
buildRange <-function(min, max, length){
  
  rangev1<-round(seq(min, max, length.out=length+1), 2)
  
  tibble(rangev1) %>%
    mutate(rangev2=lead(rangev1, 1)) %>%  
    mutate(NUMERICVAL=TRUE) %>% 
    filter(!is.na(rangev2))
  
  }


# collapse in the PI cross filters

# collapse_filter<-function(x, pi_filter) { 
#     as_tibble(x) %>% 
#       select(all_of(pi_filter)) %>% 
#       unique() %>% 
#       unlist() %>% 
#       str_c(collapse=' && ')
#   }
#   
# # collapse the PI names
# collapse_names<-function(x, de_teia_name, pi_value, option_name) { 
#     as_tibble(x) %>% 
#       select(all_of(c(de_teia_name, pi_value, option_name))) %>% 
#       distinct() %>% 
#       mutate(pi_name=if_else(is.na(option_name),
#                              paste0(str_trunc(de_teia_name, 20, "right")),
#                              paste0(str_trunc(de_teia_name, 20, "right"), ": ",
#                                     option_name))) %>% 
#       select(pi_name) %>%
#       unique() %>%
#       unlist() %>% 
#       str_c(collapse=' + ')
#     
#   }

#create pretty data tables for data elements
makeDT_DE <-function(x, preselections){
    DT::datatable(x,
                  filter = 'bottom',
                  escape = FALSE,
                  rownames = FALSE,
                  width= "100%",
                  selection= list(selected = preselections),
                  extensions = c('Buttons'),
                  options = list(
                      pageLength = 5,
                      dom = 'Blfrtip', 
                      buttons = c('colvis', 'excel')))
}

#create pretty data tables for PI
makeDT_PI <-function(x, HIDDEN_COLS, preselections){
    DT::datatable(x,
                  filter = 'bottom',
                  escape = FALSE,
                  rownames = FALSE,
                  width= "100%",
                  selection= list(selected = preselections),
                  editable = TRUE,
                  extensions = c('Buttons'),
                  options = list(
                      pageLength = 10,
                      dom = 'Blfrtip', 
                      buttons = c('colvis', 'excel'),
                      columnDefs = list(list(targets=HIDDEN_COLS,
                                             visible=FALSE))))
}


## Function for inputting the PI data into the JSON template

pi_printer<-function(template, type, program_id, pi_name, pi_code, pi_filter){
  
  if (!file.exists(here("pi_template.json"))){
    return(NULL)
  }else{
    
    template<-fromJSON(here("pi_template.json"))
    
    #reassign period boundary
    pluck(template, "analyticsPeriodBoundaries","boundaryTarget")<-paste0(type, "_DATE")
    pluck(template, "analyticsType")<-paste0(type)
    
    #reassign program and others
    pluck(template, "program","id")<-program_id
    pluck(template, "expression")<-paste0("V{",str_to_lower(type),"_count}")
    pluck(template, "filter")<-pi_filter
    pluck(template, "name")<-pi_name
    pluck(template, "shortName")<-pi_code
    pluck(template, "publicAccess")<-"rw------"
    
    #remove stuff from template we dont need
    template[c("id")]<-NULL
    template[c("translations")]<-NULL
    template[c("userGroupAccesses")]<-NULL
    template[c("user")]<-NULL
    template[c("lastUpdatedBy")]<-NULL
    pluck(template, "analyticsPeriodBoundaries","id")<-""
    

    return(template)
    #now output

      }

}



##################### UI #################

ui <- dashboardPage(skin="purple",
       dashboardHeader(title = "P.I. Generator",
                      tags$li(a(href = 'https://github.com/iambodo/dhis2_pi_generator', #github link
                                icon("github"),
                                title = "Github"),
                                class = "dropdown")),
        dashboardSidebar(
        
        # Ask for base url 
        textInput(inputId = "baseurl", "DHIS2 URL", value = "https://play.dhis2.org/2.35.1/"),
        
        # Ask for username
        textInput(inputId = "username", "DHIS2 username, e.g. 'admin'", value = "admin"),
        
        #Ask for password
        passwordInput(inputId = "password", "Password for this username", value = "district"),
        
        #login button
        actionButton(inputId = "login", label = "Log-In & Load Metadata") 
        
        # downloadButton("downloadAllData", "Download As CSV")

        ),
    
    dashboardBody(
        
        # Boxes need to be put in a row (or column)
        fluidRow(
            box(selectizeInput(inputId = "program_select", "1 - Select Program(s)",
                           choice="", multiple=TRUE), width=12)
            ),
        fluidRow(
            tabBox(title="2 - Tracker Data Elements & Attributes to Convert",
                   side = "right", 
                   tabPanel(id="tab2", title="Data Points",
                            DTOutput("contents")),
                   tabPanel(id="tab1", title="Filter Options",
                          box(width=6,
                            radioButtons(inputId = "pi_type", "Choose PI Type / Expression / Period Boundaries",
                                         choices = list("EVENT" = "EVENT", 
                                                        "ENROLLMENT" = "ENROLLMENT"),
                                         selected = "EVENT"),
                            checkboxInput(inputId="negate_pi_filter", 
                                          "!Negate All Filter Conditions", value=FALSE),
                            textInput(inputId="other_filter",
                                      "Other text to append to filter?"),
                            helpText("Use this if you have already have a complex filter to add as a base")
                            ),
                          box(width=6,  
                            # br(),
                            tags$strong("Numeric Ranges"),
                            radioButtons("useRange",
                                         "How to filter numeric data types?",
                                         choices = list("d2:hasValue()"=FALSE, 
                                                        "Custom Numeric Range"=TRUE), 
                                         selected = FALSE),
                            # column(width=3,
                            numericInput(inputId="numRangeMin",
                                         "Numeric Range Min", value=0, min=0),
                            # column(width=3,
                            numericInput(inputId="numRangeMax",
                                         "Numeric Range Max", value=100, min=0),
                            # sliderInput(inputId="numRange",
                            #             "Numeric Range Max and Min",
                            #             min=0, max=100, value=c(0, 100)),
                            sliderInput(inputId="numBins",
                                        "Numeric range bins:",
                                        min = 1,  max = 7, value = 2),
                            selectInput(inputId="dateRanges","How do you want to handle date or age values?",
                                        choices = dateRangeChoices)
                          )
                   ),
                width=10),
            box(title="Select Data Elements & Attributes to Convert to PI Filters",
                checkboxInput(inputId = "selectAllCheck", 
                              "All Data Elements & Attributes in Table At Left", 
                              value = FALSE), 
                textOutput("DE_selected_count"),
                br(),
                tags$strong("Combo Filters"),
                checkboxInput(inputId = "comboFilters", 
                              "Combination Data Element & Attribute Filters", 
                              value = FALSE), 
                selectInput("comboM", 
                             "Value Count in Each Combo Filter (Max 5)", 
                             selected = 2, choices=c(2:5)),
                radioButtons("comboJunction",
                             "Combo Filter Junction",
                             choices = list(" && ", " || "), 
                             selected = " && ", inline=TRUE), 
                width=2),
            ),
        fluidRow(
            box(title="3 - Data Element & Attribute Values for Program Indicator Filters",
                 DTOutput("piTable"), 
            width=10),
            box(title="Select Values to Convert to PI",
                checkboxInput(inputId = "selectPI", 
                              "All Values in Table At Left", 
                              value = FALSE),
                textOutput("PI_selected_count"),
                helpText("Hint: You can edit cells within the PI table before downloading"),
                downloadButton("downloadAllData", label="Download PI (JSON)"), 
            width=2)
            )
        )
    )


######## SERVER ########

server <- function(input, output, session) {
    
    
    
    #set login status
    login_status <- eventReactive(input$login, {
        loginDHIS2(trailing_backslash(input$baseurl), 
                   input$username, input$password)})
  
    # if logged in then download and manipulate metadata through API
    observeEvent(input$login, {
        if (login_status() == FALSE){
            showNotification("Error! Could not log in", type="error")
        }
        
        if (login_status() == TRUE){
            
            showNotification("Logged in! Loading data...", type="message") 
    
            ### for json upload
            # 
            # inFile <- input$myJSON
            # 
            # if (is.null(input$myJSON))
            #     return(NULL)
            # 
            # data<-jsonlite::read_json(inFile$datapath, flatten=TRUE)
            # 
    
         getData <- eventReactive(input$login, { 
                if(login_status() == TRUE){
                    
                    
            ######metadata extract ######
                    url3<-paste0("api/metadata.json?fields=id,name,domainType,code,valueType,optionSet&fields=program,programStageDataElements[section,dataElement[id]],",
                                 "programTrackedEntityAttributes[name,trackedEntityAttribute[id]]&dataElements=true&programStages=true&programs=true&optionSets=true&options=true&programs=true&trackedEntityAttributes=true")
                    url<-paste0(input$baseurl,url3)
                    
                    metadata<-fromJSON(content(GET(url), type="text", encoding = "UTF-8"))
                    

                    programs <- as_tibble(metadata$programs) %>% 
                        rename("program"=name) %>% 
                        select(program,id)
                    
                
                #teia with optionsets
                    teia_os<-metadata$trackedEntityAttributes %>% 
                        as_tibble() %>% 
                        jsonlite::flatten() %>% 
                        as_tibble() %>% select(-code)
                    
                    
                    teia <- metadata$programs %>%
                        unnest_longer(col=programTrackedEntityAttributes) %>% 
                        as_tibble() %>% 
                        jsonlite::flatten() %>% 
                        as_tibble() %>% 
                        rename("program"=name) %>% 
                        select(program, "program.id"=id,
                               "de_teia_id"=programTrackedEntityAttributes.trackedEntityAttribute.id)  %>% 
                        mutate(stage="ATTRIBUTE", id="") %>%  
                        select(stage, id, program.id, de_teia_id, program)
                    

                    
                    program_stages <- metadata$programStages %>%
                        as_tibble() %>% 
                        unnest_longer(col=programStageDataElements) %>% 
                        jsonlite::flatten() %>% 
                        as_tibble() %>% 
                        left_join(programs, by=c("program.id"="id")) %>% 
                        rename("stage"=name,
                               "de_teia_id"=programStageDataElements.dataElement.id) %>% 
                        #add the attributes like theyre a separate stage
                        bind_rows(teia)
                    


                    #convert DE and keep only tracker
                    de <-metadata$dataElements %>%
                    #cant filter for just tracker DE in 1 metadata API query... will fix later
                        filter(domainType=="TRACKER") %>% 
                        jsonlite::flatten() %>% 
                        as_tibble() %>% 
                        select(-domainType, -code) %>% 
                    #add the attributes with optionsets underneath the list of DE
                        bind_rows(teia_os) %>%
                        select("de_teia_name"=name, everything())
                      
                    
                    ## options
                    options<-metadata$options %>% 
                        as_tibble() %>% 
                        filter(name!="-" & !is.na(name)) %>% 
                        jsonlite::flatten() %>% 
                        as_tibble() %>% 
                        select("option_id"=id, "option_name"=name, everything() ) 
                    
                    
                    #option sets
                    os<-metadata$optionSets %>% as_tibble() %>% 
                        rename("optionSet"=name) %>% select(-valueType, -code) %>% 
                        filter(optionSet!="-" & !is.na(optionSet))
                    
                    

                    options_full<-inner_join(os, options, by=c("id"="optionSet.id"))
                    
                    

                    #merge with de
                    de_options<-left_join(de, options_full, by=c("optionSet.id"="id"))



                    # binary options
                    yesno<-tribble(
                        ~valueType, ~pi_values,
                        "TRUE_ONLY", '1',
                        "BOOLEAN",   '1',
                        "BOOLEAN",   '0'
                    )
                    

                        
                    # Now merge with program stage DE to output as full table
                    de_full <- left_join(program_stages, de_options, 
                                         by=c("de_teia_id"="id")) %>% 
                        as_tibble() %>% 
                        left_join(yesno, by="valueType") %>% 
                        mutate(pi_values=case_when(
                            !is.na(pi_values) ~ pi_values,
                            !is.na(option_name) ~ str_c("'", code, "'"))) %>%
                        mutate(NUMERICVAL=if_else((is.na(optionSet.id) & 
                                               valueType %in% c("NUMBER",
                                                                "INTEGER",
                                                                "INTEGER_POSITIVE",
                                                                "DATE",
                                                                "AGE")),TRUE,FALSE)) %>% 
                        select(program, stage, "stage_id"=id, NUMERICVAL, everything() ) 
                        


                    #make a smaller list that doesnt overwhelm for Table 1
                    de_condensed<-de_full %>% 
                        select(program, stage, de_teia_name, valueType, de_teia_id) %>% 
                        distinct()
                    
    
                    #list together for output
                        de_dfs<-list(
                            "de1"=de_full,
                            "de2"=de_condensed,
                            "hidCols"=hidCols
                        )
            
            showNotification("processed data", type="message", duration=5) 
                        
                    
                return(de_dfs)    

                }})
            
        ##once metadata downloaded then change the program selector options
         
      ##### SELECTOR OPTIONS ####    
         
         observe({
                if(login_status() == FALSE  | is.null(getData() )){
                    updateSelectizeInput(session, "program_select",
                                         choices = c("Log In") )                  }

                if(login_status() == TRUE & !is.null(getData())){
                    
                    updateSelectizeInput(session, "program_select",
                                         choices = unique(getData()$de2$program) )
                }
            })

# create df for simple table 1 based on program selection
        program_table<-reactive({
                
                getData()$de2 %>%  
                as_tibble() %>% 
                filter(program %in% input$program_select)
            })
    
  #select All DE rows
    selected_all_DE<-reactive({
          rows<-if (input$selectAllCheck) {

            seq(1, nrow(program_table() ))
            # c(1:length(program_table()$de_teia_id ))

          } else {
            c(NULL)
          }
          return(rows)
        })
    
    
    # #select All DE rows
    # selected_all_DE<-eventReactive(input$selectAllCheck,
    #                                
    #     if(length(input$contents_rows_all)>0){
    #       
    #       input$contents_rows_all
    #     
    #   } else {
    #     
    #     c(NULL)    })

    
 # render table 1
 output$contents <- renderDT(
   
                makeDT_DE(program_table() , selected_all_DE())
                
                            )
        } 
        
    #selections from table 1 - use this to filter table 1 (PI options)
program_rows<-reactive({
    if (is.null(input$contents_rows_selected) & input$selectAllCheck==FALSE) {
        return(NULL)
    } 
    # if you check the select all box, move over all rows from filtered DE
    if(input$selectAllCheck==TRUE & length(input$contents_rows_all)>0) {
        program_table() %>% 
            slice(input$contents_rows_all)
    }
    else {
        program_table() %>% 
            slice(input$contents_rows_selected)
    }
        })

#count of selected rows
output$DE_selected_count<-renderText({
  
  if (!is.null(program_rows() )) {
    
  str_c("Selected ", length(input$contents_rows_selected), " DE/TEIA")
    
  } 
  else {
    c("Selected 0 DE/TEIA")
  }
    
  })


## If the choices for PI combos is more than the number of PI selected, it'll crash!
observe({
  if (!is.null(program_rows() ) & length(input$contents_rows_selected) < input$comboM){
    updateSelectInput(session, "comboM",
                       selected = 2) }
})



# build more complex table 2 based on table 1 selection
pi_table_data <-reactive({
    
    if (is.null(program_rows()) ) {
      
        dat<-as_tibble()
        return(dat)

        
    } else {
        
    # numeric ranges for PI filters, if chosen
      if(input$useRange==TRUE){
      ranges<-buildRange(input$numRangeMin, input$numRangeMax, input$numBins)
      }else{
        ranges<-tibble(NUMERICVAL=as.logical(FALSE),
                       rangev1=as.numeric(),
                       rangev2=as.numeric())
      }
      
      
      
      # filter data elements from program row selection, and build filters
      dat<-getData()$de1 %>% 
            as_tibble() %>% 
        # filter down to selected DE and then generate names + filter
            filter(program %in% program_rows()$program) %>% 
            filter(stage %in% program_rows()$stage) %>% 
            filter(de_teia_id %in% program_rows()$de_teia_id) %>% 
            left_join(ranges, by="NUMERICVAL") %>% 
            # mutate(pi_values=if_else(NUMERIC!="", as.character(ranges), pi_values)) %>% 
            mutate(pi_filter= if_else((NUMERICVAL==TRUE & input$useRange==TRUE),
                build_pi_filter_num(stage_id, de_teia_id, rangev1, rangev2, input$pi_type,
                                    valueType, input$dateRanges),
                build_pi_filter(stage_id,de_teia_id, 
                                              pi_values,
                                              input$pi_type))) %>% 
            mutate(pi_filter=negate_filter(pi_filter, input$negate_pi_filter)) %>% 
            mutate(pi_values=if_else((NUMERICVAL==TRUE & input$useRange==TRUE),
                                     str_c(rangev1, "_", rangev2), pi_values)) %>% 
            select(-starts_with("range"), -NUMERICVAL)
      
      
      ## combo filters!! if 1 program chosen, multiple DE selected, and checkbox clicked
      
      if (input$comboFilters==TRUE & n_distinct(dat$program.id)==1 & 
                         length(input$contents_rows_selected) >= input$comboM & nrow(program_rows()) >1 ) { 
        
        program_alias<-dat$program.id %>% unique()

        #first combine the PI filters
        dat1<-dat %>%
            pull(pi_filter) %>%
            combn(., m=as.numeric(input$comboM)) %>% ##where we set the combo count in input 
            as.data.frame() %>% 
            tibble::rownames_to_column() %>% 
            pivot_longer(-rowname) %>% 
            pivot_wider(names_from=rowname, values_from=value, names_prefix=c("val")) %>% 
            tidyr::unite(pi_filter, -name, sep=input$comboJunction) %>%  
            select(pi_filter) 
          
          
          #then build the names and code
          dat2<-dat %>% 
            select(all_of(c("de_teia_name", "pi_values", "option_name","pi_filter"))) %>% 
            distinct() %>% 
            mutate(pi_name=case_when(
              is.na(option_name) &  is.na(pi_values) ~ paste0(str_trunc(de_teia_name, 30, "right")),
              is.na(option_name) & !is.na(pi_values) ~ paste0(str_trunc(de_teia_name, 30, "right"), ": ", pi_values),
              !is.na(option_name) ~ paste0(str_trunc(de_teia_name, 30, "right"), ": ", option_name))) %>%
            pull(pi_name) %>%
            combn(., m=as.numeric(input$comboM)) %>% 
            as.data.frame() %>% 
            tibble::rownames_to_column()  %>% 
            pivot_longer(-rowname) %>% 
            pivot_wider(names_from=rowname, values_from=value, names_prefix=c("val")) %>% 
            tidyr::unite(pi_name, -name, sep=" + ") %>%  
            select(pi_name) %>% 
            rowid_to_column() %>% 
            mutate(pi_code=str_c(str_trunc(str_to_upper(pi_name), 40, "right"),"_COMBO_",rowid)) %>% 
            mutate(program.id=as.character(program_alias)) %>% select(-rowid)
            
            #finally merge the two (they'll be the same length of rows)
            dat<-bind_cols(dat2, dat1)
          
          
            # #super annoying but to hide columns in the DT we do this
            # df <- data.frame(matrix(ncol = 8, nrow = 1))
            # x <- c("stage_id", "program.id", "de_teia_id","code","pi_values",
            #        "program", "optionSet.id", "option_id")
            # colnames(df) <- x
            # 
            # 
            # dat<-bind_cols(dat, df)
            
      ##ATTEMPT 2
          # mutate(pi_name=collapse_names(dat, "de_teia_name", "pi_values", "option_name")) %>% 
          # mutate(pi_name=str_to_sentence(pi_name)) %>%
          # mutate(pi_code=str_to_upper(pi_name)) %>% 
          # mutate(pi_filter=collapse_filter(dat, "pi_filter")) 

        ##ATTEMPT 1
          #   filter(de_teia_name.deA!=de_teia_name.deB) %>% 
          #   mutate(pi_name=if_else((is.na(option_name.deA) | is.na(option_name.deB)),
          #                          paste0(shorten_name(de_teia_name.deA, 20), "+", #first deteia
          #                                 shorten_name(de_teia_name.deB, 20)),     #second deteia
          #                          paste0(shorten_name(de_teia_name.deA, 15), ": ", 
          #                                 shorten_name(option_name.deA, 15), "+",
          #                                 shorten_name(de_teia_name.deB, 15), ": ", 
          #                                 shorten_name(option_name.deB, 15))  )) %>%
          #   mutate(pi_filter=str_c(pi_filter.deA, " && ", pi_filter.deB)) %>% 
          #   mutate(pi_name=str_to_sentence(pi_name)) %>%
          #   mutate(pi_code=str_to_upper(pi_name)) %>%
          # #strip of eveerything except essentials
            # select(-contains(".deB"))
        
        return(dat)
        
      } else {
        
        #finally, format the name and code of PI, for both combo and single-filter PI types
        dat<-dat %>%
          mutate(pi_name=case_when(
            is.na(option_name) &  is.na(pi_values) ~ paste0(str_trunc(de_teia_name, 30, "right")),
            is.na(option_name) & !is.na(pi_values) ~ paste0(str_trunc(de_teia_name, 30, "right"), ": ", pi_values),
           !is.na(option_name) ~ paste0(str_trunc(de_teia_name, 30, "right"), ": ", option_name))) %>%
          mutate(pi_name=str_to_sentence(pi_name)) %>%
          mutate(pi_code=str_trunc(str_to_upper(pi_name), 50, "right")) %>% 
          mutate(pi_filter=str_c(pi_filter," ",input$other_filter))
          
        
        return(dat)
      }
    

    }
  

    
})



## If the max range is empty it'll crash!  
observe({
  if (!is.null(program_rows() ) & (is.na(input$numRangeMax))){
    
    updateNumericInput(session, "numRangeMax",
                      value = input$numRangeMin) }
  
})

## If the min range is empty, itll crash! 
observe({
  if (!is.null(program_rows() ) & is.na(input$numRangeMin)){
    
    updateNumericInput(session, "numRangeMin",
                       value = input$numRangeMax) }
  
})




#hide majority of columns from column 2 by default
hidCols<-reactive({
  
  which(colnames(pi_table_data() ) %in%
          c("stage_id", "program.id", "de_teia_id","code","pi_values",
            "program", "optionSet.id", "option_id"))-1L
    
})


#select All PI rows
selected_all_PI<-reactive({
  rows<-if (input$selectPI) {
    
    seq(1, nrow(pi_table_data() ))

  } else {
    c(NULL)
  }
  return(rows)
})




#render table 2
 output$piTable <- renderDT(
     
     as_tibble(pi_table_data() ) %>% 
             makeDT_PI(HIDDEN_COLS = hidCols(),
                       preselections = selected_all_PI() )
 )



 #count of selected rows
 output$PI_selected_count<-renderText({
   
   if (!is.null(pi_table_data()  )) {
     
     str_c("Selected ", length(input$piTable_rows_selected), "PI")
     
   } 
   else {
     c("Selected 0 PI")
   }
   
 })

 
 
  
  ###Tracking Changes in the Table Edits -- Creating a proxy###
  ###lifted from 
  ###https://stackoverflow.com/questions/58595096/editing-multiple-cells-in-a-datatable-in-shiny
  
  rvs <- reactiveValues(
    data = NA #dynamic data object
  )
  
  observe({
    rvs$data <- pi_table_data()
  })
  
  proxy = dataTableProxy('piTable')
  observe({
    DT::replaceData(proxy, rvs$data, rownames = FALSE, resetPaging = FALSE)
  })
  
  observeEvent(input$piTable_cell_edit, {
    rvs$data <- editData(rvs$data, input$piTable_cell_edit, rownames = FALSE)
  })

  
  #### DOWNLOAD ####
  
  # PI Download selection
  #selections from table 2
  pi_rows<-reactive({
    if (is.null(input$piTable_rows_selected) & input$selectPI==FALSE) {
      return(NULL)
    } 
    # if you check the select all box, move over all rows from filtered PI
    if(input$selectPI==TRUE & length(input$piTable_rows_all)>0) {
      rvs$data %>% 
        slice(input$piTable_rows_all)
    }
    else {
      rvs$data %>% 
        slice(input$piTable_rows_selected)
    }
  })
  
  
  #download handler and other text
  output$downloadAllData <- downloadHandler(filename = function() {
    paste0("program_indicators_",Sys.Date(),".json")
  },
  content = function(file) {
    
    if (is.null(pi_rows()) ){
      
      testpi<-list("programIndicators"=NULL)
      write_json(testpi, file, pretty=TRUE, auto_unbox=TRUE)
      
      
    } else {

    #output from template->PI conversion
    output_pi<-list()
    for (i in 1:nrow(pi_rows() )){
      output_pi[[i]]<-pi_printer(template, input$pi_type, 
                              pi_rows()$program.id[i], pi_rows()$pi_name[i],
                              pi_rows()$pi_code[i], pi_rows()$pi_filter[i])
    }
    
    testpi<-list("programIndicators"=output_pi)
    
    write_json(testpi, file, pretty=TRUE, auto_unbox=TRUE)
    }
    
      }
  )
  
  
  

    }) }



shinyApp(ui, server)



