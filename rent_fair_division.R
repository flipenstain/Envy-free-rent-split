library(shiny)
library(dplyr)

calculateFairDivision <- function(bids, total_rent) {
  
  # Create a matrix to track room assignments
  assignments <- matrix(0, nrow = nrow(bids), ncol = ncol(bids) - 1)
  
  
  # Iterate over rooms and assign them to the highest bidder
  for (room in 1:(ncol(bids) - 1)) {
    for (person in 1:nrow(bids)) {
      max_bid <- max(bids$Room1[person], bids$Room2[person], bids$Room3[person])
      
      # Find the first person with the highest bid
      if (bids[, room + 1][person] == max_bid) {
        #Is room available?
        if (colSums(assignments)[room] < 1) {
          assignments[person, room] <- 1
        }
      }
    }
  }
  # Assign the room that is over 
  for (i in 1:nrow(assignments)) {
    # Check if the row sum is 0
    if (sum(assignments[i, ]) == 0) {
      # Find the columns with a sum of 0
      zero_columns <- which(colSums(assignments) == 0)
      
      # Check if any of those columns are non-zero in the current row
      non_zero_in_row <- any(assignments[i, zero_columns] != 0)
      
      if (!non_zero_in_row) {
        # If no non-zero values in the selected columns, set 1
        assignments[i, zero_columns] <- 1
      }
    }
  }
  
  # Calculate surplus
  
  sum_of_max_values <- sum(apply(bids[,-1], 2, max))
  surplus <- sum_of_max_values - total_rent
  
  #divide surplus
  decrease <- -surplus/3
  
  decrease
  
  for (person in 1:nrow(bids)) {
    for (room in 1:ncol(assignments)) {
      if (assignments[person, room] == 1) {
        bids[, room + 1][person] <- bids[, room + 1][person] + decrease
      }
    }
  }
  
  result <- list()
  
  for (i in 1:nrow(assignments)) {
    person <- bids$Person[i]
    room <- colnames(bids[-1])[which(assignments[i,] == 1)]
    value <- bids[i, room]
    #cat(paste(person, "gets", room, "for", round(value)), "\n")
    result[[i]] <- paste(person, "gets", room, "for", round(value))
  }
  
  return(result)
}



ui <- fluidPage(
  titlePanel("Divide Your Rent Fairly"),
  
  mainPanel(
    fluidRow(
      column(6,
             textInput("totalRent", "What's your total rent?")
      ),
      column(6,
             selectInput("numPeople", "How many of you are there?", choices = 2:5)
      )
    ),
    
    fluidRow(
      column(6,
             uiOutput("namesSection")
      ),
      column(6,
             uiOutput("roomsSection")
      )
    ),
    
    fluidRow(
      column(12,
             uiOutput("fairDivision")
      )
    ),
    
    # Add a button to trigger the fair division calculation
    actionButton("calculate", "Calculate Fair Division"),
    
    # Output the fair rent division results here
    tableOutput("resultTable")
  )
)

server <- function(input, output, session) {
  # Initialize the Rooms_evaluation dataframe
  Rooms_evaluation <- reactiveVal(data.frame())
  
  # Dynamic generation of inputs for names
  output$namesSection <- renderUI({
    numPeople <- as.integer(input$numPeople)
    
    nameInputs <- lapply(1:numPeople, function(i) {
      defaultName <- paste("Roommate", i)
      textInput(paste0("name", i), label = NULL, value = defaultName)
    })
    
    tagList(
      h5("Your names are:"),
      nameInputs
    )
  })
  
  # Dynamic generation of inputs for rooms
  output$roomsSection <- renderUI({
    numPeople <- as.integer(input$numPeople)
    
    roomInputs <- lapply(1:numPeople, function(i) {
      defaultRoom <- paste("Room", i)
      textInput(paste0("room", i), label = NULL, value = defaultRoom)
    })
    
    tagList(
      h5("And the rooms in your apartment are:"),
      roomInputs
    )
  })
  
  # Dynamic generation of fair division preferences (using sliders)
  output$fairDivision <- renderUI({
    totalRent <- as.numeric(input$totalRent)
    
    validate(
      need(!is.na(totalRent) && totalRent > 0, "Please enter a valid total rent.")
    )
    
    numPeople <- as.integer(input$numPeople)
    
    preferenceSliders <- lapply(1:numPeople, function(i) {
      roomSliders <- lapply(1:numPeople, function(j) {
        sliderInput(
          paste0("preference", i, "_room", j),
          label = paste(input[[paste0("room", j)]]),
          min = 0,
          max = totalRent,
          value = 0,
          step = 1
        )
      })
      
      tagList(
        h5(paste(input[[paste0("name", i)]], "rooms evaluation")),
        div(style = "display: flex;", roomSliders)
      )
    })
    
    tagList(
      h5("Fair division preferences:"),
      preferenceSliders
    )
  })
  
  # Fair division algorithm
  observeEvent(input$calculate, {
    
    
    numPeople <- as.integer(input$numPeople)
    
    # Gather preferences
    preferences <- sapply(1:numPeople, function(i) {
      roomPrefs <- sapply(1:numPeople, function(j) {
        input[[paste0("preference", i, "_room", j)]]
      })
      list(roomPrefs)
    })
    
    cat("\n")
    d <- data.frame(
      Person = c(input[[paste0("name", 1)]], input[[paste0("name", 2)]], input[[paste0("name", 3)]]),
      Room1 = c(input[[paste0("preference", 1, "_room", 1)]], input[[paste0("preference", 1, "_room", 2)]], input[[paste0("preference", 1, "_room", 3)]]),
      Room2 = c(input[[paste0("preference", 2, "_room", 1)]], input[[paste0("preference", 2, "_room", 2)]], input[[paste0("preference", 2, "_room", 3)]]),
      Room3 = c(input[[paste0("preference", 3, "_room", 1)]], input[[paste0("preference", 3, "_room", 2)]], input[[paste0("preference", 3, "_room", 3)]])
    )
    cat(unlist(d))
    cat("\n")
    r <- calculateFairDivision(d, 1000)
    
    cat(unlist(r))
    
    Rooms_evaluation(
      unlist(r)
    )
  })
  
  # Output the Rooms_evaluation dataframe as a table
  output$resultTable <- renderTable({
    Rooms_evaluation()
  })
}

shinyApp(ui, server)
