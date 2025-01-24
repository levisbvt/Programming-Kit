exam_cheat_sheet <- function(week) {
  week_1 <- quote({
    #R1
    #WRITE YOUR CODE INTO THIS CELL (~1 line of code below this comment)
    getwd()

    #R2
    #WRITE YOUR CODE INTO THIS CELL (~1 line of code)
    setwd("")

    #R3
    #WRITE YOUR CODE INTO THIS CELL (~2 lines of code)
    list.files()
    ?list.files

    #R4
    #WRITE YOUR CODE INTO THIS CELL (~3 lines of code + 1 comment)
    hours_sleep = 5
    reported_happiness = "3"
    hours_sleep + reported_happiness
    #cannot add it because the text is non-numeric

    #R5
    #WRITE YOUR CODE INTO THIS CELL (~2 new lines of comments)
    rm(list = ls())
    a <- b <- ab <- ba <- c <- 1
    #first, everything gets removed
    #then, five variables get created, each with value 1

    #R6
    #WRITE YOUR CODE INTO THIS CELL (~3 lines of code)
    classic_string = "Hello world."
    classic_string
    print("Hello world.")

    #R7
    #WRITE YOUR CODE INTO THIS CELL (~1 line of code + 1 comment)
    vec = c(2, FALSE, -1.243, "test")
    #they are all strings

    #R8
    #WRITE YOUR CODE INTO THIS CELL (change SECOND line of code)
    participant_ages <- c(21,26,27,19,49,NA)
    mean(participant_ages, na.rm = TRUE)

    #R9
    #WRITE YOUR CODE INTO THIS CELL (~3 lines of code)
    install.packages("fortunes")
    library(fortunes)
    fortune(5)
    fortunes("may") # This is an alternative solution

    #R10
    #WRITE YOUR CODE INTO THIS CELL (1 line of comments)
    birthday_guests <- 8
    cake_slices <- 5
    my_bool <- birthday_guests < cake_slices
    cake_slices / my_bool
    #the last line contains a division by zero

    #R11
    #WRITE YOUR CODE INTO THIS CELL (~2 lines of code)
    grades <- sort(sample(1:10, 100, replace = T))
    grades[c(1, 20, 80)]

    #R12
    #WRITE YOUR CODE INTO THIS CELL (~1 line of code)
    length(LETTERS)

    #R13
    #WRITE YOUR CODE INTO THIS CELL (~1 line of comments)
    letters2 = letters
    letters2 == letters
    letters2 <- letters
    letters -> letters2
    #the second line contains a test rather than an assignment

    #R14
    #WRITE YOUR CODE INTO THIS CELL (edit second line)
    fruits <- c("b.n.n.s",".pples")
    corrected_fruits <- gsub("\\.", "a", fruits)
    corrected_fruits <- gsub(".", "a", fruits, fixed = TRUE) # This is an alternative solution

    #R15
    #WRITE YOUR CODE INTO THIS CELL (~1-2 lines of code; don't edit first line)
    horrible_numbers <- factor(c("25", pi, "NA", 1))
    mean(as.numeric(as.character(horrible_numbers)), na.rm = T)

    #R16
    #WRITE YOUR CODE INTO THIS CELL (~4 lines of code)
    a = 4
    b = 5
    d = 70
    radians = d * pi / 180
    sqrt(a^2 + b^2 - 2*a*b * cos(radians))

    #R17
    #WRITE YOUR CODE INTO THIS CELL (~2 lines of code)
    test = seq(1,100, 2)
    prod(test)

    #R18
    #WRITE YOUR CODE INTO THIS CELL (~3-5 lines of code)
    set.seed(42)
    rows = sample(1:10, 1)
    cols = sample(1:10, 1)
    my_matrix = matrix(NA, nrow = rows, ncol = cols)
    my_matrix = matrix(nrow = rows, ncol = cols) # This is an alternative solution
    dim(my_matrix)

    #R19
    #WRITE YOUR CODE INTO THIS CELL (~1-2 lines of comments)
    set.seed(1234)
    n_subjects <- 1325
    n_vars <- 201
    EEGData1 <- matrix(runif(n_subjects*n_vars), ncol=n_vars, nrow=n_subjects)
    EEGData2 <- matrix(runif(n_subjects*n_vars), n_subjects, n_vars)
    EEGData3 <- matrix(runif(n_subjects*n_vars), n_vars, n_subjects)
    EEGData4 <- matrix(ncol=n_vars, nrow=n_subjects, data=runif(n_subjects*n_vars))
    #the third is incorrect. one either has to get the order of arguments right or provide named arguments


    #R20
    #WRITE YOUR CODE INTO THIS CELL (edit ~1 line of code)
    set.seed(1234)
    rows <- 20 -> cols #bad style but fun
    M <- matrix(sample(c(T, F), rows*cols, replace = T), rows, cols)
    colSums(M)

    #R21
    #WRITE YOUR CODE INTO THIS CELL (~1 long line of code)
    animals_array <- array(
      c(
        "Cheetah", "Hyena",
        "Penguin", "Seal",
        "Rabbit", "Warthog",
        "Arctic Hare", "Musk Ox"
      ),
      dim = c(2, 2, 2),
      dimnames = list(
        Cuteness = c("Cute", "Not Cute"),
        Habitat = c("Savannah", "Arctic"),
        Diet = c("Carnivore", "Herbivore")
      )
    )

    #R22
    #WRITE YOUR CODE INTO THIS CELL (~2 new lines of code)
    relationship_matrix <- matrix(c("", "L", "L", "H", "H", "L", "", "L", "H", "H", "L", NA, "", "H", "H","H", "H", "H", "", "L", NA, "H", "H", "L", ""), nrow = 5, byrow = TRUE)
    rownames(relationship_matrix) <- c("Harry", "Hermione", "Ron", "Draco", "Snape")
    colnames(relationship_matrix) <- c("Harry", "Hermione", "Ron", "Draco", "Snape")

    relationship_matrix[3, 2] = "L"
    relationship_matrix["Snape", "Harry"] = "H"

    #R23
    #WRITE YOUR CODE INTO THIS CELL (edit only last two lines of code)
    relationship_matrix <- matrix(c("", "L", "L", "H", "H", "L", "", "L", "H", "H",
                                    "L", "L", "", "H", "H","H", "H", "H", "", "L", "complicated", "H", "H", "L",""),
                                  nrow = 5, byrow = TRUE)
    rownames(relationship_matrix) <- c("Harry", "Hermione", "Ron", "Draco", "Snape")
    colnames(relationship_matrix) <- c("Harry", "Hermione", "Ron", "Draco", "Snape")

    # my_diag <- diag(relationship_matrix)
    # relationship_matrix[my_diag] <- NA
    diag(relationship_matrix) <- NA


    #R24
    #WRITE YOUR CODE INTO THIS CELL (~1 line of code)
    rep(letters[1:5], each = 2)


    #R25
    #WRITE YOUR CODE INTO THIS CELL (~2-6 lines of code)
    lotr_characters <- data.frame(
      Name = c("Gollum", "Smeagol", "Sauron", "Frodo", "Shelob", "Samwise"),
      First_appearance = rep(c("The Hobbit", "LOTR"), each = 3),
      Morality = rep(c("Bad", "Good"), 3),
      Dating_appeal = seq(0,10,2)
    )

    #R26
    #WRITE YOUR CODE INTO THIS CELL (~1 additional line of code)
    set.seed(1234)
    students <- 50

    grades <- data.frame(1:students,matrix(sample(8:20,students*2,TRUE)/2,,2))

    names(grades) <- c('student_ID','assignment_average','exam')

    grades$final_grade = 0.4* grades$assignment_average + 0.6 * grades$exam

    #R27
    #WRITE YOUR CODE INTO THIS CELL (~1-3 lines of code)
    grades$passed = grades$final_grade >= 5.5 & grades$exam >= 5.5

    #R28
    #WRITE YOUR CODE INTO THIS CELL (~1 line of code)
    write.csv(InsectSprays, "mydata.csv", row.names = F)

    #R29
    #WRITE YOUR CODE INTO THIS CELL (~4 lines of code)
    library(statquotes)
    library(cowsay)
    quote = statquote()
    say(what = quote$text, by = "random")

    #R30
    #WRITE YOUR CODE INTO THIS CELL (~4 lines of code)
    df = read.csv("https://raw.githubusercontent.com/hannesrosenbusch/schiphol_class/master/Kaggle_Dataset_Mental_Disorders.csv")

    vec1 = df$Exhausted[df$Mood.Swing == "YES"]
    vec2 = df$Exhausted[df$Mood.Swing == "NO"]
    t.test(vec1, vec2)

    #Radv1
    #WRITE YOUR CODE INTO THIS CELL
    v1 = 1:3
    v2 = rep("hi", 4)
    v3 = sample(1:4, 2)
    v4 = rnorm(3)
    class(v1)

    #Radv2
    #WRITE YOUR CODE INTO THIS CELL
    mtcars[1,]
    mtcars[,"mpg"]
    mtcars[1,"mpg"]

    #Radv3
    #WRITE YOUR CODE INTO THIS CELL
    install.packages("stringr")
    stringr::str_count("bananas are great", "a")

    #Radv4
    #WRITE YOUR CODE INTO THIS CELL
    set.seed(123)
    df = data.frame(animals = rep(c("cat", "dog"), each = 10))
    df$IQ = (df$animals == "dog") * 8 + rnorm(nrow(df), 50, 5)

    #Radv5
    #WRITE YOUR CODE INTO THIS CELL
    #first I make some missing values
    mtcars$mpg[1:3] = NA

    #i index logically to change the right cells
    mtcars$mpg[is.na(mtcars$mpg)] = mean(mtcars$mpg, na.rm = T)

    #Radv6
    #WRITE YOUR CODE INTO THIS CELL
    book_info <- list(
      title = "The Great Gatsby",
      author = "F. Scott Fitzgerald",
      year = 1925
    )

    # A list can store values of different types

    #Radv7
    #WRITE YOUR COMMENT INTO THIS CELL
    #https://stackoverflow.com/questions/79227703/how-to-color-only-a-piece-of-a-bar-in-a-bar-plot
    #interesting because I am interested in R's plotting capabilities

    #Radv8
    #WRITE YOUR CODE INTO THIS CELL
    df = read.csv("https://raw.githubusercontent.com/hannesrosenbusch/schiphol_class/master/Kaggle_Dataset_Mental_Disorders.csv")
    plot(df$Sadness)
  })

  week_2 <- quote({
    #R1
    hist(sample(8:20, 40, replace = T) * 0.5)

    #R2
    df <- read.csv("https://raw.githubusercontent.com/hannesrosenbusch/PIPS_DATA/master/schiphol.csv")
    plot(df$DATE, df$TAVG)

    #R3
    library(titanic, warn.conflicts = FALSE)
    library(ggplot2, warn.conflicts = FALSE)
    ggplot(titanic_train, aes(fill= factor(Survived, labels = c("dead", "alive")), x=Sex))+
      geom_bar() +
      labs(fill = "How did it go?")

    #R4
    #i like theme_bw(). seems basic

    #R5
    plot(cars$speed, cars$dist,
         main = "Car Speed vs Stopping Distance",
         xlab = "Speed (mph)",
         ylab = "Stopping Distance (ft)",
         pch = 19,
         col = "darkred")
    abline(lm(cars$dist ~ cars$speed), col = "blue", lwd = 2)
    #added title
    #added axis labels with units
    #added regression line

    #R6
    library(ggplot2, warn.conflicts = FALSE)
    library(dplyr, warn.conflicts = FALSE)
    data.frame(ChickWeight) %>%
      group_by(Chick) %>%
      filter(Chick %in% c('1', '20', '5', '40', '19')) %>%
      summarise(max_weight = max(weight)) %>%
      mutate(chick = factor(Chick, levels=c('1', '20', '5', '40', '19'))) %>%
      ggplot() +
      geom_bar(aes(chick, max_weight), stat = "identity")

    #R7
    library(ggplot2, warn.conflicts = FALSE)
    library(dplyr, warn.conflicts = FALSE)
    data.frame(cars) %>%
      ggplot() +
      geom_point(aes(speed, dist)) +
      geom_smooth(aes(speed, dist))

    #R8
    library(ggplot2)
    library(dplyr)
    library(patchwork)
    par(mfrow = c(2,2))
    plot1 <- data.frame(ChickWeight) %>%
      filter(Chick %in% c('1', '20', '5', '40', '19')) %>%
      group_by(Chick) %>%
      summarise(max_weight = max(weight)) %>%
      mutate(chick = factor(Chick, levels=c('1', '20', '5', '40', '19'))) %>%
      ggplot() +
      geom_bar(aes(chick, max_weight), stat = "identity")

    plot2 <- data.frame(ChickWeight) %>%
      filter(Chick %in% c('1', '20', '5', '40', '19')) %>%
      mutate(chick = factor(Chick, levels=c('1', '20', '5', '40', '19'))) %>%
      ggplot() +
      geom_line(aes(Time, weight, color = chick)) +
      labs(color = "chick")

    plot1 + plot2


    #R9
    library(ggstatsplot)
    ggbetweenstats(data = ToothGrowth, x = supp, y = len)


    #R10
    library(plotly)
    df = read.csv("https://raw.githubusercontent.com/hannesrosenbusch/PIPS_DATA/master/body.csv")
    plot_ly(df, x = ~TotalHeight,
            y = ~HeadCircumference,
            z = ~ShoulderToWaist,
            opacity = 0.5,
            type="scatter3d", mode = "markers")

    #R11
    #might need gifski package
    library(cranlogs)
    library(ggplot2)
    library(gganimate)
    downloads <- cran_downloads(c("ggplot2", "plotly"), from = "2013-08-21", to = "2025-01-01")
    p <- ggplot(downloads, aes(date, count, color = package)) +
      geom_line() +
      theme_bw() +
      labs(y = "Package Downloads", title = "Package popularity over time")+
      transition_reveal(date)
    animate(p,  nframes = 100, fps = 10, width = 300, height = 300, end_pause = 70)
    anim_save("package_downloads.gif", animation = last_animation())

    #R12
    library(quantmod)
    getSymbols("AAPL", from = '2024-01-01',
               to = "2024-10-31",warnings = FALSE,
               auto.assign = TRUE)

    chartSeries(AAPL)
    #they made my phoneyy

    #R13
    library(tidyquant)
    plotstock <- function(stocksym="AAPL", year = "2023", filename = "mystock.png"){

      my_data <- getSymbols(stocksym, from = paste0(year,'-01-01'),
                            to = paste0(year,'-12-31'),warnings = FALSE,
                            auto.assign = FALSE)
      png(filename)
      print(plot(my_data[,1]))
      dev.off()
    }

    plotstock("AMZN", "2015")

    #R14
    #i had unnecessary lines in task 13
    # i used = instead of <- a bunch of times

    #R15
    #functions compares length of input string with some other string.
    #bad var names
    #no comments
    #only conditional return
    #mix of assignment operators
    #no indentation

    #R16
    matrix(1:9, 3, byrow = TRUE) * 1:3

    #R17
    #ctrl shift a or ctrl i


    #R18
    devtools::install_github("sctyner/memer")
    library(memer)
    library(dplyr)

    meme_get("DistractedBf") %>%
      meme_text_distbf("tidyverse", "new R users", "base R")

    #Radv1
    set.seed(42)
    plot(rnorm(1000), rnorm(1000), axes = FALSE, xlab = "", ylab = "", main = "", frame.plot = FALSE)

    #Radv2
    # i like animals
    cowsay::say("moo", "cow")

  })

  week_3 <- quote({
    #R1
    meryls_oscars = 3
    meryls_losses = 18
    best_actress = "Meryl Streep"
    if(best_actress == "Meryl Streep") {
      meryls_oscars <- meryls_oscars + 1
    } else {
      meryls_losses <- meryls_losses + 1
    }

    #R2
    box_hist <- function(numvec) {
      if (runif(1) > 0.5) {
        boxplot(numvec, main = "Boxplot")
      } else {
        hist(numvec, main = "Histogram")
      }
    }

    #R3
    # old function
    box_hist <- function(numvec) {
      if (runif(1) > 0.5) {
        boxplot(numvec, main = "Boxplot")
      } else {
        hist(numvec, main = "Histogram")
      }
    }

    # new function
    box_hist <- function(numvec) {
      if(runif(1) > 0.5) {boxplot(numvec, main = "Boxplot")}
      else {hist(numvec, main = "Histogram")}
    }

    #R4
    # the problem was that the code didn't account for conditions within conditions.
    # E.g., if the employee fails to predict sun, then the salary should decrease
    # as well as the notice should be printed to move the party outside.
    # I've adjusted the code to reflect all possibilities.

    predicted <- "sun"
    actual <- "sun"
    employee_salary <- 54000

    if (predicted == actual) {
      employee_salary <- employee_salary + 500
      if (actual == "sun"){
        print("Yay! Birthday moved outside!")
      } else if(actual == "high wind") {
        print("Boss blew off her bike into a canal.")
      }
      print(predicted); print(actual); print(employee_salary)
    } else if (predicted != actual) {
      employee_salary <- employee_salary - 500
      if (actual == "sun"){
        print("Yay! Birthday moved outside!")
      } else if(actual == "high wind") {
        print("Boss blew off her bike into a canal.")
      }
      print(predicted); print(actual); print(employee_salary)
    }

    #R5
    my_numbers <-  runif(100, min=-0.01, max=100)
    any_negative <-  FALSE
    if (length(which(my_numbers < 0)) > 0) {
      any_negative <- TRUE
    }

    #R6
    if(0 < as.numeric(format(Sys.time(), "%H"))+as.numeric(format(Sys.time(), "%M"))/60 &&
       as.numeric(format(Sys.time(), "%H"))+as.numeric(format(Sys.time(), "%M"))/60 < 5) {
      print("Go to sleep!")
    }

    #R7
    data7 <- read.csv(
      "https://raw.githubusercontent.com/hannesrosenbusch/schiphol_class/master/Kaggle_Dataset_Mental_Disorders.csv"
    )
    data7$mood.swing.b <- ifelse(data7$Mood.Swing == "YES", 1, 0)
    data7$suicidal.thoughts.b <- ifelse(data7$Suicidal.thoughts == "YES", 1, 0)
    data7$aggressive.response.b <- ifelse(data7$Aggressive.Response == "YES", 1, 0)
    data7$optimism.b <- as.numeric(substr(data7$Optimisim, 1, 1))
    data7$Suicide.Risk <- (
      data7$mood.swing.b * 6 + data7$suicidal.thoughts.b * 8 + data7$aggressive.response.b * 7 + (11 - data7$optimism.b) * 2
    ) / 4.1

    Outlying <- ifelse(data7$Suicide.Risk >= 8 | data7$Suicide.Risk <= 2, "Yes", "No")

    #R8
    for(i in 1:length(data7[1, ])) {
      if (colnames(data7[i]) == "Sexual.Activity") {
        print("Private information")
      } else {
        print(class(data7[1, i]))
      }
    }

    for(i in 1:length(data7[, 1])) {
      if (data7$Authority.Respect[i] == "NO" &
          data7$Aggressive.Response[i] == "YES") {
        print("Assessment postponed")
        break
      } else {
        print(data7$Patient.Number[i])
        print("assessed succesfully")
      }
    }

    #R9
    library(ggplot2)
    data9 <- data.frame(
      count = NA,
      co2_values = NA
    )

    cnt <- 0
    co2.store = NA

    while(cnt <= 1000) {
      co2.store <- rnorm(1, 100, 15)
      if (co2.store <= 120) {
        data9[cnt,2] <- co2.store
        data9[cnt,1] <- cnt
        cnt <- cnt + 1
      }
    }

    ggplot(data9, aes(x = co2_values)) + stat_bin(binwidth = 2)

    #R10
    weird_fibonacci <- c(0, 3)

    for(i in 3:10) {
      weird_fibonacci[i] <- weird_fibonacci[i-2]*3 + weird_fibonacci[i-1]
    }

    #R11
    bet <- 10
    bank <- 1000
    player.win <- 0
    house.win <- 0

    data11 <- data.frame(
      Bank = NA,
      PlayerWin = NA,
      HouseWin = NA
    )

    for(i in 1:1000) {
      throw <- round(runif(3, 1, 6), 0)
      if(all(throw == throw[1])) {
        player.win <- player.win + 10*bet
        house.win <- -10*bet
        bank <- bank + house.win
        data11[i,] <- c(bank, player.win, house.win)
      } else {
        player.win <- player.win - bet
        house.win <- bet
        bank <- bank + house.win
        data11[i,] <- c(bank, player.win, house.win)
      }
    }

    #R12
    these_numbers <- c(1, 3.1, 5.2, -1, 5.2)
    maximum <- numeric()
    for (i in 1:(length(these_numbers)-1)) {
      if (these_numbers[i+1] > these_numbers[i]) {
        maximum <- these_numbers[i+1]
      }
    }
    print(these_numbers[which(these_numbers == maximum)])

    #R13
    while(format(Sys.time(), "%Y") < "2040") {
      if (format(Sys.time(), "%M%S") == "30:00" | format(Sys.time(), "%M:%S") == "00:00") {
        print(Sys.time())
        Sys.sleep(1)
      }
    }

    #R14
    verhulst <- function(x, rate_in, cap) {
      rate_in*x*(cap-x)/cap
    }

    rabbits <- c(.001)
    rate <- 2
    capacity <- 1000

    for (time in 2:50) {
      rabbits[time] <- verhulst(rabbits[time-1], rate, capacity)
    }

    plot(rabbits, type='l', xlab='time', bty='n')

    #R15
    vec <- c(4, 50, 3)
    arr <- array(NA, vec)

    for (i in 1:vec[2]) {
      for (j in 1:vec[1]) {
        for (k in 1:vec[3]) {
          arr[j, i, k] <- rnorm(1, i, 1)
        }
      }
    }

    #R16
    # my 3d array from R15
    vec <- c(4, 50, 3)
    arr <- array(NA, vec)

    for (i in 1:vec[2]) {
      for (j in 1:vec[1]) {
        for (k in 1:vec[3]) {
          arr[j, i, k] <- rnorm(1, i, 1)
        }
      }
    }

    # calculating minimum values
    apply(arr, 1, min)

    #R17
    what_do <- switch(caught_fish,
                      large_trout = "eat",
                      small_trout = "return",
                      carp = ,
                      perch = "sell")
    print(what_do)

    # A: caught_fish = "shoe", the variable what_do = NULL.
    # This is because a "shoe" argument has not been defined in the switch()
    # function, and so it will return a NULL expression

    # B: Both carp and perch will evaluate to "sell". Perch because it is paired
    # with "sell", and carp because it's element is missing and so the switch()
    # function will evaluate the next non-missing element, which is perch.

    #R18
    mcfly <- function(my_list) {
      print("Great Scott! Erased from existence!")
      rm(list = my_list, pos = 1)
    }
    mcfly('mcfly')

    # rm() will remove whatever object is inputted as my_list. the pos argument
    # indicates from which environment the object is to be removed.
    # pos = 1 indicates the global environment of R.

    #R19
    t_tests <- replicate(100, t.test(rbeta(10, shape1=2, shape2=2), mu=.5), simplify = FALSE)
    vec <- numeric()

    for(i in 1:100) {
      vec[i] <- t_tests[[i]]$p.value
    }

    #R20
    fun20 <- function(x, y, z) {
      res <- x+y^z
      return(res)
    }

    debug(fun20)
    fun20(x, y, z)

    # debug shows that x is not found. After creating object x, and subsequently
    # also y and z, debug will not show any problems anymore. RStudio will then
    # exit from the function and cease debugging.

    #R21
    special <- function(vec) {
      if(!is.vector(vec) | length(vec) <= 1) { # checks for vector with minimum 2 values
        stop("Error, input is not a vector.")
      }
      store <<- c()
      for(i in 1:length(vec)) {
        if(length(which(store == vec[i])) == 0) {
          store <- c(store, vec[i])
        }
      }
      if(length(vec) == length(store)) {
        warning("All values are special!")
      }
      print(store)
    }

    #R22
    fun22 <- function(vec) {
      vect <- c()
      vect <- c(vec[length(vec)], vec, vec[1])
      return(vect)
      print("I tried my best to quicksort!")
    }

    #R23
    grass <- "green"
    colorit <- function(color_me, grass_me) {
      grass_me <- grass
      color_me <- "blue"
      grass <- "blue"
      colorful_items = c(color_me, grass_me)
      return(colorful_items)
    }

    # A: function colorit makes grass_me the colour of grass (which is green),
    # color_me and grass blue, creates vector colorful_items with color_me and
    # grass_me, and returns this vector, effectively always returning the vector
    # '"blue" "green"'. No input is needed, other than the grass object (which is
    # not specified in the function arguments).

    # B: grass will still be green, because the blue assignment happens in the
    # local environment of the function. Thus, the original object grass retains
    # it's value "green".

    #R24
    # A: no, because the object "grass" can't be found which is necessary for
    # colorit() to function.

    # my fixed function inside the new .R file
    colorit <- function(color_me, grass_me) {
      grass_me <- "green"
      color_me <- "blue"
      colorful_items = c(color_me, grass_me)
      return(colorful_items)
    }

    #R25
    fun25 <- function(x) {
      cal <- numeric()
      cal <- x/7
      if(cal == round(cal)) {
        print("You've found a random number divisible by 7! Number found:"); print(x)
      } else {
        return(fun25(sample.int(1e3, 1)))
      }
    }

    # this number finds any number between 1:1e3 which is divisible by 7. If the
    # original input is not divisible by 7, the function will evaluate random
    # numbers until it's found a number that does.

    #Radv1
    library(dplyr)
    dictionary <- readLines("sgb-words.txt")

    infinite_monkey <- function() {
      output <- character()
      text <- "strt"
      count <- 0
      while(!(text %in% dictionary)) {
        text <- paste(letters[sample.int(26, 5, replace = T)], collapse = "")
        output <- paste(c(output, text, " "))
        count <- count + 1
      }
      #    cat("Finally a real word! The word is:", text, "\n")
      #    cat("Amount of tries:", count)
      return(count)
    }

    counts <- c()
    for(i in 1:100) {
      counts <- c(counts, infinite_monkey())
    }
    hist(counts)
  })

  switch (week,
          week1 = print(week_1),
          week2 = print(week_2),
          week3 = print(week_3)
  )
}
