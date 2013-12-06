Good style is important because while your code only has one author, it will usually have multiple readers, and when you know you will be working with multiple people on the same code, it’s a good idea to agree on a common style up-front.

#### Notation and naming

##### File names

File names should end in .r and be meaningful.

    # Good
    explore-diamonds.r
    hadley-wickham-hw-1.r
    # Bad
    foo.r
    my-homework.R

##### Identifiers

Variable and function names should be lowercase. Use _ to separate words within a name. Generally, variable names should be nouns and function names should be verbs. Strive for concise but meaningful names (this is not easy!)

    # Good
    day_one
    day_1
    # Bad
    first_day_of_the_month
    DayOne
    dayone
    djm1

#### Syntax

##### Spacing

Place spaces around all binary operators (=, +, -, <-, etc.). Do not place a space before a comma, but always place one after a comma.

    # Good
    average <- mean(feet / 12 + inches, na.rm = T)
    # Bad
    average<-mean(feet/12+inches,na.rm=T)

Place a space before left parentheses, except in a function call.

    # Good
    `if (debug)`
    `plot(x, y)`
    
    # Bad
    `if(debug)`
    `plot (x, y)`

Extra spacing (i.e., more than one space in a row) is okay if it improves alignment of equals signs or arrows (<-).

    list(
      x = call_this_long_function(a, b), 
      y = a * e / d ^ f)
    
    list(
      total = a + b + c, 
      mean  = (a + b + c) / n)

Do not place spaces around code in parentheses or square brackets. (Except if there’s a trailing comma: always place a space after a comma, just like in ordinary English.)

    # Good
    if (debug)
    diamonds[5, ]
    
    # Bad
    if ( debug )  # No spaces around debug
    x[1,]  # Needs a space after the comma
    x[1 ,]  # Space goes after, not before

