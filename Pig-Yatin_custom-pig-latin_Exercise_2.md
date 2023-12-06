# Strings and functional programming in R

#### **Exercise 2 (37.5 points)**

Make a function that converts words to your own version of Pig Latin.

The specific input and output that you decide upon is up to you. Don’t
forget to implement good function-making hygiene: we’ll be looking for
(unrendered) roxygen2-style documentation (being sure to describe your
Pig Latin conversion), examples of applying the function, 3
non-redundant tests, appropriate use of arguments, and appropriate
amount of checking for proper input.

**Your Pig Latin should incorporate two components:**

**Rearrangement component**

The default Pig Latin rearrangement rule, as per Wikipedia, moves
beginning letters to the end:

> 1.  For words that begin with consonant sounds, all letters before the
>     initial vowel are placed at the end of the word sequence.
>
> 2.  When words begin with consonant clusters (multiple consonants that
>     form one sound), the whole sound is added to the end
>
> 3.  For words beginning with vowel sounds, one removes the initial
>     vowel(s) along with the first consonant or consonant cluster.
>     Modify this somehow. Maybe you move letters from the end to the
>     beginning, or you change the rules altogether, keeping a similar
>     level of complexity.

**Addition component**

The default Pig Latin addition rule is to add “ay” to the end of the
word, after rearranging the letters of the word. You should choose some
other addition rule.

-   Define my own version of Pig Latin

> A version of Pig Latin = Pig Yatin
>
> ***Addition Component*:** Adds “y” after each vowel in the word.
>
> ***Rearrangement Component:*** Moves the first letter to the end of
> the word. If the first letter is a vowel, it is moved as is; if it is
> a consonant, it is duplicated at the end.

-   Create the function

<!-- -->

    #' Convert words to Pig Yatin (A custom version of Pig Latin)
    #'
    #' This function takes a word or a character vector of words as input, and converts it to Pig Yatin by rearranging
    #' the letters based on specific rules and adding a unique addition component.
    #'
    #' @param words A character string or vector representing the word to be converted. 
    #' **If you give a list with characters and integers it will treat all as characters
    #'
    #' @return A character string representing the Pig Yatin version of the input word.
    #'
    #' @examples
    #' Pig_Yatin("hello") # "eylloyhh"
    #' Pig_Yatin("address") # "ddreyssa"
    #' Pig_Yatin("gps")   # "psgg"
    #' Pig_Yatin(c("banana", "apple")) # "aynaynaybb" "ppleya"
    #' 
    #' @import stringr
    #' @export
    #' 
    #'

    # load required package
    library(stringr)

    # create the function
    Pig_Yatin <- function(words) {
      
      # check if the input is missing or empty
      if (missing(words) || length(words) == 0) {
        stop("Input is missing or empty. Please provide a character string or a character vector.")
      }
      
      # Check if the input is an empty string
      if (length(words) == 1 && nchar(words) == 0) {
        stop("Input cannot be an empty string.")
      }
       # Check if the input is a character or a list of characters
      if (!is.character(words) && !all(sapply(words, is.character))) {
        stop("Input must be a string or a list of strings.")
      }
      
      # If the input is a single string, convert it to a list
      if (is.character(words)) {
        words <- list(words)
      }
      
       # Initialize new_word with the original word
      for (i in seq_along(words)) {
        new_word <- tolower(words[[i]])
      }
      
      # Define vowels list 
      vowels <- c("a", "e", "i", "o", "u")

      # **Addition component** -> add "y" after vowels
      for (vowel in vowels) {
        new_word <- stringr::str_replace_all(new_word, paste0("(?i)", vowel), paste0(vowel, "y")) #`(?i)` makes the pattern case-insensitive
      }
      
      # **Rearrangement component** -> move first letter to the end of the word
      yatin_word <- character(length(new_word))  # Initialize a character vector to store the results
      
      for (i in seq_along(new_word)) {
        if (substr(new_word[i], 1, 1) %in% vowels) {
          moving_letter <- stringr::str_sub(new_word[i], 1, 1) # Extract the first letter
          rest_word <- stringr::str_sub(new_word[i], 3) # string starting from the second character
          yatin_word[i] <- paste0(rest_word, moving_letter)
        } else {
          moving_letter <- stringr::str_sub(new_word[i], 1, 1) # Extract the first letter
          rest_word <- stringr::str_sub(new_word[i], 2)  # string starting from the second character
          yatin_word[i] <- paste0(rest_word, moving_letter, moving_letter)
        }
      }

    return(yatin_word)
    }

-   Examples of applying the function

Example using a word starting with a vowel

    word1 <- "instance"
    Pig_Yatin(word1)

    ## [1] "nstaynceyi"

Example using a word starting with a consonant

    word2 <- "regard"
    Pig_Yatin("hello")

    ## [1] "eylloyhh"

Example using a character vector of words

    word_list <- c("apple", "banana", "orange", "grape", "kiwi", "melon")
    Pig_Yatin(word_list)

    ## [1] "ppleya"     "aynaynaybb" "rayngeyo"   "raypeygg"   "iywiykk"   
    ## [6] "eyloynmm"

-   Testing the `Pig_Yatin()` function

<!-- -->

    # load testthat package
    library(testthat)

    test_that("Pig_Yatin converts words to Pig Yatin correctly", {
      # Test single word, starting with a consonant
      expect_equal(Pig_Yatin("Morning"), "oyrniyngmm")
      # Test single word, starting with a Vowel
      expect_equal(Pig_Yatin("address"), "ddreyssa")
      # Test with a Character Vector
      word_list <- c("car", "airplane", "train", "bus")
      expect_equal(Pig_Yatin(word_list), c("ayrcc","iyrplayneya", "rayiyntt","uysbb"))
      })

    ## Test passed 🌈

    test_that("Error handling for missing or empty input", {
      expect_error(Pig_Yatin(), "Input is missing or empty.")
    })

    ## Test passed 🎉

    test_that("Error handling for non-character input", {
      expect_error(Pig_Yatin(981), "Input must be a string or a list of strings.")
      expect_error(Pig_Yatin(c(1,2,3)), "Input must be a string or a list of strings.")
    })

    ## Test passed 🎊

    test_that("Error handling for empty string input", {
      expect_error(Pig_Yatin(""), "Input cannot be an empty string.")
    })

    ## Test passed 🌈

    test_that("mixed list input", {
      expect_equal(Pig_Yatin(c("car", 123, "apple")), c("ayrcc","2311", "ppleya"))
      expect_equal(Pig_Yatin(c(25, 123, "apple")), c("522","2311", "ppleya"))
      })

    ## Test passed 🎊
