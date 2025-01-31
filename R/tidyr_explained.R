tidyr_explained <- function() {
  code <- '
    # Overview of tidyr functions

    # 1. pivot_longer() - Convert wide format to long format
    # 2. pivot_wider() - Convert long format to wide format
    # 3. separate() - Split a column into multiple columns
    # 4. unite() - Combine multiple columns into one
    # 5. drop_na() - Remove rows with missing values
    # 6. replace_na() - Replace missing values
    # 7. fill() - Fill missing values with the last observed non-NA value
    # 8. complete() - Ensure all combinations of a set of variables are present

    library(tidyr)
    library(dplyr)

    # Sample data
    df <- data.frame(
      id = c(1, 2),
      name = c("Alice", "Bob"),
      math = c(90, 85),
      science = c(80, 88)
    )

    # Examples

    # pivot_longer() example
    df_long <- df %>% pivot_longer(cols = c(math, science), names_to = "subject", values_to = "score")

    # pivot_wider() example
    df_wide <- df_long %>% pivot_wider(names_from = "subject", values_from = "score")

    # separate() example
    df_sep <- data.frame(name = c("John_Doe", "Jane_Smith")) %>% separate(name, into = c("first", "last"), sep = "_")

    # unite() example
    df_unite <- df_sep %>% unite("full_name", first, last, sep = " ")

    # drop_na() example
    df_dropna <- data.frame(a = c(1, NA, 3), b = c(4, 5, NA)) %>% drop_na()

    # replace_na() example
    df_replace_na <- data.frame(a = c(1, NA, 3), b = c(4, 5, NA)) %>% replace_na(list(a = 0, b = 0))

    # fill() example
    df_fill <- data.frame(id = c(1, 1, 2, 2), value = c(10, NA, 20, NA)) %>% fill(value, .direction = "down")

    # complete() example
    df_complete <- data.frame(id = c(1, 2), subject = c("math", "science"), score = c(90, 88)) %>% complete(id, subject)
  '
  cat(code)
}
