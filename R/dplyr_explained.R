dplyr_explained <- function() {
  code <- '
    # Overview of dplyr functions with examples

    # 1. select() - Select specific columns from a dataset
    # 2. filter() - Filter rows based on conditions
    # 3. mutate() - Add or modify columns in a dataset
    # 4. arrange() - Sort rows by one or more variables
    # 5. summarise() - Aggregate data by calculating summary statistics
    # 6. group_by() - Group data by one or more variables for aggregation
    # 7. rename() - Rename columns in a dataset
    # 8. left_join(), right_join(), inner_join(), full_join() - Combine datasets by matching rows based on common columns
    # 9. bind_rows() - Stack datasets vertically (combine rows)
    # 10. bind_cols() - Combine datasets horizontally (combine columns)
    # 11. distinct() - Remove duplicate rows from a dataset
    # 12. count() - Count occurrences of unique combinations of values
    # 13. slice() - Select rows by position
    # 14. top_n() - Select the top n rows based on a variable
    # 15. pull() - Extract a single column as a vector
    # 16. unite() - Combine multiple columns into a single column
    # 17. separate() - Split a column into multiple columns
    # 18. pivot_longer() - Convert wide format to long format
    # 19. pivot_wider() - Convert long format to wide format

    # Example of using each dplyr function
    library(dplyr)

    data <- tibble(a = 1:5, b = 6:10)

    # 1. select() - Select specific columns from a dataset
    selected_data <- data %>% select(a)
    print(selected_data)

    # 2. filter() - Filter rows based on conditions
    filtered_data <- data %>% filter(a > 2)
    print(filtered_data)

    # 3. mutate() - Add or modify columns in a dataset
    mutated_data <- data %>% mutate(c = a + b)
    print(mutated_data)

    # 4. arrange() - Sort rows by one or more variables
    arranged_data <- data %>% arrange(a)
    print(arranged_data)

    # 5. summarise() - Aggregate data by calculating summary statistics
    summary_data <- data %>% summarise(mean_a = mean(a))
    print(summary_data)

    # 6. group_by() - Group data by one or more variables for aggregation
    grouped_data <- data %>% group_by(a) %>% summarise(mean_b = mean(b))
    print(grouped_data)

    # 7. rename() - Rename columns in a dataset
    renamed_data <- data %>% rename(new_a = a)
    print(renamed_data)

    # 8. left_join(), right_join(), inner_join(), full_join() - Combine datasets by matching rows based on common columns
    data2 <- tibble(a = 3:7, c = 11:15)
    left_joined_data <- data %>% left_join(data2, by = "a")
    print(left_joined_data)
    right_joined_data <- data %>% right_join(data2, by = "a")
    print(right_joined_data)
    inner_joined_data <- data %>% inner_join(data2, by = "a")
    print(inner_joined_data)
    full_joined_data <- data %>% full_join(data2, by = "a")
    print(full_joined_data)

    # 9. bind_rows() - Stack datasets vertically (combine rows)
    data3 <- tibble(a = 6:10, b = 11:15)
    bound_rows_data <- bind_rows(data, data3)
    print(bound_rows_data)

    # 10. bind_cols() - Combine datasets horizontally (combine columns)
    bound_cols_data <- bind_cols(data, data3)
    print(bound_cols_data)

    # 11. distinct() - Remove duplicate rows from a dataset
    distinct_data <- data %>% distinct(a)
    print(distinct_data)

    # 12. count() - Count occurrences of unique combinations of values
    counted_data <- data %>% count(a)
    print(counted_data)

    # 13. slice() - Select rows by position
    sliced_data <- data %>% slice(1:3)
    print(sliced_data)

    # 14. top_n() - Select the top n rows based on a variable
    top_n_data <- data %>% top_n(3, a)
    print(top_n_data)

    # 15. pull() - Extract a single column as a vector
    pulled_data <- data %>% pull(a)
    print(pulled_data)

    # 16. unite() - Combine multiple columns into a single column
    united_data <- data %>% unite("a_b", a, b, sep = "-")
    print(united_data)

    # 17. separate() - Split a column into multiple columns
    separated_data <- united_data %>% separate(a_b, into = c("a", "b"), sep = "-")
    print(separated_data)

    # 18. pivot_longer() - Convert wide format to long format
    wide_data <- tibble(id = 1:3, var1 = c(10, 20, 30), var2 = c(40, 50, 60))
    long_data <- wide_data %>% pivot_longer(cols = starts_with("var"), names_to = "variable", values_to = "value")
    print(long_data)

    # 19. pivot_wider() - Convert long format to wide format
    wide_again <- long_data %>% pivot_wider(names_from = "variable", values_from = "value")
    print(wide_again)
  '
  cat(code)
}
