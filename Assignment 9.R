library(stringr)

#a
str_subset(words, "^y")

#b
str_subset(words, "^y", negate = TRUE)

#c
str_subset(words, "x$")

#d
str_subset(words, "^x|x$")

#e
str_subset(words, "^[aeiou].*[^aeiou]$")

#f
str_subset(words, "^[a-z][a-z][a-z]$")

#g
str_subset(words, "^[a-z]{7,}$")

#h
str_subset(words, "[aeiou][^aeiou]")

#i
str_subset(words, "([aeiou][^aeiou]){2}")

#j
str_subset(words, "^([aeiou][^aeiou])+$")

#2
str_subset(words, "^[a-z][a-z][a-z]$")

swapped_words <- str_replace(words, "^([a-z])(.*)([a-z])$", "\\3\\2\\1")
words[swapped_words %in% words]

#3
words |> 
  str_subset("a") |> 
  str_subset("e") |> 
  str_subset("i") |> 
  str_subset("o") |> 
  str_subset("u")

# No, there are no words that contain all vowels

#4
paste0("\"", str_replace_all("a/b/c/d/e", "/", "\\\\"), "\"") |> writeLines()

#5
install.packages("babynames")
library(babynames)
babynames

babynames |>
  distinct(name) |>
  mutate(vowel_count = str_count(name, "[aeiouAEIOU]")) |>
  slice_max(vowel_count, n = 10)

# The names Mariaguadalupe and Mariadelrosario have the most vowels.

babynames |>
  distinct(name) |>
  mutate(vowel_ratio = str_count(name, "[aeiouAEIOU]") / nchar(name)) |>
  slice_max(vowel_ratio, n = 10)

# The names Eua, Ea, Ai, Ia, Ii, Aoi, Io, Aia have the highest proportion of vowels.