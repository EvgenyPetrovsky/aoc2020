test_input <- c(
"ecl:gry pid:860033327 eyr:2020 hcl:#fffffd",
"byr:1937 iyr:2017 cid:147 hgt:183cm",
"",
"iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884",
"hcl:#cfa07d byr:1929",
"",
"hcl:#ae17e1 iyr:2013",
"eyr:2024",
"ecl:brn pid:760753108 byr:1931",
"hgt:179cm",
"",
"hcl:#cfa07d eyr:2025 pid:166559648",
"iyr:2011 ecl:brn hgt:59in",
""
)
real_input <- readLines("./inputs/day04-input.txt")

split_passports <-function(text) {
  end_of_line <- ":eol:"
  text %>%
    paste(collapse = end_of_line) %>%
    strsplit(split = paste0(end_of_line, end_of_line)) %>%
    unlist() %>%
    gsub(pattern = end_of_line, replacement = " ", x = .)

}

test_passport_tag_presence <- function(passport_data, tags) {
  tags %>%
    Map(f = function(x) grepl(x, passport_data)) %>%
    unlist() %>%
    all()
}

extract_passport_tag <- function(passport, tag) {
  text <- paste("", passport, "")
  m <- gregexpr(
    pattern = paste0(tag,":([^[:space:]])+"),
    text = text, ignore.case = T
  )
  extracted_val <- regmatches(x = text, m = m)
  substr(extracted_val, nchar(tag) + 2, 100)
}

test_passport_tag_value <- function(passport, tag)

test_byr <- function(x) {
  byr <- as.integer(x)
  byr >= 1920 & byr <= 2002
}
test_iyr <- function(x) {
  iyr <- as.integer(x)
  iyr >= 2010 & iyr <= 2020
}
test_eyr <- function(x) {
  eyr <- as.integer(x)
  eyr >= 2020 & eyr <= 2030
}
test_hgt <- function(x) {
  if (grepl(pattern = "^\\d{2,3}(cm|in)$", x) == FALSE) return(FALSE)
  hgt <- substr(x, 1, nchar(x)-2)
  mut <- substr(x, nchar(x)-1, nchar(x))
  if (mut == "cm" & hgt >= 150 & hgt <= 193) TRUE
  else if (mut == "in" & hgt >= 59 & hgt <= 76) TRUE
  else FALSE
}
test_hcl <- function(x) {
  grepl(pattern = "^#[0-9,a-f]{6}$", x)
}
test_ecl <- function(x) {
  x %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
}
test_pid <- function(x) {
  grepl(pattern = "^\\d{9}$", x)
}

test_passport_tag_values <- function(passport) {
  check_functions <- list(
    byr = test_byr,
    iyr = test_iyr,
    eyr = test_eyr,
    hgt = test_hgt,
    hcl = test_hcl,
    ecl = test_ecl,
    pid = test_pid
  )

  check_functions %>%
    names() %>%
    Map(f = function(x) {
      tag_value <- extract_passport_tag(passport, x)
      if (tag_value == "") FALSE
      else check_functions[[x]](tag_value)
    }) %>%
    unlist
}

tags <- c("byr","iyr","eyr","hgt","hcl","ecl","pid","cid")
tags_hacked <- tags[tags != "cid"]

#------------------------------------------------------------------------------#

day04_part1_solution <- function(input) {
  input %>% split_passports() %>%
    Map(f = function(x) test_passport_tag_presence(x, tags_hacked)) %>%
    Filter(f = function(x) x == TRUE) %>%
    length()
}

test_output_part1 <- 2
test_result <- day04_part1_solution(test_input)
print(paste("test result:", test_result, "valid:", test_result == test_output_part1))

real_result_part1 <- day04_part1_solution(real_input)
print(real_result_part1)

#------------------------------------------------------------------------------#

day04_part2_solution <- function(input) {
  input %>% split_passports() %>%
    Map(f = function(x) all(test_passport_tag_values(x))) %>%
    Filter(f = function(x) x == TRUE) %>%
    length()
}

test_input_part2_invalid <- c(
"eyr:1972 cid:100",
"hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926",
"",
"iyr:2019",
"hcl:#602927 eyr:1967 hgt:170cm",
"ecl:grn pid:012533040 byr:1946",
"",
"hcl:dab227 iyr:2012",
"ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277",
"",
"hgt:59cm ecl:zzz",
"eyr:2038 hcl:74454a iyr:2023",
"pid:3556412378 byr:2007"
)

test_input_part2_valid <- c(
  "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980",
  "hcl:#623a2f",
  "",
  "eyr:2029 ecl:blu cid:129 byr:1989",
  "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm",
  "",
  "hcl:#888785",
  "hgt:164cm byr:2001 iyr:2015 cid:88",
  "pid:545766238 ecl:hzl",
  "eyr:2022",
  "",
  "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
)

test_output_part2 <- 0
test_result <- day04_part2_solution(test_input_part2_invalid)
print(paste("test result:", test_result, "valid:", test_result == test_output_part2))

test_output_part2 <- 4
test_result <- day04_part2_solution(test_input_part2_valid)
print(paste("test result:", test_result, "valid:", test_result == test_output_part2))

real_result_part2 <- day04_part2_solution(real_input)
print(real_result_part2)

