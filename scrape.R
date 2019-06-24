library(rvest)
library(tidyverse)

#################### Scrape raw data
page <- xml2::read_html("https://www.bbc.co.uk/programmes/articles/4NvYDF1DWSQyKVQJWB8bypQ/previous-guests")
# Extract main content div
content <- page %>%
    html_nodes("div .text--prose")

# Extract dates of items
raw <- content %>%
    html_text() %>%
    strsplit("\n")
raw <- trimws(raw[[1]])  # Remove whitespace for consistency

# Manually add missing date before the episode with Ruth Davidson first
raw <- append(raw, "Sunday 26 November", which(grepl("^Ruth Davidson MSP, Leader of Scottish Conservatives", raw))-1)

################ Separate by weekly episode
# Find indices of dates and form these into sublists
sundays <- which(grepl("^SUNDAY\\s?[0-9]", toupper(raw)))
date_lists <- lapply(seq_along(sundays), function(i) {
    this_index <- sundays[i]

    if (i == length(sundays)) {
        next_index <- length(raw)
    } else {
        next_index <- sundays[i + 1] - 1
    }

    raw[this_index : next_index]
})

# Set the names of these to be dates and then can remove these from list along with empty strings
names <- sapply(date_lists, "[[", 1)
# Append on years as they weren't automatically added for a period of time
names[69: 112] <- paste(names[69: 112], "2017")
names[113: 156] <- paste(names[113: 156], "2016")
names[157: 183] <- paste(names[157: 183], "2015")

# Remove the first item from each list which is the date, and any empty lines
date_lists <- setNames(lapply(date_lists, function(x) x[x != ''][-1]), names)


################### Turn into editable dataframes for guests and paper review
# Now create 2 separate data frames, one for guests and other for newspaper review
df_guests <- lapply(date_lists, function(x) {
    x[!grepl("([Pp]aper|News)", x)]
})

df_papers <- lapply(date_lists, function(x) {
    x[grepl("([Pp]aper|News)", x)]
})

# Have found Evgeny Lebedev mistakenly listed as reviewing the newspapers, so he will be moved to the guests dataframe for this date
num_papers <- sapply(df_papers, length)
df_papers[num_papers > 1]

df_guests$`Sunday 25th November 2012` <- c(df_guests$`Sunday 25th November 2012`, "Evgeny Lebedev, newspaper owner")
df_papers$`Sunday 25th November 2012` <- df_papers$`Sunday 25th November 2012`[-1]

# Form tidy table for guests
df_guests <- bind_rows(lapply(df_guests, function(x) data.frame(guest=as.character(x))), .id="date") %>%
    mutate(female=0)

# Edit this in Excel or text editor of your choice
write.csv(df_guests, "guests_raw.csv", row.names=F)

# Separate paper data frame by comma then save to disk to likewise be manually cleaned
df_papers <- bind_rows(lapply(df_papers, function(x) {
    splt <- strsplit(x, ",")
    if (length(splt) == 1) {
       splt <- splt[[1]]
    }
    data.frame(guest=splt)
}),
    .id="date") %>%
    mutate(female=0)
write.csv(df_papers, "papers_raw.csv", row.names=F)

