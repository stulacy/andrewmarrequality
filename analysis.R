## Script to run analysis

library(tidyverse)
library(lubridate)
library(gridExtra)

######################### Load raw data and clean up
# Clean date up, some dates have ordinals (st, th, rd), and the string 'Sunday' isn't useful
guests <- read.csv("guests.csv", stringsAsFactors = FALSE)
papers <- read.csv("papers.csv", stringsAsFactors = FALSE)

papers <- papers %>%
    mutate(date = gsub("Sunday\\s?", "", date),
           dom = str_extract(date, "^[0-9]+"),
           mon = str_extract(date, " [[:alpha:]]+ "),
           yr = str_extract(date, " [0-9]+$"),
           date2 = dmy(sprintf("%s %s %s", dom, mon, yr))) %>%
    select(date=date2, guest, female) %>%
    mutate(female = factor(female, levels=c(1, 0), labels=c('Female', 'Male')))

guests <- guests %>%
    mutate(date = gsub("Sunday\\s?", "", date),
           dom = str_extract(date, "^[0-9]+"),
           mon = str_extract(date, " [[:alpha:]]+ "),
           yr = str_extract(date, " [0-9]+$"),
           date2 = dmy(sprintf("%s %s %s", dom, mon, yr))) %>%
    select(date=date2, guest, female) %>%
    mutate(female = factor(female, levels=c(1, 0), labels=c('Female', 'Male')))

############## Is Piers Morgan's claim of 43/58 paper reviewers in 2019 accurate?
# I get 48/63 so let's assume so, but importantly, what are these figures in context?
papers %>%
    filter(date >= "2019-01-01") %>%
    count(female)

####### Plot overall
papers %>%
    rbind(guests) %>%
    group_by(date) %>%
    summarise(prop = mean(female == 'Female') * 100) %>%
    mutate(mav = zoo::rollmean(prop, 4, fill=NA)) %>%
    filter(!is.na(mav)) %>%
    ggplot(aes(x=date, y=mav)) +
        geom_line() +
        geom_smooth() +
        geom_hline(yintercept=51, colour="orange", size=1) +
        annotate("text", x=as.Date("2016-01-01"), y=55, label="51% UK ratio",
                 colour="orange") +
        scale_x_date(date_breaks="1 year", date_labels = "%Y") +
        labs(x="", y="Female (%)", title="Proportion of female guests on Andrew Marr: 2012-2019") +
        ylim(0, 100) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle=45),
              panel.grid.minor = element_blank())
ggsave("overall_proportion.png")

####### Break down by main guest and newspaper
plt_guests <- guests %>%
    group_by(date) %>%
    summarise(prop = mean(female == 'Female')*100) %>%
    mutate(mav = zoo::rollmean(prop, 4, fill=NA)) %>%
    filter(!is.na(mav)) %>%
    ggplot(aes(x=date, y=mav)) +
        geom_line() +
        geom_smooth() +
        geom_hline(yintercept=51, colour="orange") +
        annotate("text", x=as.Date("2015-06-01"), y=60, label="UK ratio (51%)",
                 colour="orange") +
        scale_x_date(date_breaks="1 year", date_labels = "%Y") +
        labs(x="", y="Female (%)") +
        ylim(0, 100) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle=45),
              panel.grid.minor = element_blank())

# Actually yes, there is something to be said that it the Paper guests actually started off fairly 
# even and now are noticeably skewed towards women
plt_papers <- papers %>%
    group_by(date) %>%
    summarise(prop = mean(female == 'Female')*100) %>%
    mutate(mav = zoo::rollmean(prop, 4, fill=NA)) %>%
    filter(!is.na(mav)) %>%
    ggplot(aes(x=date, y=mav)) +
        geom_line() +
        geom_smooth() +
        geom_hline(yintercept=51, colour="orange", size=1) +
        annotate("text", x=as.Date("2016-01-01"), y=28, label="UK ratio (51%)",
                 colour="orange") +
        scale_x_date(date_breaks="1 year", date_labels = "%Y") +
        labs(x="", y="") +
        ylim(0, 100) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle=45),
              panel.grid.minor = element_blank())
png("stratified_proportion.png")
grid.arrange(plt_guests + ggtitle("Non-newspaper guests"), 
             plt_papers + ggtitle("Newspaper reviewers"), 
             nrow=1)
dev.off()

