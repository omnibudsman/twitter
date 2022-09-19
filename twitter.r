library(tidyverse)
library(ggpubr)

setwd(Sys.getenv(TWITTER_DIR))

# download from https://electionstudies.org/data-center/2020-time-series-study/
data.raw <- read.csv("anes_timeseries_2020_csv_20220210/anes_timeseries_2020_csv_20220210.csv")
allvars <- names(data.raw)[names(data.raw) != "V201200"]

########## NOTE TO READERS:
# I don't use the ANES sample weights in this analysis. This is technically a no-no,
# but my experience has been that these don't tend to matter too much for analyses
# like these.

#####  political opinion vars
# V202342 = assault weapons ban
# V202256 = regulation
# V201409 = transgender.bathrooms
# V201336 = opinion on abortion
# V201343 = favor the death penalty?
# V202377 = raise or lower minimum wage?

##### political knowledge
# V201644 = how many years in US senator term?
# V202548 = current unemployment rate


##### non-opinion variables
# V201602 = violence justified?
# V201122 = how irritated are you?
# V201115 = how hopeful are you?
# V201116 = how afraid?
# V201237 = are people trustworthy?
# V201651 = how satisfied with life?

##### political id
# break it down by self-identified liberal or conservative
# V201200 (1-7 liberal-conservative)
# V201018 party (1 democrat, 2 republican, 4 independent, 5 other)

##### twitter use
# V202541b used twitter in last year
# V202544 how often do you use twitter (for twitter users only)
# V202545 how often post political content on twitter (for twitter users only)

#### other
# V201337 = importance of abortion


############################
# Answer notes
############################

# -9 refused to answer
# -8 don't know
# 99 haven't thought much about this

answers.to.drop <- c(-9, -8, -7, -6, -5, 99)

libcon.key <- c("Extremely liberal","Liberal","Slightly liberal","Moderate","Slightly conservative","Conservative","Extremely conservative")

############################
# Rename and keep variables
############################

data.clean <-
  data.raw %>%
  rename(
    abortion.opinion = V201336,
    abortion.importance = V201337,
    political.id = V201200,
    twitter.frequency = V202544,
    twitter.user = V202541b,
    violence.justified = V201602,
    irritated = V201122,
    political.posting = V202545,
    party.id = V201018,
    assault.weapons.ban = V202342,
    regulation = V202256,
    transgender.bathrooms = V201409,
    trustworthy.people = V201237,
    death.penalty = V201343,
    us.senator.term = V201644,
    life.satisfaction = V201651,
    minimum.wage = V202377,
    current.unemployment.rate = V202548,
    hopefulness = V201115,
    fear = V201116,
    outrage = V201117,
    pre.election.weight = V200010a,
    post.election.weight = V200010b
  ) %>% # eliminate?
  subset(
    twitter.user %in% c(0,1)
  ) %>%
  mutate(
    pro.abortion = as.numeric(abortion.opinion == 4),
    pro.violence = as.numeric(violence.justified >= 2), # at least a little justified
    pro.assault.ban = as.numeric(assault.weapons.ban == 1),
    pro.regulation = as.numeric(regulation <= 3),
    pro.transgender.bathrooms = as.numeric(transgender.bathrooms == 2),
    college.grad = as.numeric(V201510 >= 6 & V201510 < 95),
    people.are.trustworthy = as.numeric(trustworthy.people == 2 | trustworthy.people == 1),
    pro.death.penalty = as.numeric(death.penalty == 1),
    us.senator.term.correct = as.numeric(us.senator.term == 6),
    very.unsatisfied.with.life = as.numeric(life.satisfaction == 5),
    raise.minimum.wage = as.numeric(minimum.wage == 1),
    current.unemployment.rate.correct = as.numeric(current.unemployment.rate == 2),
    libcon.label = plyr::mapvalues(political.id, c(1,2,3,4,5,6,7),c(libcon.key)),
    very.irritated = as.numeric(irritated >= 4),
    very.hopeful = as.numeric(hopefulness >= 4),
    hopeless = as.numeric(hopefulness == 1),
    very.afraid = as.numeric(fear >= 4),
    very.outraged = as.numeric(outrage >= 4)
  ) %>%
  mutate(
    twitter.user.label = plyr::mapvalues(twitter.user, c(0,1),c("Not on Twitter","Twitter users")),
    alternate.twitter.user.label = plyr::mapvalues(twitter.user, c(0,1),c("Portion of Twitter nonusers","Estimated portion of Twitter posts")),
    party.label = plyr::mapvalues(party.id, c(-9,-8,-1,1,2,4,5),c("Other","Other","Other","Democrat","Republican","Other","Other"))
  ) %>%
  mutate(
    num.frequency = plyr::mapvalues(twitter.frequency,c(1,2,3,4,5,6,7),c(150,90,30,12,4,2,0.5)), # NOTE : These are estimations for monthly usage based on the question responses
    freq.posting = plyr::mapvalues(political.posting,c(1,2,3,4,5),c(1,0.75,0.5,0.25,0)) # NOTE : These are estimations for posting frequnecy based on the question responses
  ) %>%
  mutate(
    num.posting = num.frequency * freq.posting
  )

clean.data <- 
  data.clean %>%
  subset(political.id >= 1 & political.id < 99)

twitter.users <- clean.data %>% subset(twitter.user == 1)
non.twitter.users <- clean.data %>% subset(twitter.user == 0)


############################## Section 1: Posting frequency by political group
# Note - this section is EXACTLY THE SAME as the last one but the variables are aggregates differently

# just show the distributions side by side, weighted by posts
posting.by.political.affiliation <- twitter.users %>%
  mutate(n = n(), n.postings = sum(num.posting)) %>%
  group_by(libcon.label, political.id) %>%
  summarise(
    portion.of.population = round(100*n()/max(n),2),
    portion.of.posts = round(100*sum(num.posting)/max(n.postings),2)
      )

# individual level
individual.level <- clean.data %>%
  group_by(twitter.user.label) %>%
  mutate(n = n()) %>%
  group_by(twitter.user.label, libcon.label, political.id) %>%
  summarise(
    portion.of.population = round(100*n()/max(n),2)
  )

g10 <- ggplot(individual.level) +
  geom_bar(aes(x = reorder(libcon.label, political.id), y = portion.of.population, fill=twitter.user.label), stat="identity", position="dodge") +
  ylim(0,50) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values = c("Not on Twitter" = "gray", "Twitter users"="#00acee")) +
  xlab("") +
  ylab("Percent of population\n") +
  ggtitle("Political identity by individual") +
  guides(fill=guide_legend(title="Group")) +
  theme(text=element_text(size=12, family="Raleway"), plot.margin = margin(20,20,20,20, unit="pt"))
  
g20 <- ggplot(posting.by.political.affiliation) +
  geom_bar(aes(x = reorder(libcon.label, political.id), y = portion.of.posts), stat="identity", position="dodge", fill="blue") +
  ylim(0,50) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab("") +
  ylab("Estimated percent of Tweets\n") +
  ggtitle("Political identity by tweet") +
  guides(color=guide_legend(title="")) +
  theme(text=element_text(size=12, family="Raleway"), plot.margin = margin(20,20,20,20, unit="pt"))

g30 <- ggarrange(g10, g20)
g30
ggsave("tweet_politics.png", width=12, height=6)


############################## Section 2: For several variables: X% of users, but Y% of tweets

# got a bit hacky/lazy in this section

# people who endorse political violence
violence.breakdown <- twitter.users %>%
  summarise(
    percent = round(100*mean(pro.violence)),
    percent.tweets = round(100*weighted.mean(pro.violence, num.posting))
  ) %>%
  mutate(label="Political violence is sometimes justified")

# people who are very irritated
irritation.breakdown <- twitter.users %>%
  summarise(
    percent = round(100*mean(very.irritated)),
    percent.tweets = round(100*weighted.mean(very.irritated, num.posting))
  ) %>%
  mutate(label="'Very' or 'Extremely' irritated with direction of country")

# people who are not hopeful
hope.breakdown <- twitter.users %>%
  summarise(
    percent = round(100*mean(hopeless)),
    percent.tweets = round(100*weighted.mean(hopeless, num.posting))
  ) %>%
  mutate(label="'Not at all' hopeful about direction of country")

# people who are very hopeful
outrage.breakdown <- twitter.users %>%
  summarise(
    percent = round(100*mean(very.outraged)),
    percent.tweets = round(100*weighted.mean(very.outraged, num.posting))
  ) %>%
  mutate(label="'Very' or 'Extremely' outraged with direction of country")

# calculate for general population
violence.breakdown.genpop <- non.twitter.users %>% summarise(value = round(100*mean(pro.violence))) %>% mutate(label="Political violence is sometimes justified")
irritation.breakdown.genpop <- non.twitter.users %>% summarise(value = round(100*mean(very.irritated))) %>% mutate(label="'Very' or 'Extremely' irritated with direction of country")
hope.breakdown.genpop <- non.twitter.users %>% summarise(value = round(100*mean(hopeless))) %>% mutate(label="'Not at all' hopeful about direction of country")
outrage.breakdown.genpop <- non.twitter.users %>% summarise(value = round(100*mean(very.outraged))) %>% mutate(label="'Very' or 'Extremely' outraged with direction of country")
genpop <- rbind(violence.breakdown.genpop,irritation.breakdown.genpop,hope.breakdown.genpop,outrage.breakdown.genpop) %>% mutate(name="Percentage of non-Twitter users")

combined.posting.feelings <- rbind(violence.breakdown, irritation.breakdown, hope.breakdown, outrage.breakdown) %>%
  pivot_longer(1:2) %>%
  mutate(name = plyr::mapvalues(name,c("percent","percent.tweets"),c("Percentage of Twitter users", "Percentage of tweets")))

everything.combined <- rbind(genpop,combined.posting.feelings)

ggplot(everything.combined, aes(x = reorder(name,c(value)), y = value, fill=name)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  scale_fill_manual(values = c("Percentage of Twitter users" = "#00acee", "Percentage of tweets"="blue", "Percentage of non-Twitter users"="gray")) +
  ylim(0,100) +
  theme(axis.text.x = element_blank())+#element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab("") +
  ylab("Percent\n") +
  geom_text(aes(label=value), vjust=-0.5) +
  ggtitle("Endorsement of views among Twitter users and nonusers\nand weighted by tweet frequency\n") +
  guides(fill=guide_legend(title="")) +
  theme(text=element_text(size=12, family="Raleway"), plot.margin = margin(20,20,20,20, unit="pt")) +
  facet_wrap(~label, nrow=2)
ggsave("genpop_vs_tweets.png", width=12, height=7)


##############
# random stuff

# twitter usership among college grads
mean(clean.data[(clean.data$V201510 >= 6 & clean.data$V201510 <= 8),]$twitter.user)

# twitter usership among non grads
mean(clean.data[(clean.data$V201510 < 6 & clean.data$V201510 >= 1),]$twitter.user)


