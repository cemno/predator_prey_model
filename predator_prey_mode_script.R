library(deSolve)
library(readODS)
library(tidyverse)

LotVmod <- function (Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dx = x*(alpha - beta*y)
    dy = -y*(gamma - delta*x)
    return(list(c(dx, dy)))
  })
}

Pars <- c(alpha = 2, beta = .5, gamma = .2, delta = .6)
State <- c(x = 10, y = 10)
Time <- seq(0, 100, by = .1)

out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time, method="rk4"))

matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Cute bunnies", "Rabid foxes"), lty = c(1,2), col = c(1,2), box.lwd = 0)


conditional_hunting_parameters <- read_ods("data/Simulation protocol for a conditional hunt of foxes in the predator prey model.ods")
buying_rabbits_parameters <- read_ods("data/Simulation protocol for buying rabbits to predator prey model.ods")
conditional_hunting_specific_days_parameters <- read_ods("data/Simulation protocol for conditional hunt foxes on specific days in the predator prey model.ods")
fencing_parameters <- read_ods("data/Simulation protocol for fencing rabbits in the predator prey model.ods")

conditional_hunting_data <- read_csv("data/table_conditional_hunting.csv")
conditional_hunting_data <- rename(conditional_hunting_data, days = "Time \\ Name", foxes = "A", rabbits = "B")
conditional_hunting_data <- pivot_longer(data = conditional_hunting_data, cols = c("foxes", "rabbits"), names_to = "Population")
buying_rabbits_data <- read_csv("data/table_buy_rabbits.csv")
buying_rabbits_data <- rename(buying_rabbits_data, days = "Time \\ Name", foxes = "A", rabbits = "B")
buying_rabbits_data <- pivot_longer(data = buying_rabbits_data, cols = c("foxes", "rabbits"), names_to = "Population")
conditional_hunting_specific_days_data <- read_csv("data/table_conditional_hunting_specific_days.csv")
conditional_hunting_specific_days_data <- rename(conditional_hunting_specific_days_data, days = "Time \\ Name", foxes = "A", rabbits = "B")
conditional_hunting_specific_days_data <- pivot_longer(data = conditional_hunting_specific_days_data, cols = c("foxes", "rabbits"), names_to = "Population")
fencing_data <- read_csv("data/table_fencing.csv")
fencing_data <- rename(fencing_data, days = "Time \\ Name", foxes = "A", rabbits = "B")
fencing_data <- pivot_longer(data = fencing_data, cols = c("foxes", "rabbits"), names_to = "Population")



conditional_hunting <- ggplot(conditional_hunting_data, aes(x = days, y = value, group = Population, color = Population))+
  geom_line()+
  scale_color_manual(values = c("#b84646", "#197ab6"))+
  ylab("population size")+
  ggtitle("Conditional hunting of foxes")
conditional_hunting
ggsave("graphs/conditional_hunting.png", height = 5, width = 7)

buying_rabbits <- ggplot(buying_rabbits_data, aes(x = days, y = value, group = Population, color = Population))+
  geom_line()+
  scale_color_manual(values = c("#b84646", "#197ab6"))+
  ylab("population size")+
  ggtitle("Adding additional rabbits at certain threshold")
buying_rabbits
ggsave("graphs/buying_rabbits_graph.png", height = 5, width = 7)

conditional_hunting_specific_days <- ggplot(conditional_hunting_specific_days_data, aes(x = days, y = value, group = Population, color = Population))+
  geom_line()+
  scale_color_manual(values = c("#b84646", "#197ab6"))+
  ylab("population size")+
  ggtitle("Conditional hunting on weekends of foxes")
conditional_hunting_specific_days
ggsave("graphs/conditional_hunting_specific_days.png", height = 5, width = 7)

fencing <- ggplot(fencing_data, aes(x = days, y = value, group = Population, color = Population))+
  geom_line()+
  scale_color_manual(values = c("#b84646", "#197ab6"))+
  ylab("population size")+
  ggtitle("Fencing of rabbits")
fencing
ggsave("graphs/fencing_data.png", height = 5, width = 7)
