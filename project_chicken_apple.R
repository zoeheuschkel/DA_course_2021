# Here we calculate costs and benefits of the inclusion of chicken into apple####
# plantations for the first year. ####

library(decisionSupport)
# Here we define input table, the legend file and create folders to store our results
input_table <- "data_chicken_apple.csv"
legend_file <- "legend_chicken_apple.csv"
mc_results_folder <- "mc_Results_chicken_apple"
evpi_results_folder <- "evpi_Results_chicken_apple"

# Here we make variables ####
make_variables <- function(est,n=1)
{ x <- random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir = .GlobalEnv)}
make_variables(estimate_read_csv(input_table))
dir.create(evpi_results_folder)

#Here we start writing the function ####
Chicken_Apple_Simulation<- function(){
  
  # Here we calculate all costs####
  # We assume: One chicken coop with 1200 laying hens, conventional conditions
  
  # First we calculate all necessary single investments####  
  # >for the coop
  cost_coop<-(coop_invest*number_hens)+
              vv(maintenance_costs_coop, var_CV, n=1)*
              number_hens
  
  # >for the fence (we need 4 sqm per hen), we know that chicken only move away 
  # from the coop about 100m, so for the fence it is: 
  cost_fence<-fence_price*2*100+2*(number_hens*req_area/100)
  
  cost_invest_chicken<-cost_coop+cost_fence+cost_hen*number_hens
  
  # Here we calculate the costs of taking care of the chicken####
  #>for the feed
  cost_feed<-feed_price*feed_need*number_hens
  #>for the bedding
  cost_bedding<-bedding_price*number_hens
  #>for the daily routines
  daily_cost<-cost_daily*number_hens*hourly_wage
  #>for the weekly routines
  weekly_cost<-cost_weekly*number_hens*hourly_wage
  #>for the irregular events
  costs_irregular_events<-irregular_cost*number_hens*hourly_wage
  
  #Here we sum up the costs for chicken care####
  cost_care_chicken<-cost_feed+
                    cost_bedding+
                    daily_cost+
                    weekly_cost+ 
                    costs_irregular_events
  
  #Here we calculate all risks of including chicken into the plantation####
  #The cost of each risk can be calculated by cost of the event *probability of the event
  # cost_risks<-cost_risk_damage+cost_risk_pollution+cost_risk_diseasetransmission
  # +cost_risk_beneficialreduction+cost_risk_totalloss
  
  # Here we add all costs ####
  Costs<-cost_invest_chicken+
        cost_care_chicken 
        #+cost_risks
 
  # Here we calculate all benefits of including chicken into the plantation ####
  # >all direct benefits of selling eggs and meat
  revenue_eggs<-egg_price*
                vv(eggs_per_hen, var_CV, n=1)*
                number_hens*
                vv(marketable_share, var_CV, n=1)
  revenue_meat<-revenue_hen*
                vv(number_hens, var_CV, n=1)
  
  benefit_direct<-revenue_eggs+revenue_meat
  
  # Here we calculate all further benefits of including chicken into the plantation
  # benefit_indirect<-benefit_weedmanagement+
                      #benefit_reducedmowing+
                      #benefit_volereduction+
                      #benefit_scabreduction+
                      #benefit_organicfertilizer
  
  #Here we add all benefits ####
  Benefits<-benefit_direct #+benefit_indirect
  
  # Here we calculate the result ####
  Result<-Benefits-Costs
  
  
  return(list(NPV = discount(Result, discount_rate, calculate_NPV = TRUE),
              Cashflow = Result))
}

# To get a probabilistic overview we run a Monte Carlo Simulation ####

Chicken_Apple_Simulation <- mcSimulation(estimate_read_csv("data_chicken_apple.csv"),
                                         model_function = Chicken_Apple_Simulation,
                                         numberOfModelRuns = 100,
                                         functionSyntax = "plainNames")
# This fuction is to plot the distribution of values ####
decisionSupport::plot_distributions(mcSimulation_object = Chicken_Apple_Simulation, 
                                    vars = c("NPV"),
                                    # You can even add more results here
                                    method = "smooth_simple_overlay", 
                                    base_size = 11)
# this one plots the cashflow
plot_cashflow(mcSimulation_object = Chicken_Apple_Simulation, cashflow_var_name = "Cashflow")

pls_result <- plsr.mcSimulation(object = Chicken_Apple_Simulation,
                                resultName = names(Chicken_Apple_Simulation$y)[1], ncomp = 1)
input_table <- read.csv("data_chicken_apple.csv")

plot_pls(pls_result, input_table = input_table, threshold = 0.5)

# Here we can plot each or many EVPI results
mcSimulation_table <- data.frame(Chicken_Apple_Simulation$x, Chicken_Apple_Simulation$y[(1)])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV")
plot_evpi(evpi, decision_vars = ("NPV"))

compound_figure(mcSimulation_object = Chicken_Apple_Simulation, input_table = NULL,
                plsrResults = pls_result, EVPIresults = evpi,
                decision_var_name = "NPV",
                cashflow_var_name = "Cashflow", base_size = 11)
# Click on zoom to see the big picture

# if we need the data for nicer graphs, we can export it with the "write.csv" command
write.csv(Chicken_Apple_Simulation$y, "Chicken_Apple_SimulationY.csv", row.names = FALSE)
write.csv(Chicken_Apple_Simulation$x, "Chicken_Apple_SimulationX.csv", row.names = FALSE)

Chicken_Apple_Simulation <- list(x = read.csv("Chicken_Apple_SimulationX.csv"),
                                 y = read.csv("Chicken_Apple_SimulationY.csv"))
class(Chicken_Apple_Simulation) <- cbind("Chicken_Apple_Simulation", class(Chicken_Apple_Simulation))
write.csv(Chicken_Apple_Simulation, "Chicken_Apple_Simulation.csv", row.names = FALSE)

# Write VIP results
library(chillR)
VIP_scores <- VIP(pls_result)
VIP_coefficient <- pls_result$coefficients 

VIP_NPV <- cbind(VIP_scores, VIP_coefficient)
write.csv(VIP_NPV, "VIP_NPV", row.names = FALSE)

# Write EVPI results
write.csv(evpi$NPV, "EVPI_NPV.csv", row.names = FALSE)
