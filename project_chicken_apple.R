# Here we calculate costs and benefits of the inclusion of chicken into 
# conventional apple orchards####

library(decisionSupport)

# define path of input table and others
input_table <- "data_chicken_apple.csv"
legend_file <- "legend_chicken_apple.csv"
mc_results_folder <- "mc_Results_chicken_apple"
evpi_results_folder <- "evpi_Results_chicken_apple"

# function to create global variables from input table
make_variables <- function(est,n=1)
{ x <- random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir = .GlobalEnv)}

#create variables to set up function
make_variables(estimate_read_csv(input_table))

#set the path for model outputs
dir.create(evpi_results_folder)

#calculate the total apple yield (shouldn't this be inside the function?)
harvest_total<-orchard_area*trees_per_ha*harvest_tree

# We assume: An orchard with 10ha apples, 3000-4000 trees per ha, 
# and one chicken coop with 1200 laying hens, conventional conditions

#define the function
Apple_Simulation<- function(){
  
  
  #Costs apple orchard----
  
  #set the cost to run machine for one hour
  cost_diesel<-consumption_diesel_hour*diesel_price
  
  #cost to harvest the apples = time it takes to harvest * (wage + diesel_costs)
  cost_harvesting<-(vv(yearly_harvesting_hours, var_CV, n=n_years)*
    hourly_wage)+
    (vv(yearly_harvesting_hours, var_CV, n=n_years)*
    cost_diesel)
    
  #cost to mow the grass
  cost_mowing<-(vv(yearly_mowing_hours, var_CV, n=n_years)*
    hourly_wage)+
    (vv(yearly_mowing_hours, var_CV, n=n_years)*
    cost_diesel)
  
  #cost to remove the weed from the strip
  cost_weeding<-(vv(yearly_weeding_hours, var_CV, n=n_years)*
    hourly_wage)+(vv(yearly_weeding_hours, var_CV, n=n_years)*
    cost_diesel)+ 
    cost_herbicide*
    orchard_area*2/3
 
  #cost to control for apple scab
  cost_scabcontrol<-(vv(yearly_scabcontrol_hours, var_CV, n=n_years)*
    hourly_wage)+
    cost_fungicide*
    orchard_area*2/3+
    (vv(yearly_scabcontrol_hours, var_CV, n=n_years)*
    cost_diesel)
  
  #event of extraordinary apple scab infestation (chance of 25%, should be a variable)
  #also, why is there the 0.3? The extraordinary apple scab event takes and additional 30% of time?
  scab_year_treatment<-(vv(yearly_scabcontrol_hours, var_CV, n=1)*0.3*
    hourly_wage)+
    cost_fungicide*0.3*
    orchard_area*2/3+
    yearly_scabcontrol_hours*0.3*
    cost_diesel  
  cost_scab_year<-chance_event(0.25,scab_year_treatment, 0, n=n_years, 0.25)
  
  #cost of insect control
  #insects are confused with pheromones, so its a one time application
  cost_insectcontrol<-vv(yearly_insectcontrol_hours, var_CV, n=n_years)*
    hourly_wage+
    cost_insecticide*
    orchard_area+
    vv(yearly_insectcontrol_hours, var_CV, n=n_years)*
    cost_diesel
  
  #event of extraordinary insect infestation
  insect_year_treatment<-yearly_insectcontrol_hours*0.3*
    hourly_wage+
    cost_insecticide*0.3*
    orchard_area*2/3+
    yearly_insectcontrol_hours*0.3*
    cost_diesel  
  cost_insect_year<-chance_event(0.25,insect_year_treatment, 0, n=n_years, 0.25)
  
#the chances of both events (intensive apple scab and intensive insect infestation)
#what are they based on? Also events like these every four years seem a lot to me
#maybe every 10 years? This should definately be a variable in the model
  
  #cost of nutrient application
  cost_nutrients<-(vv(yearly_fertilization_hours, var_CV, n=n_years)*
    hourly_wage)+
    cost_fertilizer*
    orchard_area*2/3+ 
    (vv(yearly_fertilization_hours, var_CV, n=n_years)*
    cost_diesel)
  
  #vole control costs
  cost_vole_control<-vv(yearly_vole_control_hours, var_CV, n=n_years)*
    hourly_wage  
  
  #trees dying because of the voles?
  dead_tree_occurrence<-vv(dead_trees, var_CV, n=1)
  
  #cost to replace dead trees
  cost_replacement_trees<-cost_tree*dead_trees+planting_hours_tree*dead_trees*hourly_wage
  
  #chance of vole damage, chance should be definately be a variable
  occurence_vole_damage<-chance_event(0.3,1,0, n=n_years)
  #--> should the cost of replacing trees be even higher at vole events? 
  #do we know how many trees voles damage at this event?
  
  #right know this for loop doesnt make a difference because you assign the exact values again to cost-replacement_tree
  # for(i in 1:n_years){
  #   
  #   if(occurence_vole_damage[i] == 1){
  #     #if vole damage happens, some trees need to be replaced and affected trees 
  #     #will die and have no fruits to be harvested. Thus, harvest needs to be adjusted. 
  #     
  #     #harvest_total[i]<-harvest_total-(dead_tree_occurrence*harvest_tree)
  #     
  #     cost_replacement_trees[i]<-cost_tree*dead_trees+planting_hours_tree*dead_trees*hourly_wage
  #   }
  #   
  # }
  
#Sum up all costs----
  Costs_apple_production <- cost_harvesting + cost_mowing + cost_weeding +
                              cost_insectcontrol + cost_insect_year +
                              cost_scabcontrol + cost_scab_year +
                              cost_vole_control + cost_nutrients +
                              occurence_vole_damage
  
    #--> isnt this the exact same loop as above?
    # for(i in 1:n_years){
    #   
    #   if(occurence_vole_damage[i] == 1){
    #     #if vole damage happens, some trees need to be replaced and affected trees 
    #     #will die and have no fruits to be harvested. Thus, harvest needs to be adjusted. 
    #     
    #     cost_replacement_trees[i]<-
    #       cost_tree*dead_trees+
    #       planting_hours_tree*dead_trees*hourly_wage
    #   }
    #   
    # }
  
  
  #Calculate harvest loss> needs to be influenced by random events (scab_year, 
  #insect_year, vole_damage)
  #harvest_loss<-random_loss+scab_loss+insect_loss+occurence_vole_damage
  #DUMMY HARVEST LOSS####
  dummy_harvest_loss<-harvest_total*0.2
  marketable_harvest<-vv(harvest_total, var_CV, n=n_years)-dummy_harvest_loss
  
  #Revenue apple production----
  
  Revenue_apple_production<-marketable_harvest*vv(apple_price, var_CV,n=n_years)
  
  #Result_apple_production----
  
  Result_apple<-Revenue_apple_production-Costs_apple_production
  
  return(list(NPV = discount(Result_apple, discount_rate, calculate_NPV = TRUE),
              Cashflow = Result))
}

n_sim <- 10000

# To get a probabilistic overview we run a Monte Carlo Simulation ####

Apple_Simulation <- mcSimulation(estimate_read_csv("data_chicken_apple.csv"),
                                         model_function = Apple_Simulation,
                                         numberOfModelRuns = n_sim,
                                         functionSyntax = "plainNames")
# This fuction is to plot the distribution of values ####
decisionSupport::plot_distributions(mcSimulation_object = Apple_Simulation, 
                                    vars = c("NPV"),
                                    # You can even add more results here
                                    method = "smooth_simple_overlay", 
                                    base_size = 11)
# this one plots the cashflow
plot_cashflow(mcSimulation_object = Apple_Simulation, cashflow_var_name = "Cashflow")

# Here we can plot each or many EVPI results
mcSimulation_table <- data.frame(Apple_Simulation$x, Apple_Simulation$y[(1)])

#share of cases with positive NPV
sum(mcSimulation_table$NPV >= 0) / n_sim

pls_result <- plsr.mcSimulation(object = Apple_Simulation,
                                resultName = names(Apple_Simulation$y)[1], ncomp = 1)
input_table <- read.csv("data_chicken_apple.csv")

plot_pls(pls_result, input_table = input_table, threshold = 0.5)

evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV")
plot_evpi(evpi, decision_vars = ("NPV"))

compound_figure(mcSimulation_object = Apple_Simulation, input_table = NULL,
                plsrResults = pls_result, EVPIresults = evpi,
                decision_var_name = "NPV",
                cashflow_var_name = "Cashflow", base_size = 11)

#Here we are writing the chicken in apple function----
  
Chicken_Apple_Simulation<- function(){
    
  #Some costs of the apple plantation are reduced due to benefits of including 
  #chicken into the plantation----
    
    cost_diesel<-consumption_diesel_hour*diesel_price
    cost_harvesting<-(vv(yearly_harvesting_hours, var_CV, n=n_years)*
                        hourly_wage)+
      (vv(yearly_harvesting_hours, var_CV, n=n_years)*
         cost_diesel)
    
    reduced_weeding<-
      cost_weeding-vv((cost_weeding*0.15), var_CV, n=n_years)
    
    reduced_mowing<-
      cost_mowing-vv((cost_mowing*0.15), var_CV, n=n_years)
    
    reduced_scabcontrol<-
      cost_scabcontrol-vv((cost_scabcontrol*0.18), var_CV, n=n_years)
    
    reduced_cost_scab_year<-
      chance_event(0.125,scab_year_treatment, 0, n=n_years, 0.25)
    
    reduced_insectcontrol<-
      cost_insectcontrol-vv((cost_insectcontrol*0.2), var_CV, n=n_years)
    
    #funfact: regular mineral fertiliser contains 70% chicken manure!
    reduced_nutrients<-
      cost_nutrients-vv((cost_nutrients*0.7), var_CV, n=n_years)
    
    reduced_vole_control<-
      cost_vole_control-vv((cost_vole_control*0.4), var_CV, n=n_years)
    
    reduced_occurence_vole_damage<-
      chance_event(0.01, dead_tree_occurrence, 0, n=n_years)
    
    for(i in 1:n_years){
      
      if(reduced_occurence_vole_damage[i] == 1){
        #if vole damage happens, some trees need to be replaced and affected trees 
        #will die and have no fruits to be harvested. Thus, harvest needs to be adjusted. 
        
        #harvest_total[i]<-harvest_total-vv(dead_trees, var_CV, n=1)*vv(harvest_tree, var_CV, n=1)
        
        cost_replacement_trees[i]<-cost_tree*dead_trees+planting_hours_tree*dead_tree*hourly_wage
      }
      
    }  
    
    #here we need to think again... less damage less loss.
    #scab_year_treatment<-(vv(yearly_scabcontrol_hours, var_CV, n=1)*0.3*
                            #hourly_wage)+
      #cost_fungicide*0.3*
      #orchard_area*2/3+
      #yearly_scabcontrol_hours*0.3*
      #cost_diesel  
    #reduced_cost_scab_year<-chance_event(0.25,scab_year_treatment, 0, n=n_years, 0.25)
    

    #same here like scab
    #insect_year_treatment<-yearly_insectcontrol_hours*0.3*
     # hourly_wage+
      #cost_insecticide*0.3*
      #orchard_area*2/3+
      #yearly_insectcontrol_hours*0.3*
      #cost_diesel  
    #cost_insect_year<-chance_event(0.25,insect_year_treatment, 0, n=n_years, 0.25)
    
    #Sum up all costs----
    Costs_apple_production<-
      cost_harvesting+
      reduced_mowing+
      reduced_weeding+
      reduced_insectcontrol+
      #cost_insect_year+
      reduced_scabcontrol+
      #cost_scab_year+
      reduced_vole_control+
      reduced_nutrients+
      reduced_occurence_vole_damage
    for(i in 1:n_years){
      
      if(reduced_occurence_vole_damage[i] == 1){
        #if vole damage happens, some trees need to be replaced and affected trees 
        #will die and have no fruits to be harvested. Thus, harvest needs to be adjusted. 
        
        cost_replacement_trees[i]<-
          cost_tree*dead_trees+
          planting_hours_tree*dead_trees*hourly_wage
      }
      
    }
    #Calculate harvest loss
    #harvest_loss<-random_loss+scab_loss+insect_loss+occurence_vole_damage
    #DUMMY HARVEST LOSS####
    dummy_harvest_loss<-harvest_total*0.2
    marketable_harvest<-harvest_total-dummy_harvest_loss
    
    #Revenue apple production----
    
    Revenue_apple_producion<-marketable_harvest*apple_price  
 
 #Costs chicken----
  
  cost_coop <- c(coop_invest * number_hens, rep(0, n_years -1))
  
  cost_maintenance_coop<-c(0,vv(maintenance_coop, var_CV, n=n_years-1)*
                             number_hens)
  
  cost_coop_nyears <- cost_coop + cost_maintenance_coop
  
  cost_fence<-c(((fence_price * 2 * 100 + 2*(number_hens*req_area/100)+
                    battery_price)), 
                rep(0,n_years-1))
  
  cost_flock<-vv(cost_hen*number_hens, n = n_years, var_CV =  var_CV)
  
  cost_insurance <- vv(chicken_insurance, var_CV = var_CV, n = n_years)
  

  #also right now we assume that we buy x amount of chicken and that they 
  #instantly die, dont eat and lay no eggs
  #is there a way to let them die halfway through the year
  #another point: shouldnt the chicken not be fed only for the time they lay 
  #eggs and get sold?
  cost_feed<-vv(feed_need, var_CV = var_CV, n = n_years)*
    feed_price*365*
    (number_hens*survival_rate)
  
  #so some chickens die, they dont eat but still get bedding?
  cost_bedding<-vv(bedding_price, var_CV = var_CV, n = n_years) *
    (number_hens*survival_rate)
  
  #calculation of daily, weekly and irregular tasks
  daily_cost<-vv(cost_daily, var_CV = var_CV, n = n_years) * 
    number_hens * 
    hourly_wage
  
  #>for the weekly routines
  weekly_cost<-vv(cost_weekly, var_CV = var_CV, n = n_years) * 
    number_hens * 
    hourly_wage
  
  costs_irregular_events<-vv(irregular_cost, var_CV = var_CV, n = n_years) * 
    number_hens * 
    hourly_wage
  
  #>for the veterinary
  costs_vet<-cost_vet_visit+ costs_vaccin*number_hens+
              chance_event(0.25, cost_vet_visit, 0, n=n_years)  
  
  
  #revenues----
  
  #same issue as above, the dead chicken die at the first day, that is a bit 
  #unrealistic
  #added vv for price, because that is in my eyes the most volatile variable
  revenue_eggs<-vv(egg_price, var_CV, n=n_years)*
    vv(eggs_per_hen, var_CV, n=n_years) * number_hens *
    vv(survival_rate, var_CV, n=n_years)*
    vv(marketable_share, var_CV, n=n_years)
  
  revenue_meat<-vv(revenue_hen, var_CV, n=n_years)*
    number_hens*
    vv(survival_rate, var_CV, n=n_years)
  
  insurance_coverage <- rep(0, n_years)
  
  
  #modify the time series for avian influenca incidences
  
  occurence_influence_flock <- chance_event(risk_influenca_flock,n = n_years)
  
  #modify the costs and benefits in case of of avian influenca
  for(i in 1:n_years){
    
    if(occurence_influence_flock[i] == 1){
      #if avian influenca happens in the flock all the chicken will be killed and 
      #thus costs and beenfits are zero for this year, maybe costs for vet?
      
      #cost_maintenance_coop[i] <- 0
      
      #cost_feed[i] <- 0
      
      cost_bedding[i] <- 0
      
      daily_cost[i] <- 0
      
      weekly_cost[i] <- 0
      
      #here is the cost to organize vet etc included; so I guess this goes up
      #maybe multiply with factor 1.5?
      costs_irregular_events[i] <- costs_irregular_events[i] * 1.5
      
      revenue_eggs[i] <- 0
      
      revenue_meat[i] <- 0
      
      insurance_coverage[i] <- cost_flock[i] + revenue_eggs[i] + revenue_meat[i]
    }

  }
  
  #add the different cost and benefit variables up
  
  cost_invest_chicken <- cost_coop_nyears + cost_fence + cost_flock
  
  cost_care_chicken <- cost_feed + cost_bedding + daily_cost +
    weekly_cost + costs_irregular_events + costs_vet + cost_insurance
  
  benefit_direct <- revenue_eggs + revenue_meat
  
  Benefits <- Revenue_apple_producion+ benefit_direct + insurance_coverage 
  
  Costs <- Costs_apple_production+ cost_invest_chicken + cost_care_chicken
  
  
  
  Result_apple_chicken<-Benefits-Costs
  
  
  return(list(NPV = discount(Result_apple_chicken, discount_rate, calculate_NPV = TRUE),
              Cashflow = Result))
}




n_sim <- 10000

# To get a probabilistic overview we run a Monte Carlo Simulation ####

Chicken_Apple_Simulation <- mcSimulation(estimate_read_csv("data_chicken_apple.csv"),
                                         model_function = Chicken_Apple_Simulation,
                                         numberOfModelRuns = n_sim,
                                         functionSyntax = "plainNames")
# This fuction is to plot the distribution of values ####
decisionSupport::plot_distributions(mcSimulation_object = Chicken_Apple_Simulation, 
                                    vars = c("NPV"),
                              
                                    # You can even add more results here
                                    method = "smooth_simple_overlay", 
                                    base_size = 11)
# this one plots the cashflow
plot_cashflow(mcSimulation_object = Chicken_Apple_Simulation, cashflow_var_name = "Cashflow")

# Here we can plot each or many EVPI results
mcSimulation_table <- data.frame(Chicken_Apple_Simulation$x, Chicken_Apple_Simulation$y[(1)])

#share of cases with positive NPV
sum(mcSimulation_table$NPV >= 0) / n_sim

pls_result <- plsr.mcSimulation(object = Chicken_Apple_Simulation,
                                resultName = names(Chicken_Apple_Simulation$y)[1], ncomp = 1)
input_table <- read.csv("data_chicken_apple.csv")

plot_pls(pls_result, input_table = input_table, threshold = 0.5)

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

