# Shelling's Model of Segregation
# Alexander Sheludkov
# 9 March 2025

# Based on:
# Schelling, Thomas C. (1971). "Dynamic models of segregation". 
# The Journal of Mathematical Sociology. 1 (2), pp. 143–186. 
# https://www.suz.uzh.ch/dam/jcr:00000000-68cb-72db-ffff-ffffff8071db/04.02_schelling_71.pdf

# Easley, D., & Kleinberg, J. (2010). Networks, crowds, and markets: 
# Reasoning about a highly connected world (Vol. 1). Cambridge: Cambridge university press.
# Pp. 107-116. https://www.cs.cornell.edu/home/kleinber/networks-book/networks-book.pdf

# install.packages(c("tidyverse", "terra", "landscapemetrics", "gganimate", "ggsci", "gifski", "cowplot"))

library(tidyverse)
library(cowplot)
library(terra)
library(landscapemetrics)
library(gganimate)
library(gifski)
library(ggsci)

# ================
# 1. Preprocessing
# ================

options(scipen = 999)

# 1.1. Create starting game field
size <- 100  # Filed size (size x size)
vacant_ratio <- 0.15  # Vacant cell ratio (20%)

set.seed(121)
grid <- 
  matrix(nrow = size, ncol = size, 
         # Vacant cells and two groups of equal size
         data = sample(c(0, 1, 2), size * size, replace = TRUE, 
                       prob = c(vacant_ratio, (1 - vacant_ratio) / 2, (1 - vacant_ratio) / 2))) %>% 
  # Convert 
  rast()

# Plot
plot(grid, col = c("white", "red", "steelblue3"), main = "Initial", legend = TRUE)

# Total agent number
freq(grid) # 4164 + 4305 = 8469

# 1.2. Function to calculate the similarity rate: 
# the fraction of neighbors in the focal window of the same group as of the center cell
similarity_level <- function(grid) {
  
  # Copy the grid as shell
  similarity_level <- grid
  
  # Calculate vector of similarity values 
  similarity_vals <- 
    # Get adjacent cells vector 
    adjacent(grid, 1:length(values(grid)), directions="8", pairs=TRUE, include=FALSE) %>% 
    as_tibble() %>% 
    # Add values
    mutate(from_val = values(grid)[from],
           to_val = values(grid)[to]) %>% 
    # Calculate cimilarity for each cell
    group_by(from) %>% 
    summarise(n_neighbors = sum(to_val != 0, na.rm = T),
              similarity = if_else(
                n_neighbors == 0, 0, # If all the adjacent cells are vacant, return 0
                sum(from_val == to_val, na.rm = T) / n_neighbors
                )
              ) %>%
    pull(similarity)
  
  # Add values to raster object
  values(similarity_level) <- similarity_vals
  
  return(similarity_level)
}

# Test
# similarity_level(grid) %>% 
#   plot(main = "Similarity")

# 1.3. Function to initiate simulation
# @initial_grid
# @nsteps: maximum number of iterations
# @threshold: similarity threshold

shelling_simulation <- function(initial_grid, nsteps, threshold){
  
  # Create a shell for the results stack from the initial grid
  result_grids <- initial_grid
  # Change layer's name
  names(result_grids) <- c("Initial")
  
  # Main cycle
  for (step in 1:nsteps) {
    
    # Print step number
    print(paste0("Step: ", step))
    
    # Copy the last layer in the resulting stack as the working layer
    step_grid <- result_grids[[nlyr(result_grids)]]
    
    # Calculate similarity
    similarity <- similarity_level(step_grid)
    
    # Find unsatisfied agents (cells below similarity threshold)
    unsatisfied <- which((values(step_grid) != 0) & (values(similarity) < threshold))
    
    # Break cycle if there are no unsatisfied agents
    if(length(unsatisfied) == 0) {
      
      print("Game is over")
      break
      
    } else {

      print(paste0("Number of unsatisfied: ", length(unsatisfied)))
      
      # Get vacant cells indices
      vacant_cells <- which(values(step_grid) == 0)
      
      # Define maximum possible moves for step (min of agents to move and vacant cells)
      n_moves <- min(length(unsatisfied), length(vacant_cells))
      
      # Shuffle the order of agents and vacant cells
      unsatisfied <- sample(unsatisfied, n_moves, replace = F)
      
      vacant_cells <- sample(vacant_cells, n_moves, replace = F)
      
      # Move agents
      values(step_grid)[vacant_cells] <- values(step_grid)[unsatisfied]
      # Assigning the old cells as vacant
      values(step_grid)[unsatisfied] <- 0
      
      # Name the new grid as step number
      names(step_grid) <- paste0("Step ", step)
      
      # Add to the resulting stack
      result_grids[[paste0("Step ", step)]] <- step_grid
      
      Sys.sleep(0.1)
    }
  }
  
  return(result_grids)
}

# =============
# 2. Simulation
# =============

# threshold = 0.25
test_25 <- shelling_simulation(grid, 100, 0.25)
# threshold = 0.50
test_50 <- shelling_simulation(grid, 100, 0.5)
# threshold = 0.75
test_75 <- shelling_simulation(grid, 100, 0.75)

# Plot final stages
test_25_final_plot <-
  test_25[[nlyr(test_25)]] %>% 
  as.data.frame(xy = T) %>% 
  as_tibble() %>% 
  ggplot(aes(x = x, y = y, fill = as.factor(`Step 7`)))+
  geom_tile(show.legend = F)+
  scale_fill_manual(name = element_blank(), values = c("white", "red", "steelblue3"))+
  coord_equal(expand = F)+
  theme_void(base_size = 8)+
  theme(panel.border = element_rect(colour = "grey10", fill = NA),
        plot.margin = margin(0.02, 0.02, 0.02, 0.02, unit = "cm")
  )+
  ggtitle('Threshold = 0.25',
          subtitle = 'Final Step (7)')

test_50_final_plot <- 
  test_50[[nlyr(test_50)]] %>% 
  as.data.frame(xy = T) %>% 
  as_tibble() %>% 
  ggplot(aes(x = x, y = y, fill = as.factor(`Step 57`)))+
  geom_tile(show.legend = F)+
  scale_fill_manual(name = element_blank(), values = c("white", "red", "steelblue3"))+
  coord_equal(expand = F)+
  theme_void(base_size = 8)+
  theme(panel.border = element_rect(colour = "grey10", fill = NA),
        plot.margin = margin(0.02, 0.02, 0.02, 0.02, unit = "cm")
  )+
  ggtitle('Threshold = 0.5',
          subtitle = 'Final Step (57)')

test_75_final_plot <- 
  test_75[[nlyr(test_75)]] %>% 
  as.data.frame(xy = T) %>% 
  as_tibble() %>% 
  ggplot(aes(x = x, y = y, fill = as.factor(`Step 100`)))+
  geom_tile(show.legend = F)+
  scale_fill_manual(name = element_blank(), values = c("white", "red", "steelblue3"))+
  coord_equal(expand = F)+
  theme_void(base_size = 8)+
  theme(panel.border = element_rect(colour = "grey10", fill = NA),
        plot.margin = margin(0.02, 0.02, 0.02, 0.02, unit = "cm")
        )+
  ggtitle('Threshold = 0.75',
          subtitle = 'Step 100')

# Combine plots
final_steps_plot <- 
  plot_grid(test_25_final_plot, test_50_final_plot, test_75_final_plot, nrow = 1)+
  theme(plot.margin = margin(0, 0.02, 0.07, 0.02, unit = "cm"))

ggsave(final_steps_plot, file = "final_steps_plot.jpeg", device = "jpeg", 
       width = 24, height = 8.75, units = "cm", dpi = 300)

# Animate simulation results at 0.5 threshold
test_50_df <- 
  test_50 %>% 
  as.data.frame(xy = T) %>% 
  as_tibble()
final_step <- nlyr(test_50) - 1
colnames(test_50_df) <- c("x", "y", 0:final_step)
  
test_50_df_anim <- 
  test_50_df %>% 
  pivot_longer(3:ncol(.), names_to = "Step", values_to = "Group") %>% 
  mutate(Step = as.integer(Step),
         Group = as.factor(Group)) %>% 
  filter(Step %in% c(0:10, seq(15, 50, 5), final_step)) %>%
  ggplot(aes(x = x, y = y, fill = Group))+
  geom_tile(show.legend = F)+
  scale_fill_manual(name = element_blank(), values = c("white", "red", "steelblue3"))+
  coord_equal(expand = F)+
  theme_void()+
  theme(panel.border = element_rect(colour = "grey10", fill = NA),
        plot.margin = margin(0.02, 0.02, 0.02, 0.02, unit = "cm"))+
  ggtitle('Threshold = 0.5',
          subtitle = 'Step: {closest_state}')+
  transition_states(Step, transition_length = 1, state_length = 1)+
  ease_aes('cubic-in-out')

# render
gganimate::animate(test_50_df_anim, 
                   fps = 2,      # число кадров в секунду
                   nframes = 20,    # общее число кадров
                   width = 400, 
                   height = 420,
                   detail = 2,
                   end_pause = 1,
                   renderer = gifski_renderer())

# Save
anim_save(animation = last_animation(), filename = "shellings_model.gif")

# ==========
# 3. Metrics
# ==========

# Mean patch size
mean_patch_size <- rbind(
  lsm_l_area_mn(test_25, directions = 8) %>% 
    mutate(threshold = "0.25"),
  lsm_l_area_mn(test_50, directions = 8) %>% 
    mutate(threshold = "0.5"),
  lsm_l_area_mn(test_75, directions = 8) %>% 
    mutate(threshold = "0.75")
) %>% 
  mutate(threshold = factor(threshold, levels = c("0.25", "0.5", "0.75")),
         value = value * 10000) %>% # convert to number of cells
  rename(mean_patch_size = value) %>% 
  select(layer, mean_patch_size, threshold)

# Entropy
entropy_level <- rbind(
  lsm_l_condent(test_25) %>% 
    mutate(threshold = "0.25"),
  lsm_l_condent(test_50) %>% 
    mutate(threshold = "0.5"),
  lsm_l_condent(test_75) %>% 
    mutate(threshold = "0.75")
) %>% 
  mutate(threshold = factor(threshold, levels = c("0.25", "0.5", "0.75"))) %>% 
  rename(conditional_entropy = value) %>% 
  select(layer, conditional_entropy, threshold)

# Combine
metrics <- mean_patch_size %>% 
  full_join(entropy_level, by = c("layer", "threshold"))

# Plot mean patch size
mean_patch_size_plot <- 
  metrics %>% 
  ggplot(aes(x = layer-1, y = mean_patch_size, color = threshold))+
  geom_point(size = 1)+
  geom_smooth(se = F, linewidth = 0.5)+
  scale_color_futurama()+
  scale_x_continuous(name = "Step")+
  scale_y_continuous(name = "Mean patch size, cells")+
  theme(legend.position = c(0.14, 0.85))
ggsave(mean_patch_size_plot, file = "mean_patch_size_plot.jpeg", device = "jpeg", 
       width = 12, height = 12, units = "cm", dpi = 300)
