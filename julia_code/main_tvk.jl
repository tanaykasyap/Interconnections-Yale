# ==============================================================================
# Interconnections project main file
#   This file contains mock code to load data on the interconnection queue and estimates the model
#
#  Author: Tanay Kasyap 
#  Date:   2025 May 27
# ==============================================================================
 

# Clear the console and variables (only clears the terminal does not clear the workspace)

function clc()
    if Sys.iswindows()
        return read(run(`powershell cls`), String)
    elseif Sys.isunix()
        return read(run(`clear`), String)
    elseif Sys.islinux()
        return read(run(`printf "\033c"`), String)
    end
end


clc()

# TVK: if you do option J + R, then it will clear the console and reload the file 

# Load file paths
include("load_file_paths.jl")


# ==============================================================================
# Read data files from text (NJR and TVK)

include("read_queue_data.jl")

# Dynamic table of queue data
file_dynamic = joinpath(data, "project_dynamic.csv");

queue = readQueueData( file_dynamic );

# Static table of project characteristics
file_static = joinpath(data, "project_static.csv");
project_static = readStaticData( file_static );

summaryStats(queue)

summaryStats(project_static)


# Merge static characteristics with dynamic queue data
"""
mergeQueueData
    Join a “queue” table with the cleaned static data to produce a combined
    project_static_dynamic table.

# Arguments
- queue::DataFrame: your queue DataFrame
- project_static::DataFrame: the output of `readStaticData(...)`

# Returns
- DataFrame: result of a left‐join on `:project_id` and `:q_id`
            with unique column names and missing‐mismatch handling
"""
function mergeQueueData(queue::DataFrame, project_static::DataFrame)::DataFrame
    return leftjoin(
        queue,
        project_static;
        on           = [:project_id, :q_id],
        makeunique   = true,
        matchmissing = :notequal
    )
end
queue = mergeQueueData( queue, project_static );


# Filter queue data to keep only periods with novel cost draws
queue_novel = keepNovelCosts( queue );

# Strip dataframes down to bare components for the model
#   x_j static characteristics
#   s_jt states 
#   a_jt actions 
#   state_grid 


# Tanay -
#Saving the cleaned dynamic queue data and loading in the static data to create the static and dynamic data
# ==============================================================================
# Tanay - Save the dynamic queue data to a file
@save joinpath(data, "project_dynamic.jld2") queue

# Tanay - To read the static project characteristics,  we can do the following:
@load joinpath(data, "project_static.jld2") project_static

vscodedisplay(project_static)
@load joinpath(data, "project_dynamic.jld2") queue
@load joinpath(data, "project_static.jld2") project_static

vscodedisplay(queue)

# ----------------------------------------
#  Merge (left-join) static characteristics onto each dynamic record
#    by the `project_id` column
project_static_dynamic = leftjoin(
    queue,
    project_static;
    on           = [:project_id, :q_id], # can do it on q_id as well.
    makeunique   = true, # ensure unique column names, we need to do this as q_id is present in both datasets
    matchmissing = :notequal  # matchmissing = :notequal 
    # (treat every missing as not equal to anything, so the join still proceeds and you end up with missing in the new columns)
)


#  Save the merged dataset as JLD2
@save joinpath(data, "project_static_dynamic.jld2") project_static_dynamic


# ==============================================================================
# Descriptive analysis of interconnection queue costs

# Plot the distribution of costs and cost transitions between phases
include("plot_cost_transitions.jl")

# Regression tables for cost transitions


# Run linear regression of cost on lagged cost
# ct_mod1 =lm(@formula(cost_poi ~ cost_poi_lag1), queue)
# r2(ct_mod1)


# ==============================================================================
# Dynamic model estimation


var_types = Dict(
    :model_year =>                    Int64,   
    :q_id =>                          String, 
    :project_id =>                    Union{Missing,Int64},
    :checklist_phase =>               String, 
    :status_date =>                   String,    
    :cost_poi =>                      Float64,
    :cost_network =>                  Float64,
    :original_status_date =>          String,    
    :action =>                        String,
    :POI_costs_carried_forward =>     Int64,   
    :network_costs_carried_forward => Int64,   
    :active_project =>                Int64);

# Define global variables for the dynamic model estimation
global_parameters = Dict(
    :N_simulations => 100,  # Simulations used in estimation
    :N_countersims => 100,
    :T_max         => 15,
    :beta          => 0.9); # Discount factor


# Estimate public state transition regressions [ALGG]
#    State transitions
#    TPD receipt probaility
#    Own continue probability

# delta_public = estimateStateTransitionsPublic( )

# Estimate cost transitions
# delta_private = estimateStateTransitionsPrivate()

    
include("estimate_dynamic_model.jl")



# ==============================================================================
# Dynamic model counterfactuals

include("run_model_counterfactuals.jl")


# ==============================================================================
#                                  END
# ==============================================================================
