# static.jl
#
# Generate and clean time-invariant project characteristics (x_j) for the interconnections model.
# This script:
 
#    Loads project_static.csv (raw String for IDs and coords)
#    Recodes clusters (handles Missing, "TC", "SGIP-TC", numeric strings)
#    Drops rows with non-numeric lat/lon and converts to Float64

# Author: Tanay Kasyap
# Date:   2025 June 8

# ----------------------------------------
print("\e[H\e[2J")


# ----------------------------------------
# 2. Load file paths and packages
include("load_file_paths.jl")
using CSV        # efficient CSV reading/writing
using DataFrames # tabular data handling
using PrettyTables # formatted table printing

# ----------------------------------------
# 3. Read raw CSV with latitude/longitude and project_id as String
types = Dict(
    :q_id                     => String,
    :project_id               => Union{Missing,Int},
    :latitude                 => String,
    :longitude                => String,
    :project_name             => String,
    :cluster                  => String,
    :county                   => String,
    :MW                       => Union{Missing,Float64},
    :voltage_level_kV         => Union{Missing,Float64},
    :co_located               => Union{Missing,Int},
    :firm                     => String,
    :PTO                      => String,
    :zone_id                  => String,
    :point_of_interconnection => String,
    :area_name                => String,
    :area_code                => Union{Missing,Int},
    :FCDS                     => Union{Missing,Int},
    :Type                     => String
)
project_static_raw = DataFrame(CSV.File(joinpath(data, "project_static.csv");
    types=types, missingstring= ["", "NA"],))


#To display the dataset in VSCode, you can use the following command:
vscodedisplay(project_static_raw)

project_static_raw.co_located = categorical(project_static_raw.co_located)
project_static_raw.FCDS       = categorical(project_static_raw.FCDS)
project_static_raw.Type       = categorical(project_static_raw.Type) 

 
 
# ----------------------------------------
# Inspecting latitude and longitude

 

# a) test for parseable numeric
isnum(x) = !ismissing(x) && tryparse(Float64, x) !== nothing

# b) build a little helper that for a column returns a NamedTuple of counts
function col_counts(col::AbstractVector)
    total = length(col)
    n_num     = count(isnum,         col)
    n_miss    = count(ismissing,     col)
    n_string  = total - n_num - n_miss
    return (numeric = n_num,
            missing = n_miss,
            string  = n_string)
end

# c) get counts for each coordinate
lat = col_counts(project_static_raw.latitude)
lon = col_counts(project_static_raw.longitude)

# d) combine into a DataFrame
counts = DataFrame(
    variable = ["latitude", "longitude"],
    numeric  = [lat.numeric,  lon.numeric],
    missing  = [lat.missing,  lon.missing],
    string   = [lat.string,   lon.string],
)

pretty_table(counts;
    header = ["Variable", "# Numeric", "# Missing", "# Other String"]
)

isnum_or_missing(x) = ismissing(x) || (tryparse(Float64, x) !== nothing) 

# 2) filter, letting through rows where each coord is numeric or missing
project_static_raw = filter(r ->
     isnum_or_missing(r.latitude) &&
     isnum_or_missing(r.longitude),
   raw)

# 3) convert strings → Float64, pass missing through
function safeparse(x)
  ismissing(x) ? missing : parse(Float64, x)
end

project_static_raw.latitude  = safeparse.(project_static_raw.latitude)
project_static_raw.longitude = safeparse.(project_static_raw.longitude)
 







##- ----------------------------------------
# Value Coding Cluster
#--------------------------------------------------

# ------------------------------------------------------------------------------
# 1) Create a temporary `code` column
# ------------------------------------------------------------------------------
 

project_static_raw.code = map(x -> begin
    if x === missing
        missing
    elseif occursin(r"^[0-9]+$", x)        # pure digits?
        parse(Int, x)
    elseif x == "TC"                       # special case
        32
    elseif x == "SGIP-TC"                  # special case
        33
    else
        missing                             # any other string → missing
    end
end, project_static_raw.cluster)

# ------------------------------------------------------------------------------
# 2) Extract the unique clusters in code‐order
# ------------------------------------------------------------------------------
# Filter out rows where code is missing, then take unique pairs and sort by code
level_df = unique(project_static_raw[.!ismissing.(project_static_raw.code), 
                                  [:cluster, :code]])
sort!(level_df, :code)

# Pull out the cluster names in the new order
level_order = level_df.cluster

@show level_order
# e.g. ["1","2",…,"14","TC","SGIP-TC"]

# ------------------------------------------------------------------------------
# 3) Convert `cluster` into an ordered categorical
# ------------------------------------------------------------------------------
project_static_raw.cluster = CategoricalArray(
    project_static_raw.cluster;
    levels = level_order,
    ordered = true
)

# (Optional) drop the helper `code` column now that you’re done:
select!(project_static_raw, Not(:code))

# ------------------------------------------------------------------------------
# 4) Verify
# ------------------------------------------------------------------------------
@show eltype(project_static_raw.cluster)        # CategoricalValue{String,UInt32}
@show levels(project_static_raw.cluster)        # should match level_order

# And sorting by cluster now uses the numeric+special order:
sorted = sort(project_static_raw, :cluster)
pretty_table(first(sorted, 10))               # show first 10 rows
# ----------------------------------------
vscodedisplay(project_static_raw)


# ----------------------------------------

 
# 1) pick out the numeric columns
# grab every numeric(ish) column -- ints & floats, possibly with missings
num_cols = [
  col for col in names(project_static_raw)
    if eltype(project_static_raw[!, col]) <: Union{Missing, Number}
]


 
 

# 2) build the summary DataFrame
summary = DataFrame(
    variable = String[],
    n        = Int[],
    mean     = Float64[],
    sd       = Float64[],
    min      = Float64[],
    median   = Float64[],
    max      = Float64[],
)

for col in num_cols
    vals = collect(skipmissing(project_static_raw[!, col]))
    push!(summary, (
        variable = col,
        n        = length(vals),
        mean     = mean(vals),
        sd       = std(vals),
        min      = minimum(vals),
        median   = median(vals),
        max      = maximum(vals),
    ))
end

# 3) print it
pretty_table(
  summary;
  header = ["Variable", "Count", "Mean", "StdDev", "Min", "Median", "Max"]
)
 

summary = describe(project_static_raw)

pretty_table(
    summary
)

project_static = select(project_static_raw,
    :q_id, :project_id, :latitude, :longitude, :project_name, :cluster,
    :county, :MW, :voltage_level_kV, :co_located, :firm, :PTO,
    :zone_id, :point_of_interconnection, :area_name, :area_code,
    :FCDS, :Type)

@save joinpath(data, "project_static.jld2") project_static

 