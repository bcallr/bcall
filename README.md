# bcall

B-Call analysis for legislative voting data in R.

## Installation

```r
devtools::install_github("Alcatruz/bcall")
library(bcall)
```

## Quick Start

### Principle 1: Automatic Clustering

Provide only rollcall data. The package clusters legislators automatically.

```r
# Your data: data.frame with rownames
rollcall <- data.frame(
  Vote_01 = c(1, -1, 1, 0, NA),
  Vote_02 = c(1, -1, 1, 1, 1),
  Vote_03 = c(-1, 1, 1, 0, -1),
  row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D", "Leg_E")
)

# Run analysis
results <- bcall_auto(rollcall)

# Visualize
plot_bcall_analysis_interactive(results)
```

### Principle 2: User Clustering

Provide rollcall data AND clustering assignment.

```r
# Your rollcall data
rollcall <- data.frame(
  Vote_01 = c(1, -1, 1, 0, NA),
  Vote_02 = c(1, -1, 1, 1, 1),
  row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D", "Leg_E")
)

# Your clustering (any 2 values)
clustering <- data.frame(
  cluster = c("Coalition", "Opposition", "Coalition", "Opposition", "Coalition"),
  row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D", "Leg_E")
)

# Run analysis
results <- bcall(rollcall, clustering, pivot = "Leg_B")

# Visualize
plot_bcall_analysis_interactive(results, color_by = "cluster")
```

## Data Format

**Rollcall:**
- Type: `data.frame` with rownames
- Rownames: Legislator names
- Columns: Votes (any names)
- Values: `1` (Yes), `-1` (No), `0` (Abstention), `NA` (Absent)

**Clustering (Principle 2 only):**
- Type: `data.frame` with 1 column and rownames
- Values: Any 2 unique values (e.g., "A"/"B", "Coalition"/"Opposition", 1/2)

**Pivot (Principle 2 only):**
- Type: `character` (legislator name that exists in data)

## Main Functions

- `bcall_auto(rollcall)` - Automatic clustering + analysis
- `bcall(rollcall, clustering, pivot)` - Analysis with your clustering
- `plot_bcall_analysis_interactive(results)` - Interactive plot
- `summarize_bcall_analysis(results)` - Summary statistics
- `export_bcall_analysis(results)` - Export to CSV/PNG

## Example with Synthetic Data

```r
library(bcall)

# Create synthetic data
rollcall <- create_synthetic_rollcall()
clustering <- create_synthetic_clustering()

# Try both approaches
results1 <- bcall_auto(rollcall)
results2 <- bcall(rollcall, clustering, pivot = "Silva_Jorge")

# Visualize
plot_bcall_analysis_interactive(results1)
plot_bcall_analysis_interactive(results2, color_by = "cluster")
```

## Citation

> Toro-Maureira S, Reutter J, Valenzuela L, Alcatruz D and Valenzuela M (2025) **B-Call: integrating ideological position and voting cohesion in legislative behavior**. *Frontiers in Political Science* 7:1670089. [doi:10.3389/fpos.2025.1670089](https://doi.org/10.3389/fpos.2025.1670089)

```bibtex
@article{ToroMaureira2025BCall,
  author  = {Toro-Maureira, Sergio and Reutter, Juan and Valenzuela, Lucas and Alcatruz, Daniel and Valenzuela, Macarena},
  title   = {B-Call: integrating ideological position and voting cohesion in legislative behavior},
  journal = {Frontiers in Political Science},
  year    = {2025},
  volume  = {7},
  pages   = {1670089},
  doi     = {10.3389/fpos.2025.1670089}
}
```
