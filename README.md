<div align="center">
  <img src="bcallplot2.png" alt="B-Call Logo" width="400"/>
</div>

# bcall

B-Call analysis for legislative voting data in R.

## What is B-Call?

**B-Call (Bidimensional Analysis of Roll Call)** is a methodology for analyzing legislative behavior that integrates two dimensions:

- **d1 (Ideological position)**: Places each legislator on the left-right spectrum based on their average voting pattern.
- **d2 (Cohesion)**: Measures how consistent the voting behavior is. Low d2 indicates predictable voting; high d2 indicates volatility.

**Why B-Call?** Traditional models like W-NOMINATE work well in stable systems with disciplined parties. But in fragmented contexts (like Chile or Brazil), legislators with low cohesion appear artificially as "moderates" when they are actually volatile. B-Call addresses this by explicitly measuring that volatility as a second dimension.

## 📚 Examples & Tutorials

<div align="center">
  <h3>
    <a href="https://alcatruz.github.io/bcall-example">
      🔗 See real-world examples with Chilean legislative data
    </a>
  </h3>
</div>

Complete tutorial with:
- 🗳️ Real voting data from Chile's Chamber of Deputies (2025)
- 📖 Step-by-step analysis examples
- 💾 Downloadable datasets
- 📊 Interactive visualizations

## Installation

```r
devtools::install_github("bcallr/bcall")
library(bcall)
```

## Quick Start

### Principle 1: Automatic Clustering

Provide only rollcall data. The package clusters legislators automatically

```r
# Your data: data.frame with rownames
rollcall <- data.frame(
  Vote_01 = c(1, -1, 1, 0, NA),
  Vote_02 = c(1, -1, 1, 1, 1),
  Vote_03 = c(-1, 1, 1, 0, -1),
  row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D", "Leg_E")
)

# Run analysis (automatic pivot selection)
results <- bcall_auto(rollcall)

# Or specify pivot to control d1 orientation
results <- bcall_auto(rollcall, pivot = "Leg_B")

# Visualize
plot_bcall_analysis_interactive(results)
```

**Important:** Although `bcall_auto()` can automatically select a pivot, it is recommended to have knowledge of the voting data and manually specify the pivot to control the ideological orientation (d1 dimension). The pivot defines which side is "right" - if the orientation appears inverted, you can either:
- Change the pivot to a legislator from the opposite cluster, OR
- Simply interpret d1 values with reversed signs (positive ↔ negative)

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

**Analysis:**
- `bcall_auto(rollcall, pivot = NULL, threshold = 0.1)` - Automatic clustering + analysis
  - `pivot`: Optional. Legislator name to control d1 orientation
  - `threshold`: Minimum participation threshold (default: 0.1). Legislators must participate in at least this proportion of votes.
- `bcall(rollcall, clustering, pivot, threshold = 0.1)` - Analysis with your clustering
  - `pivot`: Required. Legislator name to control d1 orientation
  - `threshold`: Minimum participation threshold (default: 0.1)

**Visualization:**
- `plot_bcall_analysis(results)` - Static ggplot2 plot
- `plot_bcall_analysis_interactive(results)` - Interactive plotly plot

### About the `threshold` parameter

The `threshold` parameter controls the minimum participation level required for legislators to be included in the analysis:
- **Default value**: 0.1 (10% participation)
- **Lower values** (e.g., 0.05): Include legislators with very low participation
- **Higher values** (e.g., 0.3): Only include legislators who voted in at least 30% of votes
- Legislators below the threshold are excluded from the analysis
- Used in both `bcall_auto()` and `bcall()` to filter legislators by participation

## Real Data Example

```r
library(bcall)
library(readr)
library(dplyr)
library(tibble)

# Load your CSV with votes
rollcall <- read_csv("votes.csv") %>%
  rename(legislator = 1) %>%
  column_to_rownames("legislator")

# Automatic analysis
results <- bcall_auto(rollcall)

# Visualize
plot_bcall_analysis_interactive(results)
plot_bcall_analysis(results)
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
