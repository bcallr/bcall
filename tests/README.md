# Tests for bcall Package

This directory contains comprehensive test suites for the `bcall` package using `testthat`.

## Running Tests

```r
# Install testthat if needed
install.packages("testthat")

# Run all tests
devtools::test()

# Or using testthat directly
testthat::test_dir("tests/testthat")

# Run a specific test file
testthat::test_file("tests/testthat/test-bcall_auto.R")
```

## Test Files

### `test-bcall_auto.R`
Tests for the `bcall_auto()` function (Principle 1: Automatic Clustering)

**Coverage includes:**
- Valid input handling
- Custom pivot selection
- Different distance methods (Manhattan/Euclidean)
- Threshold variations
- NA and abstention handling
- Cluster distribution
- Error handling (invalid inputs, missing rownames, etc.)
- Edge cases (minimum data, large datasets, unanimous votes)
- Verbose mode

**Test count:** ~25 tests

### `test-bcall.R`
Tests for the `bcall()` function (Principle 2: User-Provided Clustering)

**Coverage includes:**
- Valid input with data.frame clustering
- Named vector clustering
- Different cluster labels
- Threshold variations
- NA and abstention handling
- Partial overlap between rollcall and clustering
- Cluster preservation in results
- Error handling (invalid types, wrong cluster count, missing pivot, etc.)
- Edge cases (minimum data, large datasets)
- Metadata validation

**Test count:** ~30 tests

### `test-plotting.R`
Tests for visualization functions: `plot_bcall_analysis()` and `plot_bcall_analysis_interactive()`

**Coverage includes:**
- Return type validation (ggplot/plotly objects)
- Default parameters
- Custom titles
- Show/hide legislator names
- Auto color detection
- Custom color_by variables
- Alpha and size parameters
- Standard cluster name recognition
- Error handling (invalid inputs, missing variables)
- Integration tests
- Large datasets
- NA value handling

**Test count:** ~25 tests

### `test-classes.R`
Tests for R6 classes: `Clustering` and `BCall`

**Coverage includes:**
- Class instantiation
- Expected fields and structure
- Clustering with different distance methods
- d1/d2 column presence and types
- Threshold handling
- Pivot respect
- Clustering-BCall integration
- Edge cases (minimal data, NA values, abstentions)

**Test count:** ~15 tests

### `test-data-validation.R`
Tests for data validation, input checking, and integration workflows

**Coverage includes:**
- Vote value validation (1, -1, 0, NA)
- Mixed vote patterns
- All-NA columns/rows
- Clustering unique value validation
- Threshold parameter validation
- Rowname preservation
- Special characters in names
- Full workflow integration (analysis + plotting)
- Result structure consistency
- Metadata completeness
- Real-world style data patterns

**Test count:** ~20 tests

## Test Statistics

- **Total test files:** 5
- **Approximate total tests:** ~115 tests
- **Code coverage areas:**
  - Main functions: `bcall()`, `bcall_auto()`
  - Visualization: `plot_bcall_analysis()`, `plot_bcall_analysis_interactive()`
  - R6 classes: `Clustering`, `BCall`
  - Data validation and error handling
  - Integration workflows

## Test Categories

### 1. **Unit Tests**
Individual function testing with controlled inputs

### 2. **Integration Tests**
Full workflow testing (clustering → analysis → visualization)

### 3. **Error Handling Tests**
Invalid inputs and edge case error messages

### 4. **Edge Case Tests**
- Minimum viable data (2 legislators, 1 vote)
- Large datasets (100+ legislators)
- Unanimous votes
- All NA/abstention scenarios
- Special characters in names

## Expected Test Results

All tests should pass when the package is properly installed and loaded:

```r
✓ |  OK F W S | Context
✓ | 115       | All tests [~5s]

══ Results ═══════════════════════════════════════
Duration: 5.2 s

OK:       115
Failed:   0
Warnings: 0
Skipped:  0
```

## Adding New Tests

When adding new functionality to `bcall`:

1. Create tests in the appropriate file
2. Follow the existing test structure
3. Include at minimum:
   - Valid input test
   - Error handling test
   - Edge case test
4. Run all tests to ensure no regressions

## Test Data Conventions

- Use descriptive legislator names: `Leg_A`, `Legislator_1`, etc.
- Use consistent vote patterns for predictable clustering
- Set `verbose = FALSE` to avoid cluttering test output
- Use `set.seed()` for reproducible random data

## Notes

- Tests use `expect_*()` functions from testthat
- Some tests may generate warnings (e.g., for unanimous votes) - these are suppressed with `suppressWarnings()` when expected
- Large dataset tests use random sampling with fixed seeds for reproducibility
