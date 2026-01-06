# Laguna Bottlenose Dolphin Group Dynamics Analysis

## Overview

This repository contains R scripts for analyzing bottlenose dolphin group behavior during cooperative foraging with artisanal fishers in Laguna, Brazil. The analysis focuses on individual-level roles, herding/cornering behavior, and group coordination.

---

## Research Questions

### Primary Questions:
1. **Role Specialization**: Do dolphins assume different roles within a group while foraging alongside artisanal fishers?
2. **Role Consistency**: Do individuals consistently assume the same roles across foraging events?
3. **Individual Predictors**: What individual-level factors influence role assignment? (age class, sex, social centrality, specialization index)

### Bonus Questions:
4. **Communication**: Are whistles produced during foraging with humans, and does production vary by group size (1, 2, 3, 4+ dolphins)?
5. **Individual Success**: Do foraging dolphins have varying degrees of individual foraging success (via terminal buzz analysis)?
6. **Functional Communication**: Is whistle production functionally linked to foraging coordination/success?

---

## Key Adaptations for Laguna System

Unlike previous analyses of Guiana dolphins in open water, this system has unique characteristics:

### 1. Fisher Line Constraint
- Fishers stand along the beach (x-axis in drone videos)
- Creates a spatial barrier/reference line
- Dolphins herd mullet TOWARD this line (not away from obstacles)
- All metrics account for proximity and orientation to fisher line

### 2. Angle Correction
- Drone tracker sometimes flips dolphin heading 180°
- Script includes `correct_angle_flips()` function to detect and fix these errors
- Uses temporal consistency: sudden >120° changes are corrected
- Preserves original angles in separate column for validation

### 3. Cornering/Herding Metrics
New metrics specifically designed to detect herding behavior:
- **Polygon elongation**: Linear formations indicate active herding
- **Convergence angle**: How aligned dolphins are toward fishers
- **Approach vector variance**: Multiple approach angles (McGarvey et al. 2025 finding)
- **Flanking positions**: Front/rear/left/right relative to group centroid

---

## Files Included

### Main Scripts:
1. **Laguna_Bottlenose_Dolphin_Group_Metrics.R** - Core functions
2. **Test_Laguna_Metrics.R** - Example usage and testing
3. **runfolder_DGM.R** - Batch processing script (from original framework)

### Data:
- **20240521_DJI_0007_output.csv** - Example dolphin tracker output

### Documentation:
- **Laguna_Dolphin_Fisher_Foraging_Research_Framework.md** - Comprehensive research framework
- **Introduction_Outline_First_Sentences.md** - Paper structure

---

## Installation

### Required R Packages:

```r
install.packages(c(
  "tidyverse",  # Data manipulation
  "zoo",        # Time series interpolation
  "igraph",     # Network/graph analysis (MST)
  "sf",         # Spatial operations (polygons)
  "circular",   # Circular statistics
  "patchwork",  # Plot arrangements
  "viridis"     # Color palettes
))
```

---

## Usage

### Basic Workflow:

```r
# 1. Load functions
source("Laguna_Bottlenose_Dolphin_Group_Metrics.R")

# 2. Read your data
df <- read_csv("20240521_DJI_0007_output.csv")

# 3. Process and calculate metrics
results <- compute_laguna_dolphin_metrics(
  df,
  correct_angles = TRUE,      # Fix 180° flips
  interpolate = TRUE,          # Fill short gaps
  maxgap = 2,                  # Max frames to interpolate
  Nmin = 2                     # Minimum dolphins for group metrics
)

# 4. Extract individual-level data
individual_data <- extract_individual_metrics(results$data)

# 5. Visualize
p_timeseries <- plot_group_metrics_timeseries(results$frame_metrics)
p_trajectories <- plot_spatial_trajectories(results$data)

# 6. Save outputs
ggsave("group_metrics.png", p_timeseries, width = 14, height = 12)
write_csv(results$frame_metrics, "frame_metrics.csv")
write_csv(individual_data, "individual_metrics.csv")
```

### Quick Test:

```r
# Run the test script on example data
source("Test_Laguna_Metrics.R")
```

This will:
- Process the example video
- Generate plots
- Export CSVs
- Identify cornering events
- Print summary statistics

---

## Metrics Computed

### Group-Level Metrics (per frame):

#### Spatial Cohesion:
- **MPD** (Mean Pairwise Distance): Average distance between all dolphin pairs
- **MST_mean** (Minimum Spanning Tree): Network connectivity measure
- **Distance_to_fishers_mean**: Average distance to fisher line
- **Distance_to_fishers_min**: Closest dolphin to fishers

#### Shape & Formation:
- **ConvexHull_area**: Area of polygon formed by dolphins
- **Polygon_elongation**: Perimeter²/Area (higher = more linear/stretched)

#### Heading Coordination:
- **Heading_MRL** (Mean Resultant Length): 0-1, higher = more aligned
- **Heading_pairSim**: Pairwise heading similarity
- **Convergence_angle**: How aligned toward fishers (lower = converging)
- **Approach_variance**: Circular variance of approach angles

### Individual-Level Metrics (per dolphin, per frame):

#### Position:
- **dist_from_centroid**: Distance from group center
- **bearing_from_centroid**: Angular position relative to center
- **position_category**: Front / Rear / Left flank / Right flank / Center
- **Distance_to_fishers**: Individual's distance to fisher line

#### Spatial Role:
Based on position relative to centroid and movement toward fishers:
- **Front**: Leading toward fishers (potential cue-giver)
- **Left/Right flank**: Herding from sides
- **Rear**: Following/supporting
- **Center**: Within group core

---

## Understanding the Metrics

### Cornering/Herding Behavior

**What to look for:**
- **Low convergence angle** (<45°) = dolphins heading toward fishers
- **High polygon elongation** (>median) = linear formation (not circular milling)
- **Low distance to fishers** (<15m) = active approach
- **High approach variance** = coming from multiple directions (McGarvey finding)

**Example interpretation:**
```
Frame 500:
  Convergence_angle = 30°     → Aligned toward fishers ✓
  Polygon_elongation = 85     → Linear formation ✓
  Distance_to_fishers_min = 8m → Close approach ✓
  Approach_variance = 0.6     → Multiple angles ✓
  → LIKELY CORNERING EVENT
```

### Role Classification

Based on position and behavior:

| Role | Position | Distance to Fishers | Heading | Behavior |
|------|----------|-------------------|---------|----------|
| **Cue-giver/Leader** | Front | Closest (<10m) | Toward fishers | Signals net cast |
| **Herder/Driver** | Left/Right flank | Medium (10-20m) | Perpendicular | Active maneuvering |
| **Barrier/Blocker** | Rear | Far (>20m) | Variable | Blocks escape |
| **Follower** | Center/Rear | Variable | Following others | Opportunistic |

---

## Outputs

### CSV Files:

1. **frame_metrics_laguna.csv**
   - One row per frame
   - Group-level metrics
   - Use for time-series analysis

2. **individual_metrics_laguna.csv**
   - One row per dolphin per frame
   - Individual positions and roles
   - Use for role classification

3. **cornering_analysis.csv**
   - Frame metrics + cornering classification
   - Binary flag for potential cornering events

### Plots:

1. **group_metrics_timeseries.png**
   - 5-panel plot showing all metrics over time
   - Group size, spatial cohesion, heading, polygon shape, herding

2. **dolphin_trajectories.png**
   - Overhead view of dolphin paths
   - Fisher line shown as red dashed line
   - Each dolphin colored differently

3. **cornering_detection.png**
   - Convergence angle over time
   - Distance to fishers (secondary axis)
   - Orange points = identified cornering events

---

## Next Steps

### For Role Analysis:

```r
# 1. Classify roles based on metrics
individual_data <- individual_data %>%
  mutate(
    role = case_when(
      position_category == "front" & Distance_to_fishers < 10 ~ "cue_giver",
      position_category %in% c("left_flank", "right_flank") ~ "herder",
      position_category == "rear" ~ "barrier",
      TRUE ~ "follower"
    )
  )

# 2. Calculate role repeatability
role_repeatability <- individual_data %>%
  group_by(ObjectID) %>%
  summarize(
    primary_role = names(which.max(table(role))),
    role_consistency = max(table(role)) / n()
  )

# 3. Test predictors
# (Requires linking to photo-ID, age, sex, social network data)
```

### For Cornering Detection:

```r
# Identify cornering sequences
cornering_events <- results$frame_metrics %>%
  mutate(
    is_cornering = Convergence_angle < 45 &
                   Polygon_elongation > median(Polygon_elongation, na.rm = TRUE) &
                   Distance_to_fishers_min < 15
  ) %>%
  # Identify continuous sequences
  mutate(
    cornering_id = cumsum(is_cornering != lag(is_cornering, default = FALSE))
  ) %>%
  filter(is_cornering) %>%
  group_by(cornering_id) %>%
  summarize(
    start_frame = min(FrameIndex),
    end_frame = max(FrameIndex),
    duration_frames = n(),
    min_distance = min(Distance_to_fishers_min),
    mean_convergence = mean(Convergence_angle)
  )
```

---

## Troubleshooting

### Common Issues:

**1. Angle flipping not corrected properly:**
- Check `angle_col` name matches your data
- Adjust `threshold_deg` parameter (default 120°)
- Inspect `*_original` column to validate corrections

**2. Too many NA values in metrics:**
- Check `Nmin` parameter (set to 1 for solo dolphins)
- Increase `maxgap` for more interpolation
- Verify coordinates are in meters (not pixels)

**3. Fisher line position incorrect:**
- Manually specify `fisher_line_y` parameter
- Check that y-coordinates increase away from beach
- Plot trajectories to visually inspect

**4. Polygon metrics all NA:**
- Need at least 3 dolphins for convex hull
- Check data has sufficient group sizes
- Filter for `N >= 3` before computing

---

## Data Format

### Required Columns (from dolphin-tracker):

```
FrameIndex          - Integer frame number
ObjectID            - Unique dolphin ID (can be numeric or character)
CenterX_m           - X coordinate in meters
CenterY_m           - Y coordinate in meters
MovingAvgAngle_deg  - Heading angle in degrees (-180 to 180)
```

### Optional Columns:
```
Angle_deg                - Raw angle (before filtering)
FilteredAngle_deg        - Filtered angle
GSD_cmpx                 - Ground sampling distance
EstAltitude_m            - Drone altitude
```

---

## Comparison to McGarvey et al. (2025)

### What They Found (Group-Level):
- Fishers more successful when dolphins approach from different trajectories
- Diving synchrony consistency matters
- Group proximity and heading are repeatable

### What We Add (Individual-Level):
- **Decompose** group variance → individual contributions
- **Identify** which dolphins create trajectory variance (herders vs. cue-givers)
- **Quantify** individual positions (front/flank/rear)
- **Classify** roles and test consistency
- **Detect** cornering events explicitly

---

## Citation

If you use this code, please cite:

- McGarvey et al. (2025) *Current Zoology* - Group coordination framework
- Cantor et al. (2023) *PNAS* - Foraging synchrony and survival benefits
- This repository: [GitHub link when published]

---

## Contact

For questions or issues:
- Check the research framework document first
- Review example outputs
- Create an issue on GitHub
- Contact: [your email]

---

## License

This code is provided for research purposes. Please cite appropriately.

---

## Changelog

### Version 1.0 (2025-01-04):
- Initial release
- Angle flip correction implemented
- Cornering metrics added
- Individual role classification framework
- Fisher line awareness integrated
- Tested on Laguna bottlenose dolphin data

### Planned Features:
- Automatic role classification (clustering)
- Integration with acoustic data (whistles, buzzes)
- Multi-video batch processing
- Statistical analysis templates
- Repeatability calculations
- Photo-ID matching utilities
