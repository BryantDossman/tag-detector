# VHF Tag Detection Analysis - User Walkthrough

This guide will help you analyze new VHF receiver data files to detect wildlife tag transmissions, even if you're not familiar with R programming.

## Quick Start (For the Impatient)

1. **Add your data files** to the `receiver_data` folder (must be .txt files)
2. **Open Terminal/Command Prompt** and navigate to this folder
3. **Run the analysis**: `Rscript process_receiver_data.R`
4. **Check results** in `receiver_detections.csv`

That's it! Read below for detailed instructions and troubleshooting.

---

## Prerequisites

- **R installed** on your computer ([Download here](https://cran.r-project.org/))
- **Data files** in the correct format (see "Data Format" section below)

## Step-by-Step Instructions

### Step 1: Prepare Your Data

1. **Copy your receiver data files** into the `receiver_data` folder
2. **File requirements**:
   - Must be `.txt`, `.dat`, or `.log` files
   - Should contain pulse data in the format: `p1,timestamp,freq_offset,signal,noise,snr`
   - Example: `p1,1757957925.9191,3.398,-2.51,-35.25,32.74`

### Step 2: Run the Analysis

**Option A: Using Terminal/Command Prompt (Recommended)**
1. Open Terminal (Mac/Linux) or Command Prompt (Windows)
2. Navigate to the Tag Detector folder:
   ```bash
   cd "/path/to/Tag Detector"
   ```
3. Run the analysis:
   ```bash
   Rscript process_receiver_data.R
   ```

**Option B: Using RStudio**
1. Open RStudio
2. Set working directory to the Tag Detector folder
3. Run: `source("process_receiver_data.R")`

### Step 3: Review Results

The analysis will create several output files:

1. **`receiver_detections.csv`** - Main results file with all detected tags
2. **`receiver_file_summary.csv`** - Summary of each processed file
3. **Console output** - Real-time progress and summary statistics

## Understanding Your Results

### Detection Summary (Console Output)
```
=== ANALYSIS COMPLETE ===
Files processed: 4 
Total detections: 78 

Detections by tag:
  Tag 127: 15 detections
  Tag 233: 30 detections  
  Tag 243: 33 detections

Quality summary:
  Mean match score: 0.755 (range: 0.704 - 0.825)
  Mean timing error: 0.15 ms (max: 0.36 ms)
  Detection time span: 1.60 hours
```

### Key Metrics to Look For:
- **Total detections**: How many tag transmissions were found
- **Match score**: Higher is better (0.7+ is good, 0.8+ is excellent)
- **Timing error**: Lower is better (under 1ms is excellent)
- **Detection time span**: How long your recording session covered

### Main Results File (receiver_detections.csv)

Open this file in Excel or any spreadsheet program. Key columns:

| Column | Description |
|--------|-------------|
| `tag_id` | Which tag was detected (127, 233, 243, etc.) |
| `datetime` | When the detection occurred |
| `match_score` | Detection confidence (0.7-1.0, higher = better) |
| `source_file` | Which data file contained this detection |
| `mean_sig_db` | Average signal strength |
| `duration_s` | How long the tag burst lasted |

## Troubleshooting

### No Detections Found
If you see "No tag detections found", try these solutions:

1. **Check your data format**:
   ```bash
   Rscript -e "source('main.R'); validate_data_files()"
   ```

2. **Try relaxed parameters** (for noisy data):
   ```bash
   Rscript -e "source('process_receiver_data.R'); run_relaxed_analysis()"
   ```

3. **Verify tag database**: Make sure `tags.csv` contains the tags you expect to find

### Common Issues

**"Error: No files found"**
- Check that your data files are in the `receiver_data` folder
- Ensure files have `.txt`, `.dat`, or `.log` extensions

**"Error in source('main.R')"**
- Make sure you're running the command from the Tag Detector folder
- Check that all R script files are present

**Very few detections**
- Your detection parameters might be too strict
- Try the relaxed analysis: `run_relaxed_analysis()`
- Check if your tags are in the database (`tags.csv`)

## Advanced Usage

### Adjusting Detection Parameters

If you need to fine-tune the analysis, you can modify parameters in `process_receiver_data.R`:

```r
# Standard parameters (in the script)
DETECTION_PARAMS <- list(
  pulse_slop_ms = 1.5,      # Timing tolerance (increase for noisy data)
  freq_slop_khz = 2.0,      # Frequency tolerance
  sig_slop_db = 10,         # Signal strength tolerance  
  min_match_score = 0.7,    # Minimum detection score (0.6-0.9)
  max_pulse_rate = 0        # Rate limiting (0 = disabled)
)
```

### Processing Different File Types

The system supports multiple file patterns:
```bash
# Process only .dat files
Rscript -e "source('main.R'); run_tag_analysis('receiver_data', file_pattern='*.dat')"

# Process .log files  
Rscript -e "source('main.R'); run_tag_analysis('receiver_data', file_pattern='*.log')"
```

### Adding New Tags

To detect different tags, edit `tags.csv` with your tag specifications:
- `id`: Tag ID number
- `tagFreq`: Tag frequency (MHz)  
- `g1`, `g2`, `g3`: Gap timings (milliseconds)
- `bi`: Burst interval (seconds)
- `dfreq`: Expected frequency offset (kHz)

## Expected Data Format

Your receiver data files should contain lines like:
```
p1,1757957925.9191,3.398,-2.51,-35.25,32.74
p1,1757957925.9899,3.401,-2.443,-39.39,36.95
```

Format: `antenna,timestamp,freq_offset_khz,signal_db,noise_db,snr_db`

## Getting Help

- **File format issues**: Run `validate_data_files()` to check your data
- **No detections**: Try `run_relaxed_analysis()` for more sensitive detection
- **Parameter tuning**: Adjust values in `DETECTION_PARAMS` section of the script
- **Multiple analyses**: You can run the script multiple times with different parameters

## Quick Reference Commands

```bash
# Standard analysis
Rscript process_receiver_data.R

# Relaxed analysis (more sensitive)
Rscript -e "source('process_receiver_data.R'); run_relaxed_analysis()"

# Validate data files only  
Rscript -e "source('process_receiver_data.R'); validate_data_files()"

# Interactive R session
R
source("process_receiver_data.R")
results <- run_receiver_analysis()
```

---

## What the System Detects

This system looks for **VHF wildlife tracking tags** that transmit in specific patterns:
- **4-pulse bursts** with precise timing gaps
- **Consistent frequency** within each burst  
- **Regular burst intervals** (typically 30-40 seconds apart)
- **Known tag signatures** from the tag database

The algorithm matches your recorded data against known tag patterns and scores each potential detection based on timing accuracy, frequency consistency, and signal quality.

Good luck with your analysis!