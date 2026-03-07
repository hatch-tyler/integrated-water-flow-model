# GWHydExtract

**Groundwater Hydrograph Extractor for IWFM**

GWHydExtract is a post-processing tool that extracts groundwater hydrographs from an IWFM simulation's "all heads" output file at user-specified locations, without re-running the model. It uses the same finite element interpolation as the IWFM simulation kernel, producing output in IWFM's native hydrograph format for direct consumption by downstream tools such as IWFM2OBS.

## Motivation

IWFM computes hydraulic heads at every model node at each timestep. Groundwater hydrographs at specific locations are generated during simulation using finite element interpolation of these nodal heads. If a user needs hydrographs at **new locations or layers** not specified in the original simulation input, they would normally need to re-run the model -- which can take hours for large models (e.g., C2VSimFG with 30,000+ nodes and 4 layers).

GWHydExtract eliminates this requirement. Given a completed simulation with all-heads output enabled, it can generate hydrographs at arbitrary locations in seconds.

## Prerequisites

- A completed IWFM simulation with the **all-heads output file** enabled in the groundwater main file. This is the file that records heads at every node, every layer, every timestep.
- The **preprocessor binary file** (produced by the IWFM PreProcessor) must be accessible.
- The simulation main file, groundwater main file, and all-heads output file must all be in their original locations relative to each other (GWHydExtract auto-discovers them).

## Usage

```
GWHydExtract <input_file>
GWHydExtract                    (prompts for input file)
```

### Windows

```
GWHydExtract_x64.exe MyExtraction.in
```

### Linux / Docker

```
GWHydExtract MyExtraction.in
# or
iwfm-gwhydextract MyExtraction.in
```

## Input File Format

The input file uses standard IWFM comment conventions: lines beginning with `C`, `c`, `*`, or `#` in column 1 are comments. Inline comments are delimited by `/`. Data lines are indented (column 1 is a space).

The file contains four control lines followed by the hydrograph specification table:

```
C  GWHydExtract Input File
C  ========================
C
C  Path to simulation main file (auto-discovers PP binary, GW main, all-heads)
   ..\Simulation\Simulation_MAIN.IN                / SIMFILE
C
C  Output file for extracted hydrographs
   GW_Hydrographs_Extracted.out                     / OUTFILE
C
C  Number of hydrographs to extract
   5                                                 / NHYD
C
C  Coordinate unit conversion factor (applied to X,Y coordinates below)
   3.2808                                            / FACTXY
C
C  Hydrograph specifications:
C  ID  HYDTYP  LAYER  X/NODE  [Y]  NAME
C
    1    0       1    592798.70   4209815.43   Well_MW-01
    2    0       2    592798.70   4209815.43   Well_MW-01_L2
    3    1       1    5432                     Node_5432_L1
    4    1       0    5432                     Node_5432_AllLayers
    5    0       1    622426.42   4296803.18   NewSite_A
```

### Control Lines

| Line | Parameter | Description |
|------|-----------|-------------|
| 1 | SIMFILE | Path to the IWFM simulation main file. Can be absolute or relative to the input file directory. GWHydExtract reads this file to auto-discover the preprocessor binary, groundwater main file, time stepping parameters, and the all-heads output file path. |
| 2 | OUTFILE | Path for the output hydrograph file. Can be absolute or relative to the input file directory. |
| 3 | NHYD | Number of hydrographs to extract (must be > 0). |
| 4 | FACTXY | Coordinate unit conversion factor applied to all X and Y values in the hydrograph table. Set this to match the `FACTXY` value in the groundwater main file so that coordinates are converted to the model's internal units. For example, if coordinates are in meters and the model uses feet, set `FACTXY = 3.2808`. Use `1.0` if coordinates are already in model units. |

### Hydrograph Specification Table

Each of the NHYD lines specifies one hydrograph to extract. The format depends on the hydrograph type (HYDTYP):

**At XY coordinates (HYDTYP = 0):**

```
ID  HYDTYP  LAYER  X  Y  NAME
```

The point (X, Y) can be anywhere within the model domain. GWHydExtract uses finite element shape functions to interpolate heads from the surrounding element nodes, exactly as the IWFM simulation does.

**At model node (HYDTYP = 1):**

```
ID  HYDTYP  LAYER  NODE_ID  NAME
```

NODE_ID is the user-facing node number (as defined in the preprocessor input), not the internal index.

### Column Definitions

| Column | Type | Description |
|--------|------|-------------|
| ID | Integer | Unique hydrograph identifier (appears in output header). |
| HYDTYP | Integer | `0` = at XY coordinates (finite element interpolation), `1` = at model node (direct lookup). |
| LAYER | Integer | Aquifer layer number (1 to NLayers), or `0` to average over all active layers at that location. |
| X | Real | X coordinate of the hydrograph location (HYDTYP=0 only). Multiplied by FACTXY. |
| Y | Real | Y coordinate of the hydrograph location (HYDTYP=0 only). Multiplied by FACTXY. |
| NODE_ID | Integer | Model node ID (HYDTYP=1 only). |
| NAME | String | Hydrograph name (up to 30 characters, no spaces). Appears in log output. |

## Output Format

The output file is in IWFM's native time-series hydrograph format, identical to the `*_GW_Hydrographs.out` file produced by the simulation. This format is directly readable by IWFM2OBS and other IWFM post-processing tools.

### Structure

```
*                                        ***************************************
*                                        *       GROUNDWATER HYDROGRAPH        *
*                                        *             (UNIT=FEET)             *
*                                        ***************************************
*          HYDROGRAPH ID        1           2           3       ...
*                  LAYER        1           2           3       ...
*                   NODE        0           0           0       ...
*                ELEMENT    12947       12947       12947       ...
*        TIME
09/30/1973_24:00            5.938       5.938       5.938       ...
10/31/1973_24:00            5.163       4.407       3.270       ...
```

- **Title block**: Identifies the file as a groundwater hydrograph with the head unit.
- **Header rows**: Hydrograph ID, layer, node (0 for AtXY hydrographs), and element for each column.
- **Data rows**: One row per timestep. Column 1 is the timestamp (`MM/DD/YYYY_HH:MM` format). Remaining columns are head values in F10.3 format (feet, meters, etc., matching the model's head output unit).

The number of data rows equals NTIME + 1 (initial conditions plus all simulation timesteps).

## Auto-Discovery

GWHydExtract requires only the simulation main file path. It automatically discovers all other files by parsing the IWFM file chain:

```
Simulation Main File
  |
  +-- Preprocessor Binary File  (grid geometry, stratigraphy)
  |
  +-- Groundwater Main File
        |
        +-- Head unit and conversion factor
        |
        +-- All-Heads Output File  (heads at every node/layer/timestep)
```

File paths in IWFM input files are resolved relative to their parent file's directory, following the same conventions as the IWFM simulation. The preprocessor binary and groundwater main file paths are resolved relative to the simulation main file directory. The all-heads file path is resolved relative to the simulation working directory.

## How It Works

1. **Initialization**: Parses the input file, then follows the IWFM file chain to discover the preprocessor binary, groundwater main file, and all-heads output file. Loads the finite element grid and stratigraphy from the preprocessor binary.

2. **Interpolation setup**: For each AtXY hydrograph, locates the containing element and computes the finite element shape function coefficients (interpolation weights). These coefficients are computed once and reused for every timestep. For AtNode hydrographs, maps the user node ID to the internal index.

3. **Extraction loop**: Reads the all-heads file one timestep at a time. For each timestep and each hydrograph:
   - **AtXY**: Computes `H = sum(coeff_i * Head(node_i, layer))` using the pre-computed shape function coefficients.
   - **AtNode**: Reads `Head(node, layer)` directly.
   - **Layer = 0**: Averages the interpolated/direct head value across all active layers at that location.

4. **Output**: Writes each timestep's values to the output file in IWFM's native hydrograph format.

## Accuracy

GWHydExtract uses the same `AppGridType%FEInterpolate` routine as the IWFM simulation kernel for locating elements and computing shape function coefficients. The all-heads output file stores heads that have already been converted to output units (e.g., feet), so no additional conversion is needed.

When tested against the C2VSimFG model (30,179 nodes, 32,537 elements, 4 layers, 576 monthly timesteps), extracted hydrographs matched the simulation's native hydrograph output with a maximum absolute difference of 0.001 -- the last-digit resolution of the F10.3 output format. This difference arises because the all-heads file stores values at output format precision, while the simulation computes interpolation from full double-precision internal values.

For practical purposes, the output is identical to what the simulation produces.

## Performance

| Model | Nodes | Elements | Layers | Timesteps | Hydrographs | Runtime |
|-------|-------|----------|--------|-----------|-------------|---------|
| C2VSimFG | 30,179 | 32,537 | 4 | 577 | 8 | 19 sec |

The dominant cost is reading the all-heads file (which contains `NLayers x NNodes` values per timestep). The number of hydrographs has negligible impact on runtime since the interpolation is a small fraction of the I/O cost.

## Building

GWHydExtract is an optional build target, disabled by default.

### PowerShell (Windows)

```powershell
.\build-iwfm.ps1 -GWHydExtract
# or build only GWHydExtract:
.\build-iwfm.ps1 -Target GWHydExtract -GWHydExtract
```

### CMake

```bash
cmake .. -DIWFM_BUILD_GWHYDEXTRACT=ON
cmake --build . --target GWHydExtract
```

### Docker

GWHydExtract is included in the runtime container when built with the default Dockerfile.runtime:

```bash
docker build -t iwfm-runtime:2025.0 -f docker/Dockerfile.runtime .
docker run --rm -v /path/to/model:/data iwfm-runtime:2025.0 iwfm-gwhydextract input.in
```

The output executable is:
- Windows: `Bin/GWHydExtract_x64.exe`
- Linux: `Bin/GWHydExtract`

## Example

This example extracts hydrographs at two well locations across all four layers of the C2VSimFG model.

**Input file** (`GWHydExtract_test.in`):

```
C  GWHydExtract Test Input File
C  ============================
C  Path to simulation main file
   ..\Simulation\C2VSimFG.in                        / SIMFILE
C
C  Output file for extracted hydrographs
   GWHydExtract_test.out                             / OUTFILE
C
C  Number of hydrographs to extract
   8                                                 / NHYD
C
C  Coordinate unit conversion factor (FACTXY=3.2808 converts meters to feet)
   3.2808                                            / FACTXY
C
C  Hydrograph specifications:
C  ID  HYDTYP  LAYER  X              Y              NAME
C
    1    0       1    592798.7048    4209815.426    S_380313N1219426W001%1
    2    0       2    592798.7048    4209815.426    S_380313N1219426W001%2
    3    0       3    592798.7048    4209815.426    S_380313N1219426W001%3
    4    0       4    592798.7048    4209815.426    S_380313N1219426W001%4
    5    0       1    622426.4231    4296803.182    S_381150N1215899W001%1
    6    0       2    622426.4231    4296803.182    S_381150N1215899W001%2
    7    0       3    622426.4231    4296803.182    S_381150N1215899W001%3
    8    0       4    622426.4231    4296803.182    S_381150N1215899W001%4
```

**Run**:

```
GWHydExtract_x64.exe GWHydExtract_test.in
```

**Console output**:

```
* INFO : Program GWHydExtract - Groundwater Hydrograph Extractor
* INFO : 8 hydrographs specified (8 AtXY, 0 AtNode)
* INFO : BDT=09/30/1973_24:00  EDT=09/30/2021_24:00  NTIME=576
* INFO : Grid: 30179 nodes, 32537 elements, 4 layers
* INFO : Processing 577 timesteps...
* INFO : Extraction complete. 8 hydrographs written.
* INFO : NORMAL TERMINATION - GWHydExtract

**************************************************
TOTAL RUN TIME: 19.085 SECONDS
**************************************************
```

**Output file** (`GWHydExtract_test.out`) -- first few data lines:

```
09/30/1973_24:00            5.938       5.938       5.938       5.938      -6.257      11.151       7.034       7.034
10/31/1973_24:00            5.163       4.407       3.270       3.252      -4.355       0.581       5.267       5.837
11/30/1973_24:00            4.812       3.964       2.606       2.528      -2.583      -0.209       4.535       5.629
```

## Integration with IWFM2OBS

The output file is format-compatible with IWFM's native `*_GW_Hydrographs.out` files. To use it with IWFM2OBS for PEST calibration:

1. Run GWHydExtract to generate hydrographs at calibration well locations.
2. Point the IWFM2OBS input file at the GWHydExtract output (as the GW hydrograph file).
3. IWFM2OBS will read it identically to a simulation-produced hydrograph file.

This workflow enables extracting hydrographs at new well locations for parameter estimation without modifying the simulation input files or re-running the model.

## Error Conditions

| Error | Cause | Resolution |
|-------|-------|------------|
| Cannot open simulation main file | SIMFILE path is incorrect or file does not exist | Check the path in line 1 of the input file. Relative paths are resolved from the input file's directory. |
| All-heads output file path is blank | The groundwater main file does not specify an all-heads output file | Enable all-heads output in the GW main file and re-run the simulation. |
| Hydrograph is outside the model grid | The (X, Y) coordinates (after FACTXY conversion) fall outside all model elements | Verify coordinates and FACTXY. Coordinates must be in the model's coordinate system after conversion. |
| Node ID not found in model grid | HYDTYP=1 node ID does not exist in the preprocessor grid | Use a valid node ID from the model. |
| Layer exceeds model layers | LAYER value is greater than the number of aquifer layers | Use a layer number between 0 and NLayers. |
| Computed NTIME <= 0 | The begin/end dates or time unit in the simulation file are inconsistent | Verify the simulation main file is valid and the simulation completed successfully. |

## Log File

GWHydExtract writes diagnostic messages to `GWHydExtract_Messages.out` in the current working directory. This file records all discovered paths, grid dimensions, and processing status. Check this file first when troubleshooting.

## Source Files

| File | Description |
|------|-------------|
| `GWHydExtract_Main.f90` | Main program driver. Handles command-line arguments, timer, and log file setup. |
| `Class_GWHydExtract.f90` | Core module defining `GWHydExtractType` with `New`, `Run`, and `Kill` methods. Contains all file parsing, grid loading, interpolation, and output logic. |

## License

Part of IWFM (Integrated Water Flow Model). Copyright (C) 2005-2025, State of California, Department of Water Resources. Licensed under GPL v2.
