# PV Stringing Wizard (AutoLISP) — README

## Purpose
The PV Stringing Wizard streamlines module stringing inside AutoCAD. It guides you through defining inverters/strings, selecting start and waypoint modules, then automatically places start/end markers, draws and cleans up the string route, counts modules traversed, and labels each string according to your plan.

This tool is designed to keep you in control of the routing while offloading the repetitive steps: counting, markers, fillets, and labeling.

---

## Key Capabilities
- Identify all instances of a selected module block type.
- Accept a stringing plan (inverters, strings per inverter, modules per string).
- Semi-automated routing:
  - You pick the start module and the next module in a straight run (end-of-row or waypoint).
  - The wizard draws a polyline segment and counts all modules it crosses (including endpoints).
  - Repeats until the target module count is reached.
- Automatic marker placement:
  - Minus block at the first module center.
  - Plus block at the final module center.
- Polyline cleanup: fillet corners to 12" radius.
- Labeling: place a string title at the longest horizontal segment (bottom-center justified, offset 1/8").
- Debug mode for diagnostics during setup and testing.

---

## Requirements
- AutoCAD with AutoLISP support.
- A consistent module block (same definition name) for the array you intend to string.
- Marker blocks present in the drawing or in a searchable path:
  - Minus marker block: default name `PV_STRING_MINUS`
  - Plus marker block: default name `PV_STRING_PLUS`
- Drawing units assumed in inches for the default 12" fillet and 1/8" label offset.

---

## Installation
1. Save the LISP file as `pvw-stringing-wizard.lsp` in a known folder (for example: `C:/CAD/lisp/pv/`).
2. Load the file:
   - Temporarily via APPLOAD, or
   - Automatically via Startup Suite, or
   - Autoload with a partial CUIx by adding `(load "pvw-stringing-wizard")` in the matching MNL file.
3. (Optional) Add a ribbon button or menu macro that calls `PVW`.

---

## Configuration (defaults)
The wizard uses these defaults, which you can edit near the top of the LISP file:
- `*pvw-minus-block*` → name of the minus marker block (default `PV_STRING_MINUS`)
- `*pvw-plus-block*` → name of the plus marker block (default `PV_STRING_PLUS`)
- `*pvw-text-style*` → MTEXT style used for labels (default `Standard`)
- `*pvw-text-height*` → label height (default `0.125`)
- `*pvw-fillet-radius*` → fillet radius in drawing units (default `12.0`)
- `*pvw-eps*` → small numeric tolerance used in calculations

A debug toggle command `PVWDEBUG` is available to turn additional messaging on or off.

---

## Getting Started
1. Prepare a test drawing with a uniform module block (e.g., a rectangular symbol repeated in rows/columns).
2. Ensure the minus/plus marker blocks are available.
3. Run the command: `PVW`.
4. Follow the prompts.

---

## End-to-End Workflow

### 1) Select module type
- The wizard prompts you to pick a module block instance in the drawing.
- It determines the block’s effective name (works with dynamic blocks) and indexes all matching inserts (centers cached for counting).

### 2) Enter stringing plan
- Prompts (sequence example):
  - How many inverters?
  - Names of inverters? (comma-separated, e.g., A,B)
  - Number of strings for inverter A?
  - Number of strings for inverter B?
  - Number of modules for STRING A-1?
  - …and so on for each string.
- The plan is flattened into a worklist of strings with target module counts.

### 3) Begin each string
- The wizard announces the string name and its target module count (for example, STRING A-1: 15 modules).
- You are prompted to select the **first** module:
  - A minus marker is inserted at the module’s center.
  - A new polyline is started at this location.

### 4) Add a segment (waypoint)
- You are prompted to select the **next** module in a straight run (often the end of the row or a clear waypoint).
- The wizard draws a polyline segment from the current point to the selected module center.
- The wizard counts the number of module centers intersected by the segment, including start and end modules (robust distance-to-segment method with tolerance).

### 5) Track progress vs. target
- The remaining module count for the string is reduced by the segment total.
- If the target is not met:
  - The wizard reports how many modules are still needed (for example, 3 more modules needed for STRING A-1) and prompts for the next module in line.
- If the target is met or exceeded:
  - The wizard places a plus marker at the final module center.
  - The current polyline is ended.

### 6) Cleanup and label
- The wizard fillets the string polyline corners to a 12" radius.
- It detects the longest horizontal segment of the string and places an MTEXT label with bottom-center justification, offset 1/8" above the segment’s midpoint. If no horizontal segment exists, it uses a reasonable fallback (e.g., polyline midpoint).

### 7) Repeat for remaining strings
- Steps 3–6 repeat for each string in your plan until all are completed.

---

## Counting Rules (summary)
- Counting is performed against the cached centers of all found module inserts of the selected block definition.
- For each new segment (from the previous module center to the newly selected module center), the wizard counts all module centers within a small tolerance from that line segment.
- The tolerance should be set large enough to catch centers along an intended straight run, but small enough to ignore adjacent rows. A starting point of 6–8 inches works well in many layouts; you may adjust if modules are spaced unusually or drawn at different scales.

---

## Labeling Rules
- Label text: the string name (e.g., STRING A-1).
- Text style: `*pvw-text-style*` (default `Standard`).
- Height: `*pvw-text-height*` (default `0.125`).
- Placement: longest horizontal segment midpoint, bottom-center justification, offset upward by the text height. Fallback if no horizontal segment is found.

---

## Markers
- Minus marker: placed at the center of the first module of the string.
- Plus marker: placed at the center of the final module of the string.
- Ensure both marker block definitions are loaded or discoverable from support paths. You may rename them via configuration.

---

## Fillet Behavior
- Default radius is 12" (assumes drawing units in inches).
- The wizard applies fillets to corners of the newly created string polyline.
- If your standards require a different bend radius, adjust `*pvw-fillet-radius*`.

---

## Debug Mode
- Run `PVWDEBUG` to toggle diagnostic messaging.
- In debug mode, the wizard prints additional information (counts, totals) and can be extended to display temporary markers for testing.
- Use debug mode when calibrating counting tolerance or validating array indexing.

---

## Best Practices
- Use a **single module block definition** for the array you plan to string. Mixed block names in one array can cause incorrect counts.
- Keep block insertion points consistent (e.g., centered), especially if you rely on center-based counting.
- Verify drawing units. If your template uses feet or centimeters, adjust the fillet radius and label offset accordingly.
- Consider placing module blocks on a dedicated layer to simplify selection and visibility checks.

---

## Error Handling & Edge Cases
- If the user cancels while picking modules, the wizard stops the current string cleanly.
- If the selected module doesn’t match the chosen block definition, the wizard prompts again.
- If the segment selection jumps across multiple rows (due to an unusually large tolerance or misclick), the count may overshoot; the wizard will still place the plus marker when the target is reached or exceeded.
- If the array is irregular (gaps, rotated modules), counting may require a smaller tolerance and/or shorter waypoint selections.

---

## Limitations (v1)
- The initial release assumes **straight-run waypoints** chosen by the user; it does not auto-detect row/column topology or route around obstacles.
- Filleting is applied uniformly; no per-vertex exceptions.
- Quadrant-specific lead-in/out offsets from marker circles are not yet applied; start/end segments currently originate/terminate at module centers (may be enhanced later to detect the correct quadrant and offset by marker radius).

---

## Roadmap
- Quadrant-aware lead-in/out points with configurable marker radius.
- Row/column grouping with tolerance-based clustering.
- Optional auto-serpentine routing along detected rows.
- Enhanced fillet control and smoothing routines.
- Dialog-driven input (DCL/.NET) for string plans and presets.
- Validation/reporting: CSV export of string counts and totals.

---

## Commands
- `PVW` — Launch the PV Stringing Wizard.
- `PVWDEBUG` — Toggle debug messages on/off.

---

## File Naming & Autoload
- Recommended file name: `pvw-stringing-wizard.lsp`
- If you load through a partial CUIx (recommended), create a matching MNL file (same name as the CUIx) and include `(load "pvw-stringing-wizard")` for automatic loading.
- Macro for a ribbon button/menu: `^C^C_PVW`

---

## Support
- If counts are off, first try:
  - Lowering the counting tolerance.
  - Verifying that all modules are the same block definition.
  - Checking that centers align reasonably in a straight run.
- Use `PVWDEBUG` to observe intermediate steps and recalibrate.

---
