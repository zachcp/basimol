# Claude Guide: Working with Basimol in Blender

## Overview
Basimol is a Clojure-based molecular visualization tool running in Basilisp (Python implementation of Clojure) within Blender. This guide focuses on proper namespace loading and using basimol functions directly.

## Environment
- **Language**: Basilisp (Python-based Clojure)
- **Host**: Blender 4.5+ with nREPL server
- **Project**: basimol - molecular visualization in Blender

## Quick Start

### 1. Standard Startup Sequence
```clojure
;; Load basimol namespaces (this should work reliably)
(require '[basimol.core :as core])
(require '[basimol.styles :as styles])

;; Verify loading worked
(core/say-hello)
;; => "hello"

;; Clear any existing objects to start clean
(core/clear-objects)
```

### 2. Basic Molecular Visualization
```clojure
;; Load a PDB structure
(def structure (core/load-pdb "1fap"))

;; Create materials with custom properties
(def cartoon-material 
  (core/create-basic-material 
    "protein" 
    {"Base Color" [0.8 0.2 0.2 1.0] "Alpha" 0.8}))

(def ligand-material 
  (core/create-basic-material 
    "ligand" 
    {"Base Color" [0.2 0.8 0.2 1.0] "Emission Strength" 2.0}))

;; Filter and visualize different components
(def protein (core/filter-polymer structure))
(def ligand (core/filter-resname structure "RAP"))

;; Draw with different styles
(core/draw! (aget structure protein) :cartoon {} cartoon-material)
(core/draw! (aget structure ligand) :ball+stick {} ligand-material)
```

## Core Functions

### Loading and Filtering
- `(core/load-pdb "code")` - Load PDB structure by ID
- `(core/filter-polymer arr)` - Filter to polymer chains
- `(core/filter-resname arr "NAME")` - Filter by residue name  
- `(core/filter-chain arr "A")` - Filter by chain ID
- `(core/filter-element arr "C")` - Filter by element
- `(core/filter-amino-acids arr)` - Filter to amino acids only

### Visualization
- `(core/draw! arr style overrides material)` - Main drawing function
- `(core/clear-objects)` - Remove all molecular objects
- `(core/create-basic-material name props)` - Create materials

### Styles Available
- `:cartoon` - Protein cartoon representation
- `:ball+stick` - Ball and stick model
- `:surface` - Molecular surface
- `:spheres` - Space-filling spheres
- `:sticks` - Stick representation
- `:ribbon` - Ribbon representation

### Camera Control
- `(core/get-view)` - Capture current camera view matrix
- `(core/set-view! matrix)` - Set camera to specific view

## Style Customization

### Material Properties
```clojure
;; Common material properties
{"Base Color" [r g b a]           ;; RGBA color (0-1 range)
 "Emission Strength" 2.0          ;; Glow intensity
 "Emission Color" [r g b a]       ;; Glow color
 "Alpha" 0.5                      ;; Transparency
 "Roughness" 0.2                  ;; Surface roughness
 "Metallic" 0.0}                  ;; Metallic property
```

### Style Overrides
```clojure
;; Override default style parameters
(def custom-styles
  {:cartoon {"Loop Radius" 1.1 "DSSP" true}
   :ball+stick {"Sphere Radii" 0.4 "Bond Radius" 0.3}
   :surface {"Probe Size" 1.4 "Scale Radii" 1.2}})

(core/draw! structure :cartoon custom-styles material)
```

## Complete Example

```clojure
;; Standard startup
(require '[basimol.core :as core])
(core/clear-objects)

;; Load and prepare structure
(def pdb (core/load-pdb "1fap"))
(def protein-atoms (core/filter-polymer pdb))
(def ligand-atoms (core/filter-resname pdb "RAP"))

;; Create materials
(def protein-mat (core/create-basic-material "protein" 
                   {"Base Color" [0.3 0.5 0.8 1.0] "Alpha" 0.9}))
(def ligand-mat (core/create-basic-material "ligand" 
                  {"Base Color" [0.8 0.3 0.2 1.0] "Emission Strength" 1.5}))

;; Visualize
(core/draw! (aget pdb protein-atoms) :cartoon {} protein-mat)
(core/draw! (aget pdb ligand-atoms) :ball+stick 
            {:ball+stick {"Sphere Radii" 0.5}} ligand-mat)

;; Set specific camera view (optional)
(def saved-view [[0.285 0.385 -0.878 -1.771]
                 [-0.958 0.115 -0.261 -0.527]
                 [0.0 0.916 0.402 0.810]
                 [0.0 0.0 0.0 1.0]])
(core/set-view! saved-view)
```

## Troubleshooting

### If Namespace Loading Fails
```clojure
;; Check if namespaces are available
(all-ns)
;; Should show basimol.core and basimol.styles

;; If not available, check the project paths
(require '[basimol.core :as core] :reload)
```

### Python Interop Fallback
Only use direct Python interop when basimol functions don't cover your needs:
```clojure
;; Import additional Python modules if needed
(python/exec "import numpy as np")

;; Access bpy directly (it's already imported in basimol.core)
(python/eval "bpy.context.scene")
```

## Best Practices

1. **Always start with `(core/clear-objects)`** before creating new visualizations
2. **Use basimol functions first** - they handle Python interop correctly
3. **Create materials before drawing** to ensure proper rendering
4. **Save camera views** with `get-view` for reproducible visualizations
5. **Filter structures efficiently** - combine filters for complex selections

## Default Styles Reference

The `basimol.styles/default-styles` map contains sensible defaults for all visualization styles. Override specific parameters as needed rather than recreating entire style definitions.