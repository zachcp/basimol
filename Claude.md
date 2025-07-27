# Claude Guide: Working with Basilisp in Blender

## Overview
This guide explains how to work with the basimol project, a Clojure-based molecular visualization tool running in Basilisp (Python implementation of Clojure) within Blender.

## Environment Details
- **Language**: Basilisp (Python-based Clojure implementation)
- **Host**: Blender 4.5+ 
- **Project**: basimol - molecular visualization in Blender
- **REPL**: Connected via nREPL server running in Blender

## Key Differences from Standard Clojure

### 1. Python Interop Instead of Java
- Use `python/exec` and `python/eval` for Python code execution
- No `System/getProperty` or Java-specific functions
- Python modules accessed through special syntax

### 2. Namespace Loading Challenges
- Standard `require` may fail due to missing Python path setup
- The basimol namespace appears in `(all-ns)` but isn't directly requireable
- Dependencies like `basimol.styles` and `basimol.materials` may not resolve

### 3. Blender API Access
- Blender Python API (`bpy`) must be explicitly imported and made available
- Use `python/exec "import bpy; globals()['bpy'] = bpy"` to make bpy accessible
- Keyword arguments in Python calls need special handling

## Getting Started

### 1. Verify REPL Connection
```clojure
;; Test basic evaluation
(+ 1 2 3)
;; => 6

;; Check current namespace
*ns*
;; => user

;; List available namespaces
(all-ns)
;; Should show basilisp-blender.* namespaces
```

### 2. Import Blender API
```clojure
;; Import and make bpy available globally
(python/exec "
import bpy
globals()['bpy'] = bpy
")

;; Verify bpy is accessible
(python/eval "bpy")
;; => <module 'bpy' from '...'>
```

### 3. Load Basilisp-Blender Utilities
```clojure
;; Load available Blender utilities
(require 'basilisp-blender.bpy-utils)

;; Check what's available
(ns-publics 'basilisp-blender.bpy-utils)
```

## Working with Basimol Functions

### Project Structure
```
/Users/zcpowers/Documents/Projects/basimol/
├── src/basimol/
│   ├── core.lpy       # Main functions including clear-objects
│   ├── styles.lpy     # Styling utilities
│   └── materials.lpy  # Material definitions
├── deps.edn          # Dependencies (Clojure format)
└── examples/         # Example scripts
```

### Defining Functions Manually
Since direct namespace loading may fail, manually define functions from source:

```clojure
;; Example: clear-objects function
(defn clear-objects []
  (let [bpy (python/eval "bpy")
        mol-collection (.. bpy -data -collections (get "Molecular Nodes"))]
    (when mol-collection
      (doseq [obj (.. mol-collection -objects)]
        (python/exec (str "bpy.data.objects.remove(" obj ", do_unlink=True)"))))
    (doseq [obj (.. bpy -data -objects)]
      (when (and (= "MESH" (.-type obj)) (not= "Camera" (.-name obj)))
        (python/exec (str "bpy.data.objects.remove(" obj ", do_unlink=True)"))))))
```

### Python Interop Patterns

#### Executing Python Code
```clojure
;; Simple execution
(python/exec "import some_module")

;; Multi-line execution
(python/exec "
import bpy
import bmesh
result = some_calculation()
")
```

#### Accessing Python Objects
```clojure
;; Get Python object
(python/eval "bpy.context.scene")

;; Access attributes using Clojure syntax
(.. bpy -data -objects)

;; Call methods with arguments
(.. bpy -data -objects (get "Cube"))
```

#### Handling Keyword Arguments
Python functions with keyword arguments need special handling:
```clojure
;; Wrong - will cause "required parameter to be a keyword argument" error
(.. bpy -data -objects (remove obj :do_unlink true))

;; Right - use python/exec with string interpolation
(python/exec (str "bpy.data.objects.remove(" obj ", do_unlink=True)"))
```

## Common Patterns

### 1. Exploring Available Functions
```clojure
;; List all namespaces
(all-ns)

;; Check what's in a namespace
(ns-publics 'some-namespace)

;; Use MCP tools to read source files
;; (via clojure-mcp:read_file with pattern matching)
```

### 2. Working with Blender Collections
```clojure
;; Get a collection
(let [collection (.. bpy -data -collections (get "Collection Name"))]
  (when collection
    ;; Work with objects in collection
    (doseq [obj (.. collection -objects)]
      ;; Do something with obj
      )))
```

### 3. Filtering Blender Objects
```clojure
;; Filter objects by type
(doseq [obj (.. bpy -data -objects)]
  (when (= "MESH" (.-type obj))
    ;; Process mesh objects
    ))

;; Filter by name pattern
(doseq [obj (.. bpy -data -objects)]
  (when (clojure.string/includes? (.-name obj) "pattern")
    ;; Process matching objects
    ))
```

## Troubleshooting

### Common Issues

1. **ImportError: Basilisp namespace 'basimol.core' not found**
   - Solution: Define functions manually or fix Python path

2. **TypeError: required parameter to be a keyword argument**
   - Solution: Use `python/exec` with proper Python syntax for keyword args

3. **NameError: name 'bpy' is not defined**
   - Solution: Import bpy and add to globals as shown above

4. **AttributeError on Python objects**
   - Solution: Check object exists and use proper attribute access syntax

### Debugging Tips

```clojure
;; Check if object exists
(when-let [obj (.. bpy -data -objects (get "ObjectName"))]
  ;; Object exists, safe to use
  )

;; Print Python object details
(python/exec "print(dir(some_object))")

;; Use try-catch for error handling
(try
  (some-operation)
  (catch Exception e
    (println "Error:" (str e))))
```

## Best Practices

1. **Always check object existence** before accessing Blender objects
2. **Use python/exec for complex Python operations** rather than trying to translate everything to Clojure syntax
3. **Import required Python modules explicitly** at the start of your session
4. **Define utility functions** to wrap common Blender operations
5. **Use MCP tools** to explore source files and understand function definitions before implementing

## Example Session Flow

```clojure
;; 1. Basic setup
(python/exec "import bpy; globals()['bpy'] = bpy")
(require 'basilisp-blender.bpy-utils)

;; 2. Define utility functions
(defn clear-objects [] 
  ;; Implementation as shown above
  )

;; 3. Use functions
(clear-objects)

;; 4. Verify results
(python/exec "print(len(bpy.data.objects))")
```

This guide should help future LLMs quickly understand the basimol/Basilisp environment and start working effectively with the molecular visualization tools.