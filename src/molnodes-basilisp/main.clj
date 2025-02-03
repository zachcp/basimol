(ns molnodes-basilisp.main
  (:import bpy
           builtins
           databpy
           mathutils
           [molecularnodes.entities.molecule.molecule :as molecule]
           [molecularnodes.download :as download]
           [molecularnodes.blender.nodes  :as bl_nodes]
           [numpy :as np]
           [biotite.structure.io.pdbx :as pdbx]
           [biotite.structure.filter :as filter]
           [biotite.structure :as struct]
           [biotite.structure.bonds :as bonds]))



(defn auto-set-camera []
  #_TODO "auto set the view")

(defn- center-array [atom-array]
  (if (builtins/isinstance atom-array struct/AtomArrayStack)
    (doseq [arr atom-array]
      (set! (.-coord arr)
            (np/subtract (.-coord arr)
                         (databpy/centre (.-coord arr)))))
    (set! (.-coord atom-array)
          (np/subtract (.-coord atom-array)
                       (databpy/centre (.-coord atom-array))))))


 ;; Remove Molecules and then the Cube
(defn clear-objects []
  (let [mol-collection (.. bpy -data -collections (get "Molecular Nodes"))]
    (when mol-collection
      (doseq [obj (.. mol-collection -objects)]
        (.. bpy -data -objects (remove obj ** :do_unlink true))))
    (doseq [obj (.. bpy -data -objects)]
      (when (and (= "MESH" (.-type obj))
                 (not= "Camera" (.-name obj)))
        (.. bpy -data -objects (remove obj ** :do_unlink true))))))



(defn filter-atomname [arr atomname]
  (= atomname (.get_annotation arr "atom_name")))


(defn filter-chain [arr chain]
  (= chain (.get_annotation arr "chain_id")))


(defn filter-element [arr element]
  (= element (.get_annotation arr "element")))


(defn filter-hetero [arr]
  (= true (.get_annotation arr "hetero")))

(defn filter-inscode [arr inscode]
  (= inscode (.get_annotation arr "ins_code")))

        
(defn filter-resid [arr num]
  (= num (.get_annotation arr "res_id")))


(defn filter-resname [arr res_name]
  (= res_name (.get_annotation arr "res_name")))


;; (defn get-camera [] (.. bpy/context -scene -camera))
;; (defn get-camera-location [] (.. bpy/context -scene -camera -location))


(defn- get-temp-context-info []
  (let [win (.. bpy -context -window)
        areas3d (let [areas (.. bpy -context -screen -areas)]
                  (-> (filter #(= (.-type %) "VIEW_3D") areas)
                      first))
        region (let [region (.-regions areas3d)]
                 (-> (filter #(= (.-type %) "WINDOW") region)
                     first))]
    {:window win
     :area areas3d
     :region region}))


(defn get-view []
  (let [ctx (get-temp-context-info)
        area3d (:area ctx)
        camera (.. bpy -context -scene -camera)
        region3d (.. area3d -spaces -active -region_3d)]
    (when (and area3d region3d camera)
      (let [view-matrix (.-view_matrix region3d)
            camera-matrix (.inverted view-matrix)]
        (set! (.-matrix_world camera) camera-matrix)
        (let [mat (.-matrix_world camera)]
          (map vec mat))))))


;; core loading function. used molnodes code + biotite bond connections.
(defn load-pdb [code]
  ^struct/AtomArrayStack
  (let [stack (-> code download/download pdbx.CIFFile/read pdbx/get_structure)
        _  (set! (.-bonds stack) (bonds/connect_via_residue_names stack))]
    (center-array stack)))


(defn render! []
  #_TODO "take a collection of states corresponding to frames and generate an output")


(defn set-view! [matrix]
  (let [mmat (mathutils/Matrix (vec matrix))
        ctx (get-temp-context-info)
        area3d (:area ctx)
        camera (.. bpy -context -scene -camera)
        region3d (.. area3d -spaces -active -region_3d)]
    (when (and area3d region3d camera)
      (set! (.. bpy -context -scene -camera -matrix_world) mmat)
      (set! (.-view_matrix region3d) (.inverted mmat)))))





(comment


  (defn create-material
    "Creates a basic material with the given name and properties
   props is a map that can include:
   :color [r g b a]
   :metallic float
   :roughness float"
    [name {:keys [color metallic roughness]
           :or {color [0.8 0.8 0.8 1.0]
                metallic 0.0
                roughness 0.5}}]
    (let [material (.. bpy -data -materials (new "test"))
          _ (set! (. material use_nodes) true)
          nodes (.. material -node_tree -nodes)
          principled (. nodes (get "Principled BSDF"))]
      (set! (.. principled -inputs ["Base Color"] -default_value) color)
      (set! (.. principled -inputs ["Metallic"] -default_value) metallic)
      (set! (.. principled -inputs ["Roughness"] -default_value) roughness)
      material))

 ;; move the view around and get the view
 ;; the move around and set it back
  (def mat01 (get-view))

  (set-view! (get-view))

  (defn paint-struct [mol]
    (let [arr (.get_array mol 0)
          [obj _] (molecule/_create_object  arr ** :name "heyyo" :style "Style Cartoon")]
      (bl_nodes/create_starting_node_tree obj ** :style "cartoon")))


 ;; load a file
  (def fap (load-pdb "1FAP"))
  (.get_annotation fap "res_id")
  (.get_annotation fap "res_name")
  (.get_annotation fap "res_name")

  ;; works on stack and on Array
  (filter/filter_amino_acids fap)
  (filter/filter_amino_acids (first (seq fap)))

;; styles_mapping = {
;;     "preset_1": "Style Preset 1",
;;     "preset_2": "Style Preset 2",
;;     "preset_3": "Style Preset 3",
;;     "preset_4": "Style Preset 4",
;;     "atoms": "Style Spheres",
;;     "spheres": "Style Spheres",
;;     "vdw": "Style Spheres",
;;     "sphere": "Style Spheres",
;;     "cartoon": "Style Cartoon",
;;     "sticks": "Style Sticks",
;;     "ribbon": "Style Ribbon",
;;     "surface": "Style Surface",
;;     "ball_and_stick": "Style Ball and Stick",
;;     "ball+stick": "Style Ball and Stick",
;;     "oxdna": "MN_oxdna_style_ribbon",
;;     "density_surface": "Style Density Surface",
;;     "density_wire": "Style Density Wire",
;; }
  (let [style "surface"
        molname (str (gensym))
        [obj _] (molecule/_create_object  fap ** :name molname :style style)]
    (bl_nodes/create_starting_node_tree obj ** :style style))


  (let [style "sticks"
        molname (str (gensym))
        filtered-structure (aget  fap 0 (filter-resid  fap 1))
        [obj _] (molecule/_create_object  filtered-structure ** :name molname :style style)]
    (bl_nodes/create_starting_node_tree obj ** :style style))
  (clear-objects)
;;  (defn filter-atom-name [stack, atomname])
;;  (defn render! [stack])
  )

   