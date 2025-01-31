(ns molnodes-basilisp.main
  (:import bpy mathutils pathlib
           [bpy.types :as types]
           [bpy.props :as props]
           [molecularnodes.entities.molecule.molecule :as molecule]
           [molecularnodes.download :as download]
           [molecularnodes.blender  :as bl]
           [molecularnodes.blender.nodes  :as bl_nodes]
           [biotite.structure.io.pdb :as pdb]
           [biotite.structure.io.pdbx :as pdbx]
           [biotite.structure.io :as io]
           [biotite.structure :as struct]
           [biotite.structure.bonds :as bonds]
           [biotite.database.rcsb :as rcsb])
  (:require
   [basilisp.string :as string]))



(defn clear-all []
  #_TODO "To fully clear the screen")

(defn auto-set-camera []
  #_TODO "auto set the view")

(defn get-camera [] (.. bpy/context -scene -camera))

(defn get-camera-location [] (.. bpy/context -scene -camera -location))

(defn- get-temp-context-info []
  (let [win (.. bpy -context -window)
        screen (.-screen win)
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
        region (:region ctx)
        camera (.. bpy -context -scene -camera)
        region3d (.. area3d -spaces -active -region_3d)]
    (when (and area3d region3d camera)
      (let [view-matrix (.-view_matrix region3d)
            camera-matrix (.inverted view-matrix)]
        (set! (.-matrix_world camera) camera-matrix)
        (.-matrix_world camera)))))


;; core loading function. used molnodes code + biotite bond connections.
(defn load-pdb [code]
  ^struct/AtomArrayStack
  (let [stack (-> code download/download pdbx.CIFFile/read pdbx/get_structure)
        _  (set! (.-bonds stack) (bonds/connect_via_residue_names stack))]
    stack))


(defn render [])


(defn render! []
   #_TODO "take a collection of states corresponding to frames and generate an output")


(defn set-view! [matrix]
  (let [ctx (get-temp-context-info)
        area3d (:area ctx)
        region (:region ctx)
        camera (.. bpy -context -scene -camera)
        region3d (.. area3d -spaces -active -region_3d)]
    (when (and area3d region3d camera)
      (set! (.. bpy -context -scene -camera -matrix_world) matrix)
      (set! (.-view_matrix region3d) (.inverted matrix)))))
   

(comment  
  ;; move the view around and get the view
  ;; the move around and set it back
  (def mat01 (get-view))
  (set-view! mat01)
  (defn filter-atom-name [stack, atomname]) 
  (defn render! [stack])

  
   ;; it would be good to paint by color and material as well.
   (defn paint-struct [mol]
     (let [arr (.get_array pdb_struct 1)
           bonds (bonds/connect_via_residue_names arr)
           _ (set! (.-bonds arr) bonds)
           [obj frames] (molecule/_create_object  arr ** :name "heyyo" :style "Style Cartoon")]
       (bl_nodes/create_starting_node_tree obj ** :style "cartoon"))))

  ;; ;; it would be good to paint by color and material as well.
  ;; (defn load-mol []
  ;;   (let [arr (.get_array pdb_struct 1)
  ;;         bonds (bonds/connect_via_residue_names arr)
  ;;         _ (set! (.-bonds arr) bonds)
  ;;         [obj frames] (molecule/_create_object  arr ** :name "heyyo" :style "Style Cartoon")]
  ;;     (bl_nodes/create_starting_node_tree obj ** :style "cartoon")))

  ;; we need a get view fn
  (paint-struct)
  (molecule/create_object pdb_struct)
  ; initialize the classes we ned to add additional Geom Nodes
  (load-node-groups)
  (load-pdb "1FAP")
  (load-pdb "7VDV")
  (center-object-on-origin! "7VDV")
  (mol-by-name "1FAP")
  (geom-node-by-name "1FAP")
  ;; Example creation of cube and subnodes
  (create-subdivided-cube)
  )