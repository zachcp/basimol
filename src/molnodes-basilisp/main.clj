(ns molnodes-basilisp.main
   (:import bpy
            builtins
            mathutils
            pathlib
            databpy
            [molecularnodes.entities.molecule.molecule :as molecule]
            [molecularnodes.download :as download]
            [molecularnodes.blender  :as bl]
            [molecularnodes.blender.nodes  :as bl_nodes]
            [numpy :as np]
            [biotite.structure.io.pdb :as pdb]
            [biotite.structure.io.pdbx :as pdbx]
            [biotite.structure.io :as io]
            [biotite.structure :as struct]
            [biotite.structure.bonds :as bonds]
            [biotite.database.rcsb :as rcsb])
   (:require
    [basilisp.string :as str])


 (defn auto-set-camera []
   #_TODO "auto set the view")


;; (defn- center-array [atom_array]
;;   (if (builtins/isinstance atom_array struct/AtomArrayStack)
;;     (doseq [arr atom_array] (databpy/centre (.-coord arr)))
;;     (databpy/centre (.-coord atom_array))))


 (defn clear-all []
   #_TODO "To fully clear the screen")

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


 (defn get-camera [] (.. bpy/context -scene -camera))


 (defn get-camera-location [] (.. bpy/context -scene -camera -location))


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
         _  (set! (.-bonds stack) (bonds/connect_via_residue_names stack))
         _ (center-array stack)]
     stack))


 (defn render! []
   #_TODO "take a collection of states corresponding to frames and generate an output")


 (defn set-view! [matrix]
   (let [ctx (get-temp-context-info)
         area3d (:area ctx)
         camera (.. bpy -context -scene -camera)
         region3d (.. area3d -spaces -active -region_3d)]
     (when (and area3d region3d camera)
       (set! (.. bpy -context -scene -camera -matrix_world) matrix)
       (set! (.-view_matrix region3d) (.inverted matrix)))))


 (comment
  ;; move the view around and get the view
  ;; the move around and set it back
   (def mat01 (get-view))
   mat01
   (set-view! mat01)

   (defn load-pdb [code]
     ^struct/AtomArrayStack
     (let [stack (-> code download/download pdbx.CIFFile/read pdbx/get_structure)
           _  (set! (.-bonds stack) (bonds/connect_via_residue_names stack))
           stack2 (center-array stack)]
       stack2))
  
  ;;
  ;;
  ;;  (let [mat02 (mathutil/Matrix [[-0.26504477858543396, 0.3475402295589447, 0.8994258046150208, 0.3516474962234497],
  ;;                                [0.9642360210418701, 0.09552931040525436, 0.2472304254770279, 0.09665936231613159],
  ;;                                [9.8351881661074e-07, 0.9327859878540039, -0.36043038964271545, -0.14091697335243225],
  ;;                                [0.0, 0.0, 0.0, 1.0]])]
  ;;    mat02)
  ;;
  ;;  (matrix->str mat01)
  ;;  (mathutils/Matrix [[1 2] [2 2]])
  ;;

   (defn paint-struct [mol]
     (let [arr (.get_array mol 0)
           [obj _] (molecule/_create_object  arr ** :name "heyyo" :style "Style Cartoon")]
       (bl_nodes/create_starting_node_tree obj ** :style "cartoon")))


 (defn- center-array [atom-array]
   (if (builtins/isinstance atom-array struct/AtomArrayStack)
     (doseq [arr atom-array]
       (set! (.-coord arr)
             (np/subtract (.-coord arr)
                          (databpy/centre (.-coord arr)))))
     (set! (.-coord atom-array)
           (np/subtract (.-coord atom-array)
                        (databpy/centre (.-coord atom-array))))))

 ;; load a file
 ;;  (def fap (load-pdb "1FAP"))
;;  (center-array fap)
;;  (= (.-coord fap) (.-coord fap))
;;  (= (.-coord fap) (.-coord (center-array fap)))
;;  (paint-struct fap)
;;  (paint-struct (center-array fap))
;;  (paint-struct (center-array (.get_array fap 0)))
;;  (.get_array fap 0)
;;  (databpy/centre  (.-coord (.get_array fap 0)))
;;  (.-coord (.get_array fap 0))
;;  (center-array (.get_array fap 0))
;;  (.get_array fap)
;;  (clear-objects)
 (defn filter-atom-name [stack, atomname])
 (defn render! [stack])
 
 )