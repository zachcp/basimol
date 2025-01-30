(ns molnodes-basilisp.main
   (:import bpy mathutils pathlib
            [bpy.types :as types]
            [bpy.props :as props]
            [molecularnodes.entities.molecule.molecule :as molecule]
            [molecularnodes.blender  :as bl]
            [molecularnodes.blender.nodes  :as bl_nodes]
            [biotite.structure.io.pdb :as pdb]
            [biotite.structure.io :as io]
            [biotite.structure.bonds :as bonds]
            [biotite.database.rcsb :as rcsb])
   (:require
    [basilisp.string :as string]))

 (def molecular-node-types
  ; from: for groups in bpy.data.node_groups: print(groups.name)
   {:atoms-to-curves "atoms_to_curves"
    :backbone-nh "Backbone NH"
    :base-align ".Base align"
    :between-float "Between Float"
    :boolean-run-fill "Boolean Run Fill"
    :boolean-run-mask "Boolean Run Mask"
    :bs-smooth ".bs_smooth"
    :cartoon-arrow-instance ".cartoon_arrow_instance"
    :cartoon-arrow-primitive ".cartoon.arrow_primitive"
    :cartoon-arrows-scale ".cartoon_arrows_scale"
    :color-attribute-random "Color Attribute Random"
    :color-common "Color Common"
    :curve-custom-profile ".curve_custom_profile"
    :curve-end-fix-color ".curve_end_fix_color"
    :curve-ends-adjust-angle ".curve_ends_adjust_angle"
    :curve-ends-adjust-position "curve_ends_adjust_position"
    :curve-profile-backup ".curve_profile_backup"
    :curve-to-mesh ".curve_to_mesh"
    :debug-arrows ".DEBUG_arrows"
    :dihedral-angle "Dihedral Angle"
    :dssp-sheet-checks ".DSSP Sheet Checks"
    :expand-selection ".expand_selection"
    :fallback-boolean "Fallback Boolean"
    :field-offset ".field_offset"
    :field-offset-bool ".field_offset_bool"
    :field-offset-vec ".field_offset_vec"
    :group-pick "Group Pick"
    :group-pick-vector "Group Pick Vector"
    :guide-rotation ".guide_rotation"
    :hbond-backbone-check "HBond Backbone Check"
    :hbond-backbone-check-backup "HBond Backbone Check_backup"
    :hbond-energy "HBond Energy"
    :hbond-i-1-j+1-and-hbond-j-1-i+1 ".HBond (i - 1, j + 1) and HBond (j - 1, i + 1)"
    :hbond-i-1-j-and-hbond-j-i+1 ".Hbond (i - 1,j) and Hbond (j,i + 1)"
    :hbond-i-j-and-hbond-j-i ".HBond (i, j) and HBond (j, i)"
    :hbond-j-1-i-and-hbond-i-j+1 ".Hbond (j - 1,i) and Hbond (i,j + 1)"
    :helix-detect "Helix Detect"
    :is-alpha-carbon "Is Alpha Carbon"
    :is-helix "Is Helix"
    :is-loop "Is Loop"
    :is-nucleic "Is Nucleic"
    :is-odd ".is_odd"
    :is-peptide "Is Peptide"
    :is-sheet "Is Sheet"
    :mn-assembly-instance-chains ".MN_assembly_instance_chains"
    :mn-cartoon-bs-alternate-axis ".MN_cartoon_bs_alternate_axis"
    :mn-cartoon-smooth-handles ".MN_cartoon_smooth_handles"
    :mn-color-input "MN Color Input"
    :mn-constants-atom-name-nucleic ".MN_constants_atom_name_nucleic"
    :mn-constants-atom-name-peptide ".MN_constants_atom_name_peptide"
    :mn-dna-double-helix "MN_dna_double_helix"
    :mn-fresnel "MN Fresnel"
    :mn-mask-transparent ".MN_mask_transparent"
    :mn-mask-transparent001 ".MN_mask_transparent.001"
    :mn-select-nucleic ".MN_select_nucleic"
    :mn-select-nucleic-type "MN_select_nucleic_type"
    :mn-select-peptide ".MN_select_peptide"
    :mn-select-sec-struct ".MN_select_sec_struct"
    :mn-select-sec-struct-id ".MN_select_sec_struct_id"
    :mn-topo-assign-backbone ".MN_topo_assign_backbone"
    :mn-topo-backbone "MN_topo_backbone"
    :mn-topo-calc-helix ".MN_topo_calc_helix"
    :mn-topo-calc-sheet ".MN_topo_calc_sheet"
    :mn-topo-phi-psi ".MN_topo_phi_psi"
    :mn-units "MN Units"
    :mn-utils-curve-resample "MN_utils_curve_resample"
    :mn-utils-helix "MN_utils_helix"
    :mn-utils-style-cartoon ".MN_utils_style_cartoon"
    :mn-utils-style-ribbon-nucleic ".MN_utils_style_ribbon_nucleic"
    :mn-utils-style-spheres-icosphere ".MN_utils_style_spheres_icosphere"
    :mn-utils-style-spheres-points ".MN_utils_style_spheres_points"
    :mn-world-scale ".MN_world_scale"
    :sample-atom-value ".SampleAtomValue"
    :sec-struct-counter ".sec_struct_counter"
    :selective-scale ".selective_scale"
    :seperate-polymers "Separate Polymers"})

;;    Offset Boolean
;;    Offset Color
;;    Offset Float
;;    Offset Integer
;;    Offset Vector
;;    Point Distance
;;    Point Edge Angle
;;    Points of Edge
;;    Res Group ID
;;    Res Info
;;    Residue Mask
;;    Sample Mix Float
;;    Sample Mix Vector
;;    Sample Nearest Atoms
;;    Select Atomic Number
;;    Select Attribute
;;    Select Bonded
;;    Select Chain_
;;    Select Cube
;;    Select Element
;;    Select Entity_
;;    Select Ligand_
;;    Select Proximity
;;    Select Res ID
;;    Select Res ID Range
;;    Select Res ID_
;;    Select Res Name
;;    Select Res Whole
;;    Select Segment_
;;    Select Sphere
;;    Self Sample Proximity
;;    Separate Atoms
;;    Separate Polymers
;;    Set Color
;;    Starfile Instances
;;    Style Ball and Stick
;;    Style Cartoon
;;    Style Density Surface
;;    Style Density Wire
;;    Style Preset 1
;;    Style Preset 2
;;    Style Preset 3
;;    Style Preset 4
;;    Style Ribbon
;;    Style Spheres
;;    Style Sticks
;;    Style Surface
;;    Select Res ID
;;    Select Res ID Range
;;    Select Res ID_
;;    Select Res Name
;;    Select Res Whole
;;    Select Segment_
;;    Select Sphere
;;    Self Sample Proximity
;;    Separate Atoms
;;    Separate Polymers
;;    Set Color
;;    Starfile Instances
;;    Style Ball and Stick
;;    Style Cartoon
;;    Style Density Surface
;;    Style Density Wire
;;    Style Preset 1
;;    Style Preset 2
;;    Style Preset 3
;;    Style Preset 4
;;    Style Ribbon
;;    Style Spheres
;;    Style Sticks
;;    Style Surface


 (defn- locate-molnodes-file []
   "locate the molecularnodes blenderfile"
   (let [package "molecularnodes"
         prefix "MN_"
         extensions-path (pathlib/Path (bpy.utils/user_resource "EXTENSIONS"))
         blend-files (python/list (.rglob extensions-path (str "**/" package "/**/" prefix "*.blend")))]
     (str (first blend-files))))


 (defn load-node-groups []
   "This will make the MolecularNode nodegroups available for use"
   (let [blendfile (locate-molnodes-file)]
     (with [[data-from data-to] (bpy.data.libraries/load blendfile)]
           (doseq [node (.-node-groups data-from)]
             (.append (.-node-groups data-to) node)))))


 (defn get-current-nodegroups   []
   "set of nodegroups in the workspace"
   (into #{} (for [node (bpy.data.node_groups/values)] (.-name node))))


 (defn load-pdb [code]
   "primary way to load a PDB file. uses MN builtin function. Ultimately uses 
   https://github.com/BradyAJohnston/MolecularNodes/blob/bb35110ebf781021448f6f2294ef300768972650/molecularnodes/entities/molecule/molecule.py#L126 
   and https://github.com/BradyAJohnston/MolecularNodes/blob/bb35110ebf781021448f6f2294ef300768972650/molecularnodes/blender/nodes.py#L521
   "
   (do
     (set! (.. bpy/context -scene -MN_pdb_code) code)
     (set! (.. bpy/context -scene -MN_import_style) "cartoon")
     (set! (.. bpy/context -scene -MN_import_centre) true)
     (bpy.ops.mn/import_wwpdb)))


 (defn- mol-by-name [objname]
   (bpy.data.objects/get objname))


 (defn geom-node-by-name [name]
   ^bpy.types.GeometryNodeTree
   (-> (bpy.data.objects/get name) .-modifiers .-active .-node-group))


 (defn get-camera-location []
   (let [camera (.. bpy/context -scene -camera)
         location (.-location camera)]
     location))


 (defn set-camera! [distance]
  ;; Set camera position
   (let [camera (.. bpy/context -scene -camera)]
     (set! (.-location camera) (new mathutils.Vector [distance 0 0]))

    ;; Calculate rotation to look at origin
     (let [direction (new mathutils.Vector [1 0 0])
           _ (.normalize direction)
           up (new mathutils.Vector [0 0 1])
           rotation (.to_track_quat direction "Z" "Y")]
       (set! (.-rotation_euler camera) (.to_euler rotation)))

    ;; Update the view
     (.. bpy/context -view_layer update)))


 (defn bounding-box-center [obj-name]
   "add the bounding box vectors and average them"
   (let [obj (.. bpy -data -objects (get obj-name))
         bound-box (map #(new mathutils.Vector %) (.-bound_box obj))
         combined_vec (reduce + bound-box)]
     (/ combined_vec 8)))


 (defn center-object-on-origin! [obj-name]
   "center the object  on the middle of the bounding box"
   (let [obj (.. bpy -data -objects (get obj-name))
         objcenter (bounding-box-center obj-name)]
     (set! (.-location obj) (- objcenter))))



 (defn create-subdivided-cube  []
   "minimal example of adding Geometry Nodes to a Mesh"
   (let [_     (.. bpy -ops -mesh (primitive_cube_add ** :size 2 :enter_editmode false :location [0 0 0]))
         cube  (.. bpy -context -active_object)
         modifier (..  cube -modifiers  (new ** :name "GeometryNodes" :type "NODES"))
         node_tree (.. bpy -data -node_groups (new ** :name "SubdivideSetup" :type "GeometryNodeTree"))
         nodes  (.-nodes node_tree)]
          ;; attach the ndoe tree to the cube modifiers
     (set! (.-node_group modifier) node_tree)
          ;; node-level setup 
     (let [input-node (.new nodes "NodeGroupInput")
           output-node (.new nodes "NodeGroupOutput")
           subdivide-node (.new nodes "GeometryNodeSubdivisionSurface")
           links (.-links node_tree)]

       (.. node_tree -interface (new_socket ** :name  "Geometry" :in_out "INPUT" :socket_type "NodeSocketGeometry"))
       (.. node_tree -interface (new_socket ** :name  "Geometry" :in_out "OUTPUT" :socket_type "NodeSocketGeometry"))
       (set! (.. subdivide-node -inputs  (get "Level") -default-value) 2)
       (.. links (new (.. input-node -outputs (get "Geometry")) (.. subdivide-node -inputs (get "Mesh"))))
       (.. links (new (.. subdivide-node -outputs (get "Mesh")) (.. output-node -inputs (get "Geometry"))))
       nil)))

;; Create_object

                ;; mol.create_object (name=file.name,
                ;;    centre=self.centre,
                ;;    style=style,
                ;;    del_solvent=self.del_solvent,
                ;;    build_assembly=self.assembly)

;; https://github.com/BradyAJohnston/MolecularNodes/blob/6cec9f5e06b776d4761076fada84179182d85329/molecularnodes/entities/molecule/ui.py#L70

;;     obj = mol.create_object (name=pdb_code,
;;                          centre=centre,
;;                          style=style,
;;                          del_solvent=del_solvent,
;;                          del_hydrogen=del_hydrogen,
;;                          build_assembly=build_assembly,
;;                          color=color)

;; obj.mn ["pdb_code"] = pdb_code
;; obj.mn ["entity_type"] = format


      ;;  obj, frames = _create_object (array=array,
      ;;                         name=name,
      ;;                         centre=centre,
      ;;                         style=style,
      ;;                         collection=collection,
      ;;                         verbose=verbose)


    ;;  if style:
    ;;         bl.nodes.create_starting_node_tree(
    ;;             object=obj, coll_frames=frames, style=style, color=color
    ;;         )



;; https://github.com/BradyAJohnston/MolecularNodes/blob/6cec9f5e06b776d4761076fada84179182d85329/molecularnodes/entities/molecule/molecule.py#L192
;; use biotite....


 (comment
  ;; the easiest thing to do is to mimic the MN data structure BEFORE it gets sent to blender.
  ;; then I can control EVERY aspect of the rendering. E.g. Which bits get rotated etc.

   (import tempfile)
   (def pdb_struct
     (let [tempdir (tempfile/gettempdir)
           pdbfile (rcsb/fetch "1l2y", "pdb", tempdir)
           pdb_obj (pdb.PDBFile/read pdbfile)
           struct (.get_structure pdb_obj)]
       struct))


   ;; it would be good to paint by color and material as well.
   (defn paint-struct []
     (let [arr (.get_array pdb_struct 1)
           bonds (bonds/connect_via_residue_names arr)
           _ (set! (.-bonds arr) bonds)
           [obj frames] (molecule/_create_object  arr ** :name "heyyo" :style "Style Cartoon")]
       (bl_nodes/create_starting_node_tree obj ** :style "cartoon")))
   )

   ;; we need a get view fn
   
   (paint-struct)
 
      
      ;;    if style:
      ;;        bl.nodes.create_starting_node_tree(
      ;;            object=obj, coll_frames=frames, style=style, color=color
      ;;        )

      ;;    try:
      ;;        obj["entity_ids"] = self.entity_ids
      ;;    except AttributeError:
      ;;        obj["entity_ids"] = None

      ;;    try:
      ;;        obj.mn.biological_assemblies = json.dumps(self.assemblies())
      ;;    except InvalidFileError:
      ;;        obj.mn.biological_assemblies = ""

      ;;    if build_assembly and style:
      ;;        bl.nodes.assembly_insert(obj)

      ;;    # attach the model bpy.Object to the molecule object
      ;;    self.object = obj
      ;;    # same with the collection of bpy Objects for frames
      ;;    self.frames = frames

      ;;    return obj


   (molecule/create_object pdb_struct)


  ; initialize the classes we ned to add additional Geom Nodes
   (load-node-groups)

 ;; Works
   (load-pdb "1FAP")
   (set-camera! 3)
   (center-object-on-origin! "1FAP")


   (load-pdb "7VDV")
   (center-object-on-origin! "7VDV")

   (mol-by-name "1FAP")
   (geom-node-by-name "1FAP")


  ;; Example creation of cube and subnodes
   (create-subdivided-cube)



   (load-pdb "1FAP")
   (def fap (mol-by-name "1FAP"))  ; GeometryNode
   (def exampletree (geom-node-by-name "1FAP")) ; GeometryNodeTree



  ;; #Create a new Group node in the main tree
  ;; group_node = main_tree.nodes.new ("GeometryNodeGroup")
  ;; group_node.node_tree = group

  ;; print out the Molecular Nodes within the Tree.
   (let [exampletree (geom-node-by-name "1FAP")
         nodes (.-nodes exampletree)
         group (bpy.data.node_groups/get "Separate Polymers")
         group-node (.new nodes "GeometryNodeGroup")]
     (set! (.-node_tree group-node) group)

    ;; .MN_constants_atom_name_peptide
     (doseq [node nodes]
       (println node)))


  ;; <bpy_struct, NodeGroupInput("Group Input") at 0x3438e6e58>                <-- Keep
  ;; <bpy_struct, NodeGroupOutput("Group Output") at 0x3438e6b78>              <-- Keep
  ;; <bpy_struct, GeometryNodeGroup("Style Cartoon") at 0x3438e8328>           <-- Keep
  ;; <bpy_struct, GeometryNodeGroup("Set Color") at 0x34905ccd8>               <-- Keep
  ;; <bpy_struct, GeometryNodeGroup("Color Common") at 0x354fb2758>            <-- Keep
  ;; <bpy_struct, GeometryNodeGroup("Color Attribute Random") at 0x34901cdb8>  <-- Keep


 ;; Nodes need to be added to the workspace before they can be used
   (molecular-node-types :mn-dna-double-helix)
   (molecular-node-types :separate-polymers)
   (def blendfile "/Users/zcpowers/Library/Application Support/Blender/4.2/extensions/blender_org/molecularnodes/assets/MN_data_file_4.2.blend")


  ;; todo:
  ;; Function to add node to a NodeGroup
  ;; Function to delete nodes from a NodeGroup
  ;; Function to build up NodegroupCombinations
   )