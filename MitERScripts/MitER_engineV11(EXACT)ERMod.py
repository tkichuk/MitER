import bpy
import bmesh
import csv
import os


from bpy.props import StringProperty, BoolProperty
from bpy_extras.io_utils import ImportHelper
from bpy.types import Operator
from os import listdir
from os.path import isfile, join
from mathutils import Vector
from object_print3d_utils import mesh_helpers
from object_print3d_utils import operators

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# // NAME OF FUNCTION: GetVolumeAndDimensions 
# //
# // PARAMETERS: mesh_object_list, BBList, vol_list
# //
# // WHAT THE FUNCTION DOES: It iterates through each object in the X3D file, corrects non-manifold
# // geometry, measures volume and bounding box dimensions, and outputs volume and dimensions into 
# // two sorted lists
# //
# // DEPENDENCIES: none apart from the .X3D file
# //
# // THE CODE: 

def GetVolumeAndDimensions (mesh_object_list, BBList, vol_list): 

    for o in mesh_object_list:

        H = bpy.data.objects[o.name].dimensions.x
        L = bpy.data.objects[o.name].dimensions.y
        W = bpy.data.objects[o.name].dimensions.z
        
        #Vol of bounding box
        BBVolume = H * L * W
        BBList.append(BBVolume)

        
        me = o.data
        ob = bpy.context.scene.objects [o.name]    
        bpy.ops.object.select_all (action='DESELECT')
        bpy.context.view_layer.objects.active = ob  
        bpy.ops.object.editmode_toggle()
        bpy.ops.mesh.print3d_clean_non_manifold()
        bpy.ops.object.editmode_toggle()
        
       
        # Get a BMesh representation
        bm = bmesh.new()
        bm.from_mesh(me)
        
        bm = mesh_helpers.bmesh_copy_from_object(o, apply_modifiers=True)
        bmesh.ops.triangulate(bm, faces=bm.faces[:])
        
        
        volume = bm.calc_volume()
        area = sum(f.calc_area() for f in bm.faces)
        
        
        # Determining blender vertices
        bbverts = [o.matrix_world @ Vector(bbvert) for bbvert in o.bound_box]
        
        # Possible backwards comptability for Blender v < 2.8
        #bbverts = [o.matrix_world * Vector(bbvert) for bbvert in o.bound_box]
        
        plain_verts = [vert.to_tuple() for vert in bbverts]
        
        
        me.update()
        bm.clear()
        
        vol_list.append(volume)
        
        # Progress Update Print Statements
        print (o.name)
        print (volume)
        print (BBVolume)
        
        # Sorting by Volume of Mesh
        Input  = vol_list
        Output = sorted (Input, key = lambda x:float(x))
        
        # Sorting by voume of bounding box
        Input = BBList
        SortedBBList = sorted (Input, key = lambda x:float(x))

    return Output, SortedBBList

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### start of bbox manipulators 
# define a set of functions to play with bound box 

def bbox(ob):
    return (Vector(b) for b in ob.bound_box)

# Determining center of object
def bbox_center(ob):
    return sum(bbox(ob), Vector()) / 8

# Determines the axes of bounding box
# Returns position vector, and x, y, z coordinates nearest to position vector
def bbox_axes(ob):
    bb = list(bbox(ob))
    return tuple(bb[i] for i in (0, 4, 3, 1))

### end of bbox manipulators 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# // NAME OF FUNCTION: IdentifyAndNameCell
# //
# // PARAMETERS: mesh_obs, output
# //
# // WHAT THE FUNCTION DOES: It iterates through each object in the X3D file, determines the volume
# // of each and assigns the object with the largest volume the name of "Cell"
# // 
# //
# // DEPENDENCIES: none apart from the .X3D file
# //
# // THE CODE: 

def IdentifyAndNameCell (mesh_obs, output):

    for o in mesh_obs:
        # recomputing bounding boxes
        me = o.data
        
        # Get a BMesh representation
        bm = bmesh.new()
        bm.from_mesh(me)
        bm = mesh_helpers.bmesh_copy_from_object(o, apply_modifiers=True)
        bmesh.ops.triangulate(bm, faces=bm.faces[:])
        volume = bm.calc_volume()
        

        # Output list is sorted Cell Volumes
        # If the volume of mesh object is equal to the LARGEST mesh volume, IT IS A CELL!

        if volume == output [-1]:
            ob = bpy.context.scene.objects [o.name]    
            bpy.ops.object.select_all (action='DESELECT')
            bpy.context.view_layer.objects.active = ob  
            ob.select_set (True) 
            ob.name = 'Cell'
            # And on the 8th day, god created cells
            print ('Cell was created')            


    return mesh_obs 

## end of blender scene context maker 

### start of another BlenderCellBoxDefiner 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# // NAME OF FUNCTION: FirstBoolForLabelMaker
# //
# // PARAMETERS: mesh_ob, volume
# //
# // WHAT THE FUNCTION DOES: For the identified input mesh, this function splits it and if there 
# // are fewer than 200 individual components, the mesh is labeled "ER"
# // 
# //
# // DEPENDENCIES: none apart from the .X3D file
# //
# // THE CODE: 

def FirstBoolForLabelMaker  (mesh_ob, volume):

    # progress update
    print ("Volume: ", volume)

    # do stuff with ob 

    ob = bpy.context.scene.objects[mesh_ob.name]
    ob.select_set(True)
   
    
    # do stuff with objectToSelect 
    bpy.ops.object.select_all (action='DESELECT')
    objectToSelect = bpy.data.objects[mesh_ob.name]
    objectToSelect.select_set (True)
    bpy.context.view_layer.objects.active = objectToSelect
    bpy.ops.mesh.separate(type='LOOSE')


    # create the objs object 

    objs = [obj for obj in bpy.context.selected_objects]
    n = len(objs)
    if n <= 200:
        ob.name = 'ER'
    
    for o in objs:
        bpy.ops.object.join ()                
        bpy.ops.object.select_all (action='DESELECT')
        bpy.context.view_layer.objects.active = ob  
    

    return mesh_ob  

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# // NAME OF FUNCTION: SecondBoolForLabelMaker
# //
# // PARAMETERS: mesh_ob, other_out, volume
# //
# // WHAT THE FUNCTION DOES: For the identified input mesh, this function splits it and if there 
# // are fewer than 200 individual components, the mesh is labeled "Mitochondria" and the volume
# // of each unconnected mitochondrial component is outputed
# //
# // DEPENDENCIES: none apart from the .X3D file
# //
# // THE CODE: 


def SecondBoolForLabelMaker (mesh_ob, other_out, volume):

    MitoOut = [] 

    # Progress update print statement 
    print ("Volume: ", volume)

    ob = bpy.context.scene.objects[mesh_ob.name]
    ob.select_set(True)
    #ob.name = 'Copy'
    bpy.ops.object.select_all(action='DESELECT')
    objectToSelect = bpy.data.objects[mesh_ob.name]
    objectToSelect.select_set(True)    
    bpy.context.view_layer.objects.active = objectToSelect
    bpy.ops.mesh.separate(type='LOOSE')
    n = 0

    objs = [obj for obj in bpy.context.selected_objects]
    for o in objs:
        # Count number of mitochondrial fragments
        # Add volume of mitochondrial fragement to list
        me = o.data
        # Get a BMesh representation
        bm = bmesh.new()
        bm.from_mesh(me)
        bm = mesh_helpers.bmesh_copy_from_object(o, apply_modifiers=True)
        bmesh.ops.triangulate(bm, faces=bm.faces[:])
        volume = bm.calc_volume()
        MitoOut.append(volume)
        me.update()
        bm.clear()
        n=n+1
        
    # Progress Update Print Statements
    print (n)
    print (MitoOut)
    
    # Rejoining loose components
    for o in objs:
        bpy.ops.object.join()                
        bpy.ops.object.select_all(action='DESELECT')
        bpy.context.view_layer.objects.active = ob  
        #ob.select_set(True) 
    if n <= 100:
        ob.name = 'Mitochondria'
        # Append mitochondrial fragment volume list
        other_out.append(MitoOut)

    return mesh_ob, other_out 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# // NAME OF FUNCTION: ThirdBoolForLabelMaker
# //
# // PARAMETERS: mesh_ob, volume
# //
# // WHAT THE FUNCTION DOES: For the identified input mesh (in this case the one with the smallest 
# // bounding box), this function assigns the mesh the name "MitoNodes"
# // 
# //
# // DEPENDENCIES: none apart from the .X3D file
# //
# // THE CODE: 

def ThirdBoolForLabelMaker  (mesh_ob, volume): 

    print ("Volume: ", volume)
    ob = bpy.context.scene.objects[mesh_ob.name]
    bpy.ops.object.select_all(action='DESELECT')
    bpy.context.view_layer.objects.active = ob 
    ob.select_set (True)
    ob.name = "MitoNodes"

    return mesh_ob

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# // NAME OF FUNCTION: LabelMaker
# //
# // PARAMETERS: mesh_obs, SortedBBList, other_out
# //
# // WHAT THE FUNCTION DOES: For an X3D file containing 5 structures, this function labels
# // the following: 
# //     1. Cell Surface
# //     2. ER Surface
# //     3. ER Nodes
# //     4. Mitochondria Surface
# //     5. Mitochondria Nodes
# //
# // DEPENDENCIES: FirstBoolForLabelMaker, SecondBoolForLabelMaker, ThirdBoolForLabelMaker
# //
# // THE CODE: 

def LabelMaker (mesh_obs, SortedBBList, other_out):

    for o in mesh_obs:

        # TO DO: MAYBE MOVE THIS OUT OF THE FOR LOOP?
        # This is the bounding box of the Cell
        H = bpy.data.objects['Cell'].dimensions.x
        L = bpy.data.objects['Cell'].dimensions.y
        W = bpy.data.objects['Cell'].dimensions.z
        
        # Volume of Bounding Box of Cell
        BBCell = H * L * W
        
        # Creating blender mesh of Cell
        ob = bpy.context.scene.objects['Cell']
        me = ob.data
        # Get a BMesh representation
        bm = bmesh.new()
        bm.from_mesh(me)
        bm = mesh_helpers.bmesh_copy_from_object(ob, apply_modifiers=True)
        bmesh.ops.triangulate(bm, faces=bm.faces[:])
       
        # Volume of the Cell
        VolCell = bm.calc_volume()
        bm.free()   
     
       
        # Creating blender mesh of mesh object
        me = o.data
        
        # Get a BMesh representation
        bm = bmesh.new()
        bm.from_mesh(me)
        bm = mesh_helpers.bmesh_copy_from_object(o, apply_modifiers=True)
        bmesh.ops.triangulate(bm, faces=bm.faces[:])
   
        
        # Bounding Box
        H = bpy.data.objects[o.name].dimensions.x
        L = bpy.data.objects[o.name].dimensions.y
        W = bpy.data.objects[o.name].dimensions.z

        # Volume of Bounding Box of mesh
        BBVolume = H * L * W
        
        # Volume of Object
        volume = bm.calc_volume()
        
        # Progress Update Print Statement
        print ('The objects we are looping through are:', mesh_obs)

        if round(BBVolume, 0) == round(BBCell, 0) and volume == VolCell:
            pass

        elif round(BBVolume, 0) == round(BBCell, 0) and volume < VolCell:
            o = FirstBoolForLabelMaker (o, volume)
          
        elif BBVolume == SortedBBList[-1] or BBVolume == SortedBBList[-2] or BBVolume == SortedBBList[-3] or BBVolume == SortedBBList[-4]:
            o, other_out = SecondBoolForLabelMaker (o, other_out, volume)
            
        elif BBVolume == SortedBBList [-5]: 
            o = ThirdBoolForLabelMaker (o, volume)


        #Assign the only unlabeled structure ER Nodes
        #obj = [obj in obj in bpy.context.scene.objects if obj.name.startswith ("Shape_IndexedFaceSet")]
        obj = [elem for elem in bpy.context.scene.objects if elem.name.startswith("Shape_IndexedFaceSet")] 
        if len (obj) == 1:
            ob_elem = obj[0] 
            ob = bpy.context.scene.objects[ob_elem.name]
            bpy.ops.object.select_all(action='DESELECT')
            bpy.context.view_layer.objects.active = ob
            ob.select_set (True) 
            ob.name = "ERNodes"

        obj = [elem for elem in bpy.context.scene.objects if elem.name.startswith ("Copy")]
        if len(obj) == 1:
            ob_elem = obj[0]
            ob = bpy.context.scene.objects[on_elem.name]
            bpy.ops.object.select_all(action='DESELECT')
            bpy.context.view_layer.objects.active = ob
            ob.name = "ERNodes"

        me.update()
        bm.clear() 

    return mesh_obs

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# // NAME OF FUNCTION: ThreeStructureLabelMaker
# //
# // PARAMETERS: mesh_obs, SortedBBList, other_out
# //
# // WHAT THE FUNCTION DOES: For an X3D file containing 3 structures, this function labels
# // the following: 
# //     1. Cell Surface
# //     2. ER Surface
# //     3. Mitochondria Surface
# //
# //
# // DEPENDENCIES: FirstBoolForLabelMaker
# //
# // THE CODE: 

def ThreeStructureLabelMaker (mesh_obs, SortedBBList, other_out):

    for o in mesh_obs:

        # TO DO: MAYBE MOVE THIS OUT OF THE FOR LOOP?
        # This is the bounding box of the Cell
        H = bpy.data.objects['Cell'].dimensions.x
        L = bpy.data.objects['Cell'].dimensions.y
        W = bpy.data.objects['Cell'].dimensions.z
        
        # Volume of Bounding Box of Cell
        BBCell = H * L * W
        
        # Creating blender mesh of Cell
        ob = bpy.context.scene.objects['Cell']
        me = ob.data
        # Get a BMesh representation
        bm = bmesh.new()
        bm.from_mesh(me)
        bm = mesh_helpers.bmesh_copy_from_object(ob, apply_modifiers=True)
        bmesh.ops.triangulate(bm, faces=bm.faces[:])
        VolCell = bm.calc_volume()
        bm.free()   
        #me = ob.data
        #bm = bmesh.new()
        #bm.from_mesh(me)
        #bm.to_mesh(me)
        
        # Volume of the Cell
        
        
        # Creating blender mesh of mesh object
        me = o.data
        # Get a BMesh representation
        bm = bmesh.new()
        bm.from_mesh(me)
        bm = mesh_helpers.bmesh_copy_from_object(o, apply_modifiers=True)
        bmesh.ops.triangulate(bm, faces=bm.faces[:])
        volume = bm.calc_volume()
   
        
        # Bounding Box
        H = bpy.data.objects[o.name].dimensions.x
        L = bpy.data.objects[o.name].dimensions.y
        W = bpy.data.objects[o.name].dimensions.z

        # Volume of Bounding Box of mesh
        BBVolume = H * L * W
        
        # Volume of Object
        volume = bm.calc_volume()
        
        # Progress Update Print Statement
        print ('The objects we are looping through are:', mesh_obs)

        if round(BBVolume, 0) == round(BBCell, 0) and volume == VolCell:
            pass

        elif round(BBVolume, 0) == round(BBCell, 0) and volume < VolCell:
            o = FirstBoolForLabelMaker (o, volume)
          
        #elif BBVolume == SortedBBList[-1] or BBVolume == SortedBBList[-2] or BBVolume == SortedBBList[-3]:
            #o, other_out = SecondBoolForLabelMaker (o, other_out, volume)
            
        #elif BBVolume == SortedBBList [-5]: 
            #o = ThirdBoolForLabelMaker (o, volume)

        #Assign the only unlabeled structure a name. In this case "mitochondria". 
        obj = [elem for elem in bpy.context.scene.objects if elem.name.startswith("Shape_IndexedFaceSet")] 
        if len (obj) == 1:
            ob_elem = obj[0] 
            ob = bpy.context.scene.objects[ob_elem.name]
            bpy.ops.object.select_all(action='DESELECT')
            bpy.context.view_layer.objects.active = ob
            ob.select_set (True) 
            ob.name = "Mitochondria"

        obj = [elem for elem in bpy.context.scene.objects if elem.name.startswith ("Copy")]
        if len(obj) == 1:
            ob_elem = obj[0]
            ob = bpy.context.scene.objects[on_elem.name]
            bpy.ops.object.select_all(action='DESELECT')
            bpy.context.view_layer.objects.active = ob
            ob.name = "Mitochondria"

        me.update()
        bm.clear() 

    return mesh_obs

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# // NAME OF FUNCTION: TwoStructureLabelMaker
# //
# // PARAMETERS: mesh_obs, SortedBBList, other_out
# //
# // WHAT THE FUNCTION DOES: For an X3D file containing 2 structures, this function labels
# // the following: 
# //     1. Cell Surface
# //     2. Subcellular Structure
# //
# //
# // DEPENDENCIES: None
# //
# // THE CODE: 

def TwoStructureLabelMaker (mesh_obs, SortedBBList, other_out):

    for o in mesh_obs:

        # TO DO: MAYBE MOVE THIS OUT OF THE FOR LOOP?
        # This is the bounding box of the Cell
        H = bpy.data.objects['Cell'].dimensions.x
        L = bpy.data.objects['Cell'].dimensions.y
        W = bpy.data.objects['Cell'].dimensions.z
        
        # Volume of Bounding Box of Cell
        BBCell = H * L * W
        
        # Creating blender mesh of Cell
        ob = bpy.context.scene.objects['Cell']
        me = ob.data
        # Get a BMesh representation
        bm = bmesh.new()
        bm.from_mesh(me)
        bm = mesh_helpers.bmesh_copy_from_object(ob, apply_modifiers=True)
        bmesh.ops.triangulate(bm, faces=bm.faces[:])
      
        # Volume of the Cell
        VolCell = bm.calc_volume()
        bm.free()   
      
        
        # Creating blender mesh of mesh object
        me = o.data
        # Get a BMesh representation
        bm = bmesh.new()
        bm.from_mesh(me)
        bm = mesh_helpers.bmesh_copy_from_object(o, apply_modifiers=True)
        bmesh.ops.triangulate(bm, faces=bm.faces[:])
        
        
        # Bounding Box
        H = bpy.data.objects[o.name].dimensions.x
        L = bpy.data.objects[o.name].dimensions.y
        W = bpy.data.objects[o.name].dimensions.z

        # Volume of Bounding Box of mesh
        BBVolume = H * L * W
        
        # Volume of Object
        volume = bm.calc_volume()
        
        # Progress Update Print Statement
        print ('The objects we are looping through are:', mesh_obs)

        if round(BBVolume, 0) == round(BBCell, 0) and volume == VolCell:
            pass

        #elif round(BBVolume, 0) == round(BBCell, 0) and volume < VolCell:
            #o = FirstBoolForLabelMaker (o, volume)
          
        #elif BBVolume == SortedBBList[-1] or BBVolume == SortedBBList[-2]:
            #o, other_out = SecondBoolForLabelMaker (o, other_out, volume)
            
        #elif BBVolume == SortedBBList [-5]: 
            #o = ThirdBoolForLabelMaker (o, volume)

        #Assign the only unlabeled structure a name - in this case "SubcellularStructure"
        #obj = [obj in obj in bpy.context.scene.objects if obj.name.startswith ("Shape_IndexedFaceSet")]
        obj = [elem for elem in bpy.context.scene.objects if elem.name.startswith("Shape_IndexedFaceSet")] 
        if len (obj) == 1:
            ob_elem = obj[0] 
            ob = bpy.context.scene.objects[ob_elem.name]
            bpy.ops.object.select_all(action='DESELECT')
            bpy.context.view_layer.objects.active = ob
            ob.select_set (True) 
            ob.name = "SubcellularStructure"

        obj = [elem for elem in bpy.context.scene.objects if elem.name.startswith ("Copy")]
        if len(obj) == 1:
            ob_elem = obj[0]
            ob = bpy.context.scene.objects[ob_elem.name]
            bpy.ops.object.select_all(action='DESELECT')
            bpy.context.view_layer.objects.active = ob
            ob.name = "SubcellularStructure"
            
    
    #For the subcellular structure, output a list of the volumes of each separate component.
    mesh_ob = bpy.context.scene.objects["SubcellularStructure"]
    ob.select_set(True)
    #ob.name = 'Copy'
    bpy.ops.object.select_all(action='DESELECT')
    objectToSelect = bpy.data.objects[mesh_ob.name]
    objectToSelect.select_set(True)    
    bpy.context.view_layer.objects.active = objectToSelect
    bpy.ops.mesh.separate(type='LOOSE')
    n = 0

    objs = [obj for obj in bpy.context.scene.objects if obj.name.startswith(mesh_ob.name)]
    print (objs)
    for o in objs:
        # Count number of mitochondrial fragments
        # Add volume of mitochondrial fragement to list
        me = o.data
        # Get a BMesh representation
        bm = bmesh.new()
        bm.from_mesh(me)
        bm = mesh_helpers.bmesh_copy_from_object(o, apply_modifiers=True)
        bmesh.ops.triangulate(bm, faces=bm.faces[:])
        volume = bm.calc_volume()
        print (volume)
        other_out.append(volume)
        print (other_out)
        me.update()
        bm.clear()
        n=n+1

        me.update()
        bm.clear() 
            
    for o in objs:
        bpy.ops.object.join()                
        bpy.ops.object.select_all(action='DESELECT')
        bpy.context.view_layer.objects.active = ob  
       

    return mesh_obs, other_out

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


################

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# // NAME OF FUNCTION: CSV_Writer
# //
# // PARAMETERS: Desktop, SmallFileName, out_list, OtherOut, DistData, CellDistData, ContactDistData, OverlapList 
# //
# // WHAT THE FUNCTION DOES: Exports CSV files of all the data
# //
# //
# // DEPENDENCIES: None
# //
# // THE CODE: 


def CSV_Writer (Desktop, SmallFileName, out_list, OtherOut, DistData, CellDistData, ContactDistData, OverlapList ):
    path = Desktop+'/MitER/Data/'
    # Check whether the specified path exists or not
    isExist = os.path.exists(path)
    if not isExist:
        # Create a new directory because it does not exist
        os.makedirs(path)
    
    file = open(Desktop+'/MitER/Data/'+SmallFileName +'-CellStats.csv', 'w', newline='')
    with file:
        write = csv.writer(file)
        write.writerows([[output] for output in out_list])
    file = open(Desktop+'/MitER/Data/'+SmallFileName +'-MitoComponents.csv', 'w', newline='')
    with file:
        write = csv.writer(file)
        write.writerows([[output] for output in OtherOut])
    file = open(Desktop+'/MitER/Data/'+SmallFileName +'-ERDistribution.csv', 'w', newline='')
    with file:
        write = csv.writer(file)
        write.writerows([[output] for output in DistData])
    file = open(Desktop+'/MitER/Data/'+SmallFileName +'-ContactDistribution.csv', 'w', newline='')
    with file:
        write = csv.writer(file)
        write.writerows([[output] for output in CellDistData])
    file = open(Desktop+'/MitER/Data/'+SmallFileName +'-ERAreaDistribution.csv', 'w', newline='')
    with file:
        write = csv.writer(file)
        write.writerows([[output] for output in ContactDistData])
    file = open(Desktop+'/MitER/Data/'+SmallFileName +'-Overlap.csv', 'w', newline='')
    with file:
        write = csv.writer(file)
        write.writerows([[output] for output in OverlapList])

    return 0 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# // NAME OF FUNCTION: update_out_list
# //
# // PARAMETERS: out_list, bpycontext
# //
# // WHAT THE FUNCTION DOES: For each structure, this function measures the bounding box
# // dimensions, the center of volume of each object, the surface area, the volume,
# // and the vertices
# //
# // DEPENDENCIES: None
# //
# // THE CODE: 

def update_out_list (out_list, bpycontext):
    meshes = [o for o in bpycontext.scene.objects if o.type == "MESH"]
    print (meshes)
    for o in meshes:
        # get bounding box dimensions 
        H = bpy.data.objects[o.name].dimensions.x
        L = bpy.data.objects[o.name].dimensions.y
        W = bpy.data.objects[o.name].dimensions.z 

        ob = bpy.context.scene.objects[o.name]
        
        # Add a Voxel Remesh modifier to the object
        remesh_modifier = ob.modifiers.new(name='Remesh', type='REMESH')
        remesh_modifier.mode = 'VOXEL'
        remesh_modifier.voxel_size = 0.03
        
        # Apply the Voxel Remesh modifier
        bpy.ops.object.modifier_apply(modifier=remesh_modifier.name)
        
        for modifier in o.modifiers:
            bpy.context.view_layer.objects.active = o
            bpy.ops.object.modifier_apply(modifier=modifier.name)
               
        bpy.ops.object.select_all(action="DESELECT")
        bpy.context.view_layer.objects.active = ob 
        ob.select_set (True)
        bpy.ops.object.origin_set(type="ORIGIN_CENTER_OF_VOLUME")
        print (ob.location) 
        location = ob.location 
        
        me = o.data
        # Get a BMesh representation
        bm = bmesh.new()
        bm.from_mesh(me)
        bm = mesh_helpers.bmesh_copy_from_object(o, apply_modifiers=True)
        bmesh.ops.triangulate(bm, faces=bm.faces[:])
        volume = bm.calc_volume()
        area = sum(f.calc_area() for f in bm.faces)

        print(area)
        print (volume)
   
        # determining blender vertices 
        bbverts = [o.matrix_world @ Vector(bbvert) for bbvert in o.bound_box]

        plain_verts = [vert.to_tuple() for vert in bbverts]

        # appending info 
        out_list.append (o.name)
        out_list.append (area)
        out_list.append (volume)
        out_list.append (H)
        out_list.append (L)
        out_list.append (W)
        out_list.append (location)
        out_list.append (plain_verts)
        
        me.update() 
        bm.clear()

    return out_list


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# // NAME OF FUNCTION: update_names_t1
# //
# // PARAMETERS: collated_cubes, mitochondria_object, b_objects, dist_data
# //
# // WHAT THE FUNCTION DOES: This function determines the intersection between the cube 
# // subdivisions and the mitochondria to dertermine mitochondrial distribution
# // 
# //
# // DEPENDENCIES: triangulate_object_t1
# //
# // THE CODE: 

def update_names_t1 (collated_cubes, mitochondria_object, b_objects, dist_data):
    # Naming everything 
    # Getting vertices from the cube 
    for o in collated_cubes:
        CubeName    = o.name 
        #Cube        = b_objects[o.name]
        bbverts     = [o.matrix_world @ Vector(bbvert) for bbvert in o.bound_box]
        plain_verts = [vert.to_tuple() for vert in bbverts]
        #bewl        = Cube.modifiers.new (type="BOOLEAN", name="MitoDist")
        #bewl.object = mitochondria_object

        # Deselect all objects
        bpy.ops.object.select_all(action='DESELECT')
        
        # Select the object by name
        o.select_set(True)

        # Set the object as the active object
        bpy.context.view_layer.objects.active = o

        # Set the names of the two objects
        obj_name_1 = o.name
        obj_name_2 = "ER"

        # Get the two objects by name
        obj1 = bpy.data.objects[obj_name_1]
        obj2 = bpy.data.objects[obj_name_2]

        # Add a boolean modifier to obj1
        mod = obj1.modifiers.new(name="Interesect", type="BOOLEAN")
        mod.object = obj2
        mod.operation = 'INTERSECT'
        mod.solver = 'EXACT'
        # Enable hole tolerance
        mod.use_hole_tolerant = True

        
        # create triangulated mesh
        triangulate_object_t1 (b_objects[o.name], dist_data, plain_verts) 

        # remove the cube 
        bpy.data.objects.remove (bpy.context.scene.objects[CubeName], do_unlink=True)
        
    return 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# // NAME OF FUNCTION: triangulate_object_t1
# //
# // PARAMETERS: obj, dist_data, plain_verts
# //
# // WHAT THE FUNCTION DOES: This function outputs
# // the volume and surface area of each intersection along with the vertices of the 
# // segmentation box.
# //
# // DEPENDENCIES: none
# //
# // THE CODE: 

def triangulate_object_t1(obj, dist_data, plain_verts):
    
    bpy.ops.object.select_all (action='DESELECT')
    # Select the object by name
    obj.select_set(True)
    
    me = obj.data

    # Get a BMesh representation
    bm = bmesh.new()
    bm.from_mesh(me)
   
    bm = mesh_helpers.bmesh_copy_from_object(obj, apply_modifiers=True)
   

    bmesh.ops.triangulate(bm, faces=bm.faces[:])
    
    area = sum(f.calc_area() for f in bm.faces)
    volume = bm.calc_volume()
    
    print (obj.name)
    dist_data.append (obj.name)
    print(area)
    dist_data.append (area)
    print (volume)
    dist_data.append (volume)
    print (plain_verts)
    dist_data.append (plain_verts)
    
    # Finish up, write the bmesh back to the mesh
    bm.to_mesh(me)
    bm.free()

    return 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# // NAME OF FUNCTION: update_names_t2
# //
# // PARAMETERS: collated_cubes, mitochondria_object, b_objects, contact_dist_data
# //
# // WHAT THE FUNCTION DOES: This function determines the intersection between the cube 
# // subdivisions and the mitochondria channel (which should now be only the overlap between
# // the ER and mitochondria) to dertermine mitochondrial-ER contact distribution
# //
# // DEPENDENCIES: triangulate_object_t2
# //
# // THE CODE: 

def update_names_t2 (collated_cubes, mitochondria_object, b_objects, contact_dist_data):

    # Naming everthing 
    # getting vertices from the cube 
    for o in collated_cubes:
        me = o.data
        # Get a BMesh representation
        bm = bmesh.new()
        bm.from_mesh(me)
    
        bm = mesh_helpers.bmesh_copy_from_object(o, apply_modifiers=True)
        bmesh.ops.triangulate(bm, faces=bm.faces[:])
        #cubearea = {o:sum([f.area for f in o.data.polygons])}
        cubearea = sum(f.calc_area() for f in bm.faces)
        cubevolume = bm.calc_volume()
        
        CubeName    = o.name 
        #Cube        = b_objects[o.name]
        bbverts     = [o.matrix_world @ Vector(bbvert) for bbvert in o.bound_box]
        plain_verts = [vert.to_tuple() for vert in bbverts]
        #bewl        = Cube.modifiers.new (type="BOOLEAN", name="MitoDist")
        #bewl.object = mitochondria_object

        # Deselect all objects
        bpy.ops.object.select_all(action='DESELECT')
        
        # Select the object by name
        o.select_set(True)

        # Set the object as the active object
        bpy.context.view_layer.objects.active = o

        # Set the names of the two objects
        obj_name_1 = o.name
        obj_name_2 = "ER"

        # Get the two objects by name
        obj1 = bpy.data.objects[obj_name_1]
        obj2 = bpy.data.objects[obj_name_2]

        # Add a boolean modifier to obj1
        mod = obj1.modifiers.new(name="Difference", type="BOOLEAN")
        mod.object = obj2
        mod.operation = 'DIFFERENCE'
        mod.solver = 'EXACT'
        # Enable hole tolerance
        mod.use_hole_tolerant = True

        # create triangulated mesh 
        triangulate_object_t2 (b_objects[o.name], cubearea, cubevolume, contact_dist_data, plain_verts)

        # remove the cube 
        bpy.data.objects.remove (bpy.context.scene.objects[CubeName], do_unlink=True)

    return 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# // NAME OF FUNCTION: triangulate_object_t2
# //
# // PARAMETERS: obj, cube_area, cube_volume, contact_dist_data, plain_verts
# //
# // WHAT THE FUNCTION DOES: This function outputs
# // the volume and surface area of each intersection along with the vertices of the 
# // segmentation box.
# //
# // DEPENDENCIES: none
# //
# // THE CODE: 

def triangulate_object_t2 (obj, cube_area, cube_volume, contact_dist_data, plain_verts):
    bpy.ops.object.select_all (action='DESELECT')
    # Select the object by name
    obj.select_set(True)
    
    me = obj.data

    # Get a BMesh representation
    bm = bmesh.new()
    bm.from_mesh(me)
   
    bm = mesh_helpers.bmesh_copy_from_object(obj, apply_modifiers=True)
   

    bmesh.ops.triangulate(bm, faces=bm.faces[:])
    
    area = sum(f.calc_area() for f in bm.faces)
    volume = bm.calc_volume()
   
   
    print (obj.name)
    contact_dist_data.append (obj.name)
    print (cube_area)
    contact_dist_data.append (cube_area) 
    print (area) 
    contact_dist_data.append (area) 
    print (cube_area)
    contact_dist_data.append (cube_volume) 
    print (area) 
    contact_dist_data.append (volume) 
    print (plain_verts)
    contact_dist_data.append (plain_verts)

    # finish up, write the bmesh back to the mesh 
    bm.to_mesh (me)
    bm.free() 

    return 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# // NAME OF FUNCTION: update_names_t3
# //
# // PARAMETERS: collated_cubes, cell_object, b_objects, cell_dist_data
# //
# // WHAT THE FUNCTION DOES: This function determines the intersection between the cube 
# // subdivisions and the cell channel to determine cell volume distribution
# //
# // DEPENDENCIES: triangulate_object_t3
# //
# // THE CODE: 

def update_names_t3 (collated_cubes, cell_object, b_objects, cell_dist_data):
    # Naming everything 
    # Getting vertices from the cube 
    for o in collated_cubes:
        CubeName    = o.name 
        #Cube        = b_objects[o.name]
        bbverts     = [o.matrix_world @ Vector(bbvert) for bbvert in o.bound_box]
        plain_verts = [vert.to_tuple() for vert in bbverts]
        #bewl        = Cube.modifiers.new (type="BOOLEAN", name="MitoDist")
        #bewl.object = mitochondria_object

        # Deselect all objects
        bpy.ops.object.select_all(action='DESELECT')
        
        # Select the object by name
        o.select_set(True)

        # Set the object as the active object
        bpy.context.view_layer.objects.active = o

        # Set the names of the two objects
        obj_name_1 = o.name
        obj_name_2 = "Mitochondria"

        # Get the two objects by name
        obj1 = bpy.data.objects[obj_name_1]
        obj2 = bpy.data.objects[obj_name_2]

        # Add a boolean modifier to obj1
        mod = obj1.modifiers.new(name="Interesect", type="BOOLEAN")
        mod.object = obj2
        mod.operation = 'INTERSECT'
        mod.solver = 'EXACT'
        # Enable hole tolerance
        mod.use_hole_tolerant = True


        # create triangulated mesh 
        triangulate_object_t3 (b_objects[o.name], o, plain_verts, cell_dist_data)

        # remove the cube
        bpy.data.objects.remove (bpy.context.scene.objects[CubeName], do_unlink = True)

    return 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# // NAME OF FUNCTION: triangulate_object_t3
# //
# // PARAMETERS: obj, b_obj, p_verts, cell_dist_data
# //
# // WHAT THE FUNCTION DOES: This function  outputs
# // the volume and surface area of each intersection along with the vertices of the 
# // segmentation box.
# //
# // DEPENDENCIES: none
# //
# // THE CODE: 

def triangulate_object_t3 (obj, b_obj, p_verts, cell_dist_data):
    
    bpy.ops.object.select_all (action='DESELECT')
    # Select the object by name
    obj.select_set(True)
    
    me = obj.data

    # Get a BMesh representation
    bm = bmesh.new()
    bm.from_mesh(me)
   
    bm = mesh_helpers.bmesh_copy_from_object(obj, apply_modifiers=True)
   

    bmesh.ops.triangulate(bm, faces=bm.faces[:])
    
    area = sum(f.calc_area() for f in bm.faces)
    volume = bm.calc_volume()
   
    print (b_obj.name) 
    cell_dist_data.append (b_obj.name)
    print (area) 
    cell_dist_data.append (area) 
    print (volume) 
    cell_dist_data.append (volume)
    print (p_verts)
    cell_dist_data.append (p_verts)

    bm.to_mesh(me)
    bm.free() 

    return 


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# // NAME OF FUNCTION: update_mesh_objects_er 
# //
# // PARAMETERS: mesh_obs, overlap_list
# //
# // WHAT THE FUNCTION DOES: This function sprints the mitochondrial channel 
# // (which is now just the mitochondrial-ER contact sites) by loose components and
# // counts the number of discrete contact sites
# //
# // DEPENDENCIES: none
# //
# // THE CODE: 

def  update_mesh_objects_er (mesh_obs, overlap_list):
    
    for o in mesh_obs:
        bpy.ops.object.select_all (action="DESELECT")
        object_to_select = bpy.data.objects["Mitochondria"]
        object_to_select.select_set(True)
        print ("Splitting mesh")
        bpy.ops.mesh.separate(type="LOOSE")

    objs = [obj for obj in bpy.context.scene.objects if obj.name.startswith("Mitochondria")]
    print (objs)
    n = 0
    # iterating over each intersection 
    for o in objs:
        bbverts = [o.matrix_world @ Vector(bbvert) for bbvert in o.bound_box]
        me = o.data
        # Get a BMesh representation
        bm = bmesh.new()
        bm.from_mesh(me)

        bm = mesh_helpers.bmesh_copy_from_object(o, apply_modifiers=True)
        bmesh.ops.triangulate(bm, faces=bm.faces[:])
        area   = sum(f.calc_area() for f in bm.faces)
        volume = bm.calc_volume()
        
        plain_verts = [vert.to_tuple() for vert in bbverts]
        
        me.update()
        bm.clear()
        overlap_list.append(area)
        overlap_list.append(volume)
        overlap_list.append(plain_verts)

        n = n+1

    print (overlap_list)
    print ("Number of mitochondrial-ER contact sites in this cell: ", n)

    return 


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# END OF LIBRARY

# =======================================================
# =======================================================

# =======================================================
# ============ OFFICIAL START OF THE PROGRAM ============
# =======================================================

# =======================================================
# =======================================================

objs = bpy.data.objects
meshes = bpy.data.meshes

for obj in objs:
    objs.remove(obj)
for mesh in meshes:
    meshes.remove(mesh)

# ******************************************************************

class OT_TestOpenFilebrowser (Operator, ImportHelper):

    bl_idname = "test.open_filebrowser"
    bl_label  = "Open the file browser (yay)"
    
    def execute(self, bpycontext):
        # Do something with the selected file(s).
        
        # This portion gets all the paths to the inputs  
        Desktop       = os.path.join(os.path.join(os.path.expanduser('~')), 'Desktop')
        MitoGraphData = os.path.join(os.path.join(os.path.expanduser('~')), 'Desktop','MitoGraphData') 
        FileName      = self.filepath
        Directory     = os.path.dirname(os.path.realpath(FileName))
        OnlyFiles     = [f for f in listdir(Directory) if isfile(join(Directory, f))]
        OnlyFullFiles = [f for f in OnlyFiles if 'All.x3d' in f]
        
        # iterating through files that end in All
        # this is the main chunk of computation 

        for file in OnlyFullFiles:

                # Getting details from filenames
                objs = bpy.data.objects
                meshes = bpy.data.meshes
                for obj in objs:
                    objs.remove(obj)
                for mesh in meshes:
                    meshes.remove(mesh)

                FileName = Directory + '/' + file
                NoExtFile = file[:-4]
                print (FileName)
                SmallFileName= FileName[:-4] 
                SmallFileName= SmallFileName.replace(MitoGraphData,'')
                SmallFileName = SmallFileName.replace("/", "-")
                
                #import x3d file
                bpy.ops.import_scene.x3d (filepath = FileName)
                
                bpycontext = bpy.context
                
                #extracting mesh objects
                mesh_obs = [o for o in bpycontext.scene.objects if o.type == 'MESH']

                out_list        = []
                vol_list        = []
                DistData        = []
                ContactDistData = []
                CellDistData    = []
                BBList          = []
                OtherOut        = []
                OverlapList     = []
                
                # Iterating through mesh objects
                # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                Output, SortedBBList = GetVolumeAndDimensions (mesh_obs, BBList, vol_list) 
                print (Output)
                print (SortedBBList)
                

                # Making sure each imported x3d has 5 mesh objects
                #    1. Cell Surface
                #    2. ER Surface
                #    3. ER Nodes
                #    4. Mitochondria Surface
                #    5. Mitochondria Nodes

                if len(mesh_obs) == 5:

                    mesh_obs = IdentifyAndNameCell (mesh_obs, Output)

                    mesh_obs = LabelMaker ( mesh_obs, SortedBBList, OtherOut ) 
                    
                    # Everything is labelled now!
                    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
                    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                    # append info to out_list in a certain fashion 
                    
                    out_list = update_out_list (out_list, bpycontext) 


                    # Labeling Stuff
                    objects = bpy.data.objects   
                    Mitochondria = objects['Mitochondria']
                    ER = objects['ER']  
                    
                    # Get bounding box, rotation and center point for the cell
                    o = bpy.context.scene.objects['Cell']  
                    local_bbox_center = 0.125 * sum((Vector(b) for b in o.bound_box), Vector())
                    global_bbox_center = o.matrix_world @ local_bbox_center
                    
                    # Progress update print statements
                    print (bpy.data.objects['Cell'].dimensions)
                    print (bpy.data.objects['Cell'].rotation_euler)
                    
                    # set the name of the object you want to copy
                    obj_name = 'Cell'

                    # get the object by name
                    obj = bpy.data.objects[obj_name]

                    # duplicate the object
                    bpy.context.view_layer.objects.active = obj
                    obj.select_set(True)
                    bpy.ops.object.duplicate()
                    new_obj = bpy.context.active_object

                    # set the origin of the new object to the center of the original object
                    new_obj.location = obj.location
                    bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')
                    new_obj.scale *= 1.2
                    new_obj.name = "UpscaledCell"
                    
                    # Set the number of fractions (11 will increment by 10ths)
                    N = 11

                    # Generate a list of N fractions between 1 and 0
                    if N > 1:
                        fraction_list = [i/(N-1) for i in range(N)]
                    else:
                        fraction_list = [1]

                    # Print the list to the console
                    print(fraction_list)

                    # Iterate through the list and print each fraction
                    for fraction in fraction_list:
                        print(fraction)
                        n= fraction
                        
                        # set the name of the object you want to copy
                        obj_name = 'UpscaledCell'

                        # get the object by name
                        obj = bpy.data.objects[obj_name]

                        # duplicate the object
                        bpy.context.view_layer.objects.active = obj
                        obj.select_set(True)
                        bpy.ops.object.duplicate()
                        new_obj = bpy.context.active_object

                        # set the origin of the new object to the center of the original object
                        new_obj.location = obj.location
                        new_obj.name = f"CellCopyOutside{n}"
                        bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')

                        # Deselect all objects
                        bpy.ops.object.select_all(action='DESELECT')

                        # Select the object by name
                        obj.select_set(True)

                        # Set the object as the active object
                        bpy.context.view_layer.objects.active = obj
                        bpy.ops.object.duplicate()
                        newer_obj = bpy.context.active_object
                        newer_obj.name = f"CellCopyInside{n}"

                        #set the origin of the new object to the center of the original object
                        newer_obj.location = obj.location
                        bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')
                        newer_obj.scale *= n

                        # Deselect all objects
                        bpy.ops.object.select_all(action='DESELECT')
                        
                        # Select the object by name
                        obj.select_set(True)

                        # Set the object as the active object
                        bpy.context.view_layer.objects.active = obj
                        
                        if n != fraction_list[-1] :

                            # Set the names of the two objects
                            obj_name_1 = new_obj.name
                            obj_name_2 = newer_obj.name
                            
                            print (obj_name_1)
                            print (obj_name_2)

                            # Get the two objects by name
                            obj1 = bpy.data.objects[obj_name_1]
                            obj2 = bpy.data.objects[obj_name_2]

                            # Add a boolean modifier to obj1
                            mod = obj1.modifiers.new(name="Difference", type="BOOLEAN")
                            mod.object = obj2
                            mod.operation = 'DIFFERENCE'
                            mod.solver = 'EXACT'
                            # Enable hole tolerance
                            mod.use_hole_tolerant = True

                            # Apply the modifier to obj1
                            bpy.ops.object.modifier_apply ({"object": obj1}, modifier=mod.name)
                        
                        
                        else:  
                            new_obj.scale *= 0
                        
                        # Deselect all objects
                        bpy.ops.object.select_all(action='DESELECT')
                    
                    
                    
                    # Collates all the smaller cubes we made
                    objs = [obj for obj in bpy.context.scene.objects if obj.name.startswith("CellCopy")]
                    
                    # Extract out the objects labelled Mitochondria
                    objects = bpy.data.objects   
                    Cell = objects['Cell']
                    Mitochondria = objects['Mitochondria']
                    bm = bmesh.new()

                    update_names_t1 (objs, Mitochondria, objects, DistData)
                    
                    
                    
                    ### Repeat for difference between cube and Mito
                    # create a box with the same rotation and center point and dimensions as the cell
                    # We now want to get TOTAL surface area of mitochondria-ER contact and number of contacts along with the coordinates of their bounding box

                
                    # we get select the mitochondria intersections
                    objects = bpy.data.objects   
                    bpycontext = bpy.context
                    Mitochondria = objects['Mitochondria']
                    ER = objects['ER'] 
                    Cell = objects['Cell']
                    
                    
                    
                    # Iterate through the list and print each fraction
                    for fraction in fraction_list:
                        print(fraction)
                        n= fraction
                        
                        # set the name of the object you want to copy
                        obj_name = 'UpscaledCell'

                        # get the object by name
                        obj = bpy.data.objects[obj_name]

                        # duplicate the object
                        bpy.context.view_layer.objects.active = obj
                        obj.select_set(True)
                        bpy.ops.object.duplicate()
                        new_obj = bpy.context.active_object

                        # set the origin of the new object to the center of the original object
                        new_obj.location = obj.location
                        new_obj.name = f"CellCopyOutside{n}"
                        bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')

                        # Deselect all objects
                        bpy.ops.object.select_all(action='DESELECT')

                        # Select the object by name
                        obj.select_set(True)

                        # Set the object as the active object
                        bpy.context.view_layer.objects.active = obj
                        bpy.ops.object.duplicate()
                        newer_obj = bpy.context.active_object
                        newer_obj.name = f"CellCopyInside{n}"

                        #set the origin of the new object to the center of the original object
                        newer_obj.location = obj.location
                        bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')
                        newer_obj.scale *= n

                        # Deselect all objects
                        bpy.ops.object.select_all(action='DESELECT')
                        
                        # Select the object by name
                        obj.select_set(True)

                        # Set the object as the active object
                        bpy.context.view_layer.objects.active = obj
                        
                        if n != fraction_list[-1] :

                            # Set the names of the two objects
                            obj_name_1 = new_obj.name
                            obj_name_2 = newer_obj.name
                            
                            print (obj_name_1)
                            print (obj_name_2)

                            # Get the two objects by name
                            obj1 = bpy.data.objects[obj_name_1]
                            obj2 = bpy.data.objects[obj_name_2]

                            # Add a boolean modifier to obj1
                            mod = obj1.modifiers.new(name="Difference", type="BOOLEAN")
                            mod.object = obj2
                            mod.operation = 'DIFFERENCE'
                            mod.solver = 'EXACT'
                            # Enable hole tolerance
                            mod.use_hole_tolerant = True

                            # Apply the modifier to obj1
                            bpy.ops.object.modifier_apply ({"object": obj1}, modifier=mod.name)
                        
                        
                        else:  
                            new_obj.scale *= 0
                        
                        # Deselect all objects
                        bpy.ops.object.select_all(action='DESELECT')
                    
                    
                    # Collates all the smaller cubes we made
                    objs = [obj for obj in bpy.context.scene.objects if obj.name.startswith("CellCopy")]
                    
                    # Extract out the objects labelled Mitochondria
                    objects = bpy.data.objects   
                    Mitochondria = objects['Mitochondria']
                    Cell = objects['Cell']
                    bm = bmesh.new()

                    # Iterating through small cube array
                    update_names_t2 (objs, Mitochondria, objects, ContactDistData)
                    
                    
                    #Apply boolean modifier so that mitochondria now refers to the mito ER intersections
                    bool = Mitochondria.modifiers.new(type="BOOLEAN", name="Overlap")
                    bool.object = ER
                    bool.operation = 'INTERSECT'
                    bool.solver = "EXACT"
                    bpy.ops.object.modifier_apply({"object": Mitochondria}, modifier=bool.name)


                    ###SAME PROCEDURE FOR CELL DISTRIBUTION
                    # create a box with the same rotation and center point and dimensions as the cell
                
                    # Iterate through the list and print each fraction
                    for fraction in fraction_list:
                        print(fraction)
                        n= fraction
                        
                        # set the name of the object you want to copy
                        obj_name = 'UpscaledCell'

                        # get the object by name
                        obj = bpy.data.objects[obj_name]

                        # duplicate the object
                        bpy.context.view_layer.objects.active = obj
                        obj.select_set(True)
                        bpy.ops.object.duplicate()
                        new_obj = bpy.context.active_object

                        # set the origin of the new object to the center of the original object
                        new_obj.location = obj.location
                        new_obj.name = f"CellCopyOutside{n}"
                        bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')

                        # Deselect all objects
                        bpy.ops.object.select_all(action='DESELECT')

                        # Select the object by name
                        obj.select_set(True)

                        # Set the object as the active object
                        bpy.context.view_layer.objects.active = obj
                        bpy.ops.object.duplicate()
                        newer_obj = bpy.context.active_object
                        newer_obj.name = f"CellCopyInside{n}"

                        #set the origin of the new object to the center of the original object
                        newer_obj.location = obj.location
                        bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')
                        newer_obj.scale *= n

                        # Deselect all objects
                        bpy.ops.object.select_all(action='DESELECT')
                        
                        # Select the object by name
                        obj.select_set(True)

                        # Set the object as the active object
                        bpy.context.view_layer.objects.active = obj
                        
                        if n != fraction_list[-1] :

                            # Set the names of the two objects
                            obj_name_1 = new_obj.name
                            obj_name_2 = newer_obj.name
                            
                            print (obj_name_1)
                            print (obj_name_2)

                            # Get the two objects by name
                            obj1 = bpy.data.objects[obj_name_1]
                            obj2 = bpy.data.objects[obj_name_2]

                            # Add a boolean modifier to obj1
                            mod = obj1.modifiers.new(name="Difference", type="BOOLEAN")
                            mod.object = obj2
                            mod.operation = 'DIFFERENCE'
                            mod.solver = 'EXACT'
                            # Enable hole tolerance
                            mod.use_hole_tolerant = True

                            # Apply the modifier to obj1
                            bpy.ops.object.modifier_apply ({"object": obj1}, modifier=mod.name)
                        
                        
                        else:  
                            new_obj.scale *= 0
                        
                        # Deselect all objects
                        bpy.ops.object.select_all(action='DESELECT')
                        
                    objs = [obj for obj in bpy.context.scene.objects if obj.name.startswith("CellCopy")]
                    
                    # Extract out the objects labelled Mitochondria
                    objects = bpy.data.objects   
                    Mitochondria = objects['Mitochondria']
                    bm = bmesh.new()

                    # Iterating through small cube array
                    update_names_t3 (objs, Cell, objects, CellDistData)
                    
                   
                    # split into individual intersections
                    mesh_obs = [o for o in bpy.context.scene.objects if o.name == 'Mitochondria']
                    print (mesh_obs)

                    update_mesh_objects_er (mesh_obs, OverlapList)
                
                
                
                #This will run if there are only three structures (one of them must be the cell)
                elif len(mesh_obs) == 3:
                    mesh_obs = IdentifyAndNameCell (mesh_obs, Output)

                    mesh_obs = ThreeStructureLabelMaker ( mesh_obs, SortedBBList, OtherOut ) 
                    
                    # Everything is labelled now!
                    # Everything is labelled now!
                    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
                    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                    # append info to out_list in a certain fashion 
                    
                    out_list = update_out_list (out_list, bpycontext) 


                    # Labeling Stuff
                    objects = bpy.data.objects   
                    Mitochondria = objects['Mitochondria']
                    ER = objects['ER']  
                    
                    # Get bounding box, rotation and center point for the cell
                    o = bpy.context.scene.objects['Cell']  
                    local_bbox_center = 0.125 * sum((Vector(b) for b in o.bound_box), Vector())
                    global_bbox_center = o.matrix_world @ local_bbox_center
                    
                    # Progress update print statements
                    print (bpy.data.objects['Cell'].dimensions)
                    print (bpy.data.objects['Cell'].rotation_euler)
                    
                    # set the name of the object you want to copy
                    obj_name = 'Cell'

                    # get the object by name
                    obj = bpy.data.objects[obj_name]

                    # duplicate the object
                    bpy.context.view_layer.objects.active = obj
                    obj.select_set(True)
                    bpy.ops.object.duplicate()
                    new_obj = bpy.context.active_object

                    # set the origin of the new object to the center of the original object
                    new_obj.location = obj.location
                    bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')
                    new_obj.scale *= 1.2
                    new_obj.name = "UpscaledCell"
                    
                    # Set the number of fractions (11 will increment by 10ths)
                    N = 11

                    # Generate a list of N fractions between 1 and 0
                    if N > 1:
                        fraction_list = [i/(N-1) for i in range(N)]
                    else:
                        fraction_list = [1]

                    # Print the list to the console
                    print(fraction_list)

                    # Iterate through the list and print each fraction
                    for fraction in fraction_list:
                        print(fraction)
                        n= fraction
                        
                        # set the name of the object you want to copy
                        obj_name = 'UpscaledCell'

                        # get the object by name
                        obj = bpy.data.objects[obj_name]

                        # duplicate the object
                        bpy.context.view_layer.objects.active = obj
                        obj.select_set(True)
                        bpy.ops.object.duplicate()
                        new_obj = bpy.context.active_object

                        # set the origin of the new object to the center of the original object
                        new_obj.location = obj.location
                        new_obj.name = f"CellCopyOutside{n}"
                        bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')

                        # Deselect all objects
                        bpy.ops.object.select_all(action='DESELECT')

                        # Select the object by name
                        obj.select_set(True)

                        # Set the object as the active object
                        bpy.context.view_layer.objects.active = obj
                        bpy.ops.object.duplicate()
                        newer_obj = bpy.context.active_object
                        newer_obj.name = f"CellCopyInside{n}"

                        #set the origin of the new object to the center of the original object
                        newer_obj.location = obj.location
                        bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')
                        newer_obj.scale *= n

                        # Deselect all objects
                        bpy.ops.object.select_all(action='DESELECT')
                        
                        # Select the object by name
                        obj.select_set(True)

                        # Set the object as the active object
                        bpy.context.view_layer.objects.active = obj
                        
                        if n != fraction_list[-1] :

                            # Set the names of the two objects
                            obj_name_1 = new_obj.name
                            obj_name_2 = newer_obj.name
                            
                            print (obj_name_1)
                            print (obj_name_2)

                            # Get the two objects by name
                            obj1 = bpy.data.objects[obj_name_1]
                            obj2 = bpy.data.objects[obj_name_2]

                            # Add a boolean modifier to obj1
                            mod = obj1.modifiers.new(name="Difference", type="BOOLEAN")
                            mod.object = obj2
                            mod.operation = 'DIFFERENCE'
                            mod.solver = 'EXACT'
                            # Enable hole tolerance
                            mod.use_hole_tolerant = True

                            # Apply the modifier to obj1
                            bpy.ops.object.modifier_apply ({"object": obj1}, modifier=mod.name)
                        
                        
                        else:  
                            new_obj.scale *= 0
                        
                        # Deselect all objects
                        bpy.ops.object.select_all(action='DESELECT')
                    
                    
                    
                    # Collates all the smaller cubes we made
                    objs = [obj for obj in bpy.context.scene.objects if obj.name.startswith("CellCopy")]
                    
                    # Extract out the objects labelled Mitochondria
                    objects = bpy.data.objects   
                    Cell = objects['Cell']
                    Mitochondria = objects['Mitochondria']
                    bm = bmesh.new()

                    update_names_t1 (objs, Mitochondria, objects, DistData)
                    
                    
                    
                    ### Repeat for difference between cube and Mito
                    # create a box with the same rotation and center point and dimensions as the cell
                    # We now want to get TOTAL surface area of mitochondria-ER contact and number of contacts along with the coordinates of their bounding box

                
                    # we get select the mitochondria intersections
                    objects = bpy.data.objects   
                    bpycontext = bpy.context
                    Mitochondria = objects['Mitochondria']
                    ER = objects['ER'] 
                    Cell = objects['Cell']
                    
                    
                    
                    # Iterate through the list and print each fraction
                    for fraction in fraction_list:
                        print(fraction)
                        n= fraction
                        
                        # set the name of the object you want to copy
                        obj_name = 'UpscaledCell'

                        # get the object by name
                        obj = bpy.data.objects[obj_name]

                        # duplicate the object
                        bpy.context.view_layer.objects.active = obj
                        obj.select_set(True)
                        bpy.ops.object.duplicate()
                        new_obj = bpy.context.active_object

                        # set the origin of the new object to the center of the original object
                        new_obj.location = obj.location
                        new_obj.name = f"CellCopyOutside{n}"
                        bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')

                        # Deselect all objects
                        bpy.ops.object.select_all(action='DESELECT')

                        # Select the object by name
                        obj.select_set(True)

                        # Set the object as the active object
                        bpy.context.view_layer.objects.active = obj
                        bpy.ops.object.duplicate()
                        newer_obj = bpy.context.active_object
                        newer_obj.name = f"CellCopyInside{n}"

                        #set the origin of the new object to the center of the original object
                        newer_obj.location = obj.location
                        bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')
                        newer_obj.scale *= n

                        # Deselect all objects
                        bpy.ops.object.select_all(action='DESELECT')
                        
                        # Select the object by name
                        obj.select_set(True)

                        # Set the object as the active object
                        bpy.context.view_layer.objects.active = obj
                        
                        if n != fraction_list[-1] :

                            # Set the names of the two objects
                            obj_name_1 = new_obj.name
                            obj_name_2 = newer_obj.name
                            
                            print (obj_name_1)
                            print (obj_name_2)

                            # Get the two objects by name
                            obj1 = bpy.data.objects[obj_name_1]
                            obj2 = bpy.data.objects[obj_name_2]

                            # Add a boolean modifier to obj1
                            mod = obj1.modifiers.new(name="Difference", type="BOOLEAN")
                            mod.object = obj2
                            mod.operation = 'DIFFERENCE'
                            mod.solver = 'EXACT'
                            # Enable hole tolerance
                            mod.use_hole_tolerant = True

                            # Apply the modifier to obj1
                            bpy.ops.object.modifier_apply ({"object": obj1}, modifier=mod.name)
                        
                        
                        else:  
                            new_obj.scale *= 0
                        
                        # Deselect all objects
                        bpy.ops.object.select_all(action='DESELECT')
                    
                    
                    # Collates all the smaller cubes we made
                    objs = [obj for obj in bpy.context.scene.objects if obj.name.startswith("CellCopy")]
                    
                    # Extract out the objects labelled Mitochondria
                    objects = bpy.data.objects   
                    Mitochondria = objects['Mitochondria']
                    Cell = objects['Cell']
                    bm = bmesh.new()

                    # Iterating through small cube array
                    update_names_t2 (objs, Mitochondria, objects, ContactDistData)
                    
                    
                    #Apply boolean modifier so that mitochondria now refers to the mito ER intersections
                    bool = Mitochondria.modifiers.new(type="BOOLEAN", name="Overlap")
                    bool.object = ER
                    bool.operation = 'INTERSECT'
                    bool.solver = "EXACT"
                    bpy.ops.object.modifier_apply({"object": Mitochondria}, modifier=bool.name)


                    ###SAME PROCEDURE FOR CELL DISTRIBUTION
                    # create a box with the same rotation and center point and dimensions as the cell
                
                    # Iterate through the list and print each fraction
                    for fraction in fraction_list:
                        print(fraction)
                        n= fraction
                        
                        # set the name of the object you want to copy
                        obj_name = 'UpscaledCell'

                        # get the object by name
                        obj = bpy.data.objects[obj_name]

                        # duplicate the object
                        bpy.context.view_layer.objects.active = obj
                        obj.select_set(True)
                        bpy.ops.object.duplicate()
                        new_obj = bpy.context.active_object

                        # set the origin of the new object to the center of the original object
                        new_obj.location = obj.location
                        new_obj.name = f"CellCopyOutside{n}"
                        bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')

                        # Deselect all objects
                        bpy.ops.object.select_all(action='DESELECT')

                        # Select the object by name
                        obj.select_set(True)

                        # Set the object as the active object
                        bpy.context.view_layer.objects.active = obj
                        bpy.ops.object.duplicate()
                        newer_obj = bpy.context.active_object
                        newer_obj.name = f"CellCopyInside{n}"

                        #set the origin of the new object to the center of the original object
                        newer_obj.location = obj.location
                        bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')
                        newer_obj.scale *= n

                        # Deselect all objects
                        bpy.ops.object.select_all(action='DESELECT')
                        
                        # Select the object by name
                        obj.select_set(True)

                        # Set the object as the active object
                        bpy.context.view_layer.objects.active = obj
                        
                        if n != fraction_list[-1] :

                            # Set the names of the two objects
                            obj_name_1 = new_obj.name
                            obj_name_2 = newer_obj.name
                            
                            print (obj_name_1)
                            print (obj_name_2)

                            # Get the two objects by name
                            obj1 = bpy.data.objects[obj_name_1]
                            obj2 = bpy.data.objects[obj_name_2]

                            # Add a boolean modifier to obj1
                            mod = obj1.modifiers.new(name="Difference", type="BOOLEAN")
                            mod.object = obj2
                            mod.operation = 'DIFFERENCE'
                            mod.solver = 'EXACT'
                            # Enable hole tolerance
                            mod.use_hole_tolerant = True

                            # Apply the modifier to obj1
                            bpy.ops.object.modifier_apply ({"object": obj1}, modifier=mod.name)
                        
                        
                        else:  
                            new_obj.scale *= 0
                        
                        # Deselect all objects
                        bpy.ops.object.select_all(action='DESELECT')
                        
                    objs = [obj for obj in bpy.context.scene.objects if obj.name.startswith("CellCopy")]
                    
                    # Extract out the objects labelled Mitochondria
                    objects = bpy.data.objects   
                    Mitochondria = objects['Mitochondria']
                    bm = bmesh.new()

                    # Iterating through small cube array
                    update_names_t3 (objs, Cell, objects, CellDistData)
                    
                   
                    # split into individual intersections
                    mesh_obs = [o for o in bpy.context.scene.objects if o.name == 'Mitochondria']
                    print (mesh_obs)

                    update_mesh_objects_er (mesh_obs, OverlapList)
                
                
                #This will run if there are only two structures (one of them must be the cell)
                elif len(mesh_obs) == 2:
                    mesh_obs = IdentifyAndNameCell (mesh_obs, Output)

                    mesh_obs = TwoStructureLabelMaker ( mesh_obs, SortedBBList, OtherOut ) 
                    
                    # Everything is labelled now!
                    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
                    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                    # append info to out_list in a certain fashion 
                    
                    out_list = update_out_list (out_list, bpycontext) 


                    # Get bounding box, rotation and center point for the cell
                    o = bpy.context.scene.objects['Cell']  
                    local_bbox_center = 0.125 * sum((Vector(b) for b in o.bound_box), Vector())
                    global_bbox_center = o.matrix_world @ local_bbox_center
                    
                    # Progress update print statements
                    print (bpy.data.objects['Cell'].dimensions)
                    print (bpy.data.objects['Cell'].rotation_euler)
                    
                    # set the name of the object you want to copy
                    obj_name = 'Cell'

                    # get the object by name
                    obj = bpy.data.objects[obj_name]

                    # duplicate the object
                    bpy.context.view_layer.objects.active = obj
                    obj.select_set(True)
                    bpy.ops.object.duplicate()
                    new_obj = bpy.context.active_object

                    # set the origin of the new object to the center of the original object
                    new_obj.location = obj.location
                    bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')
                    new_obj.scale *= 1.2
                    new_obj.name = "UpscaledCell"
                    
                    # Set the number of fractions (11 will increment by 10ths)
                    N = 11

                    # Generate a list of N fractions between 1 and 0
                    if N > 1:
                        fraction_list = [i/(N-1) for i in range(N)]
                    else:
                        fraction_list = [1]

                    # Print the list to the console
                    print(fraction_list)

                    # Iterate through the list and print each fraction
                    for fraction in fraction_list:
                        print(fraction)
                        n= fraction
                        
                        # set the name of the object you want to copy
                        obj_name = 'UpscaledCell'

                        # get the object by name
                        obj = bpy.data.objects[obj_name]

                        # duplicate the object
                        bpy.context.view_layer.objects.active = obj
                        obj.select_set(True)
                        bpy.ops.object.duplicate()
                        new_obj = bpy.context.active_object

                        # set the origin of the new object to the center of the original object
                        new_obj.location = obj.location
                        new_obj.name = f"CellCopyOutside{n}"
                        bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')

                        # Deselect all objects
                        bpy.ops.object.select_all(action='DESELECT')

                        # Select the object by name
                        obj.select_set(True)

                        # Set the object as the active object
                        bpy.context.view_layer.objects.active = obj
                        bpy.ops.object.duplicate()
                        newer_obj = bpy.context.active_object
                        newer_obj.name = f"CellCopyInside{n}"

                        #set the origin of the new object to the center of the original object
                        newer_obj.location = obj.location
                        bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')
                        newer_obj.scale *= n

                        # Deselect all objects
                        bpy.ops.object.select_all(action='DESELECT')
                        
                        # Select the object by name
                        obj.select_set(True)

                        # Set the object as the active object
                        bpy.context.view_layer.objects.active = obj
                        
                        if n != fraction_list[-1] :

                            # Set the names of the two objects
                            obj_name_1 = new_obj.name
                            obj_name_2 = newer_obj.name
                            
                            print (obj_name_1)
                            print (obj_name_2)

                            # Get the two objects by name
                            obj1 = bpy.data.objects[obj_name_1]
                            obj2 = bpy.data.objects[obj_name_2]

                            # Add a boolean modifier to obj1
                            mod = obj1.modifiers.new(name="Difference", type="BOOLEAN")
                            mod.object = obj2
                            mod.operation = 'DIFFERENCE'
                            mod.solver = 'EXACT'
                            # Enable hole tolerance
                            mod.use_hole_tolerant = True

                            # Apply the modifier to obj1
                            bpy.ops.object.modifier_apply ({"object": obj1}, modifier=mod.name)
                        
                        
                        else:  
                            new_obj.scale *= 0
                        
                        # Deselect all objects
                        bpy.ops.object.select_all(action='DESELECT')
                    
                    
                    
                    # Collates all the smaller cubes we made
                    objs = [obj for obj in bpy.context.scene.objects if obj.name.startswith("CellCopy")]
                    
                    # Extract out the objects labelled Mitochondria
                    objects = bpy.data.objects   
                    Cell = objects['Cell']
                    Mitochondria = objects['SubcellularStructure']
                    bm = bmesh.new()

                    update_names_t1 (objs, Mitochondria, objects, DistData)
                    
                    ### Repeat for difference between cube and Mito
                    # create a box with the same rotation and center point and dimensions as the cell
                     # We now want to get TOTAL surface area of mitochondria-ER contact and number of contacts along with the coordinates of their bounding box


                    # we get select the mitochondria intersections
                    objects = bpy.data.objects   
                    bpycontext = bpy.context
                    Mitochondria = objects['SubcellularStructure']
                    Cell = objects['Cell']
                    
                    
                    # Iterate through the list and print each fraction
                    for fraction in fraction_list:
                        print(fraction)
                        n= fraction
                        
                        # set the name of the object you want to copy
                        obj_name = 'UpscaledCell'

                        # get the object by name
                        obj = bpy.data.objects[obj_name]

                        # duplicate the object
                        bpy.context.view_layer.objects.active = obj
                        obj.select_set(True)
                        bpy.ops.object.duplicate()
                        new_obj = bpy.context.active_object

                        # set the origin of the new object to the center of the original object
                        new_obj.location = obj.location
                        new_obj.name = f"CellCopyOutside{n}"
                        bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')

                        # Deselect all objects
                        bpy.ops.object.select_all(action='DESELECT')

                        # Select the object by name
                        obj.select_set(True)

                        # Set the object as the active object
                        bpy.context.view_layer.objects.active = obj
                        bpy.ops.object.duplicate()
                        newer_obj = bpy.context.active_object
                        newer_obj.name = f"CellCopyInside{n}"

                        #set the origin of the new object to the center of the original object
                        newer_obj.location = obj.location
                        bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')
                        newer_obj.scale *= n

                        # Deselect all objects
                        bpy.ops.object.select_all(action='DESELECT')
                        
                        # Select the object by name
                        obj.select_set(True)

                        # Set the object as the active object
                        bpy.context.view_layer.objects.active = obj
                        
                        if n != fraction_list[-1] :

                            # Set the names of the two objects
                            obj_name_1 = new_obj.name
                            obj_name_2 = newer_obj.name
                            
                            print (obj_name_1)
                            print (obj_name_2)

                            # Get the two objects by name
                            obj1 = bpy.data.objects[obj_name_1]
                            obj2 = bpy.data.objects[obj_name_2]

                            # Add a boolean modifier to obj1
                            mod = obj1.modifiers.new(name="Difference", type="BOOLEAN")
                            mod.object = obj2
                            mod.operation = 'DIFFERENCE'
                            mod.solver = 'EXACT'
                            # Enable hole tolerance
                            mod.use_hole_tolerant = True

                            # Apply the modifier to obj1
                            bpy.ops.object.modifier_apply ({"object": obj1}, modifier=mod.name)
                        
                        
                        else:  
                            new_obj.scale *= 0
                        
                        # Deselect all objects
                        bpy.ops.object.select_all(action='DESELECT')
                    
                    
                    # Collates all the smaller cubes we made
                    objs = [obj for obj in bpy.context.scene.objects if obj.name.startswith("CellCopy")]
                    
                   
                    # Iterating through small cube array
                    update_names_t2 (objs, Mitochondria, objects, ContactDistData)
                    
                    

                     ###SAME PROCEDURE FOR CELL DISTRIBUTION
                    # create a box with the same rotation and center point and dimensions as the cell
                
                    # Iterate through the list and print each fraction
                    for fraction in fraction_list:
                        print(fraction)
                        n= fraction
                        
                        # set the name of the object you want to copy
                        obj_name = 'UpscaledCell'

                        # get the object by name
                        obj = bpy.data.objects[obj_name]

                        # duplicate the object
                        bpy.context.view_layer.objects.active = obj
                        obj.select_set(True)
                        bpy.ops.object.duplicate()
                        new_obj = bpy.context.active_object

                        # set the origin of the new object to the center of the original object
                        new_obj.location = obj.location
                        new_obj.name = f"CellCopyOutside{n}"
                        bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')

                        # Deselect all objects
                        bpy.ops.object.select_all(action='DESELECT')

                        # Select the object by name
                        obj.select_set(True)

                        # Set the object as the active object
                        bpy.context.view_layer.objects.active = obj
                        bpy.ops.object.duplicate()
                        newer_obj = bpy.context.active_object
                        newer_obj.name = f"CellCopyInside{n}"

                        #set the origin of the new object to the center of the original object
                        newer_obj.location = obj.location
                        bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')
                        newer_obj.scale *= n

                        # Deselect all objects
                        bpy.ops.object.select_all(action='DESELECT')
                        
                        # Select the object by name
                        obj.select_set(True)

                        # Set the object as the active object
                        bpy.context.view_layer.objects.active = obj
                        
                        if n != fraction_list[-1] :

                            # Set the names of the two objects
                            obj_name_1 = new_obj.name
                            obj_name_2 = newer_obj.name
                            
                            print (obj_name_1)
                            print (obj_name_2)

                            # Get the two objects by name
                            obj1 = bpy.data.objects[obj_name_1]
                            obj2 = bpy.data.objects[obj_name_2]

                            # Add a boolean modifier to obj1
                            mod = obj1.modifiers.new(name="Difference", type="BOOLEAN")
                            mod.object = obj2
                            mod.operation = 'DIFFERENCE'
                            mod.solver = 'EXACT'
                            # Enable hole tolerance
                            mod.use_hole_tolerant = True

                            # Apply the modifier to obj1
                            bpy.ops.object.modifier_apply ({"object": obj1}, modifier=mod.name)
                        
                        
                        else:  
                            new_obj.scale *= 0
                        
                        # Deselect all objects
                        bpy.ops.object.select_all(action='DESELECT')
                        
                    objs = [obj for obj in bpy.context.scene.objects if obj.name.startswith("CellCopy")]
                    
                    # Extract out the objects labelled Mitochondria
                    objects = bpy.data.objects   
                    Cell = objects['Cell']
                    bm = bmesh.new()

                    # Iterating through small cube array
                    update_names_t3 (objs, Cell, objects, CellDistData)
                    
                    
                
                else:
                    print (SmallFileName +' is an incomplete file.')
                  
                
                CSV_Writer ( Desktop, SmallFileName, out_list, OtherOut, DistData, CellDistData, ContactDistData, OverlapList )


        # TO ADD: Print list of filenames that failed
        return {'FINISHED'}



# ******************************************************************

# everything happens in OT_Testopenfilebrowser 
# you need register() and unregister() to actually get the code to run 
# has to be there 

# 

def register():
    bpy.utils.register_class(OT_TestOpenFilebrowser)


def unregister():
    bpy.utils.unregister_class(OT_TestOpenFilebrowser)


# This is what Runs the program
register()

name = bpy.ops.test.open_filebrowser('INVOKE_DEFAULT')

