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

objs = bpy.data.objects
meshes = bpy.data.meshes
for obj in objs:
    objs.remove(obj)
for mesh in meshes:
    meshes.remove(mesh)

class OT_TestOpenFilebrowser(Operator, ImportHelper):

    bl_idname = "test.open_filebrowser"
    bl_label = "Open the file browser (yay)"
    
    def execute(self, context):
        """Do something with the selected file(s)."""
        Desktop = os.path.join(os.path.join(os.path.expanduser('~')), 'Desktop')
        MitoGraphData = os.path.join(os.path.join(os.path.expanduser('~')), 'Desktop','MitoGraphData') 
        FileName = self.filepath
        Directory = os.path.dirname(os.path.realpath(FileName))
        OnlyFiles = [f for f in listdir(Directory) if isfile(join(Directory, f))]
        OnlyFullFiles = [f for f in OnlyFiles if 'All.x3d' in f]
        
        # iterating through files that end in All
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
                bpy.ops.import_scene.x3d(filepath = FileName)
                
                
               
                context = bpy.context
                
                #extracting mesh objects
                mesh_obs = [o for o in context.scene.objects if o.type == 'MESH']


                out_list = []
                vol_list = []
                DistData = []
                ContactDistData= []
                CellDistData= []
                BBList = []
                OtherOut = []
                
                # Iterating through mesh objects
                for o in mesh_obs:
                    #Get bounding box dimensions
                    H = bpy.data.objects[o.name].dimensions.x
                    L = bpy.data.objects[o.name].dimensions.y
                    W = bpy.data.objects[o.name].dimensions.z
                    
                    #Vol of bounding box
                    BBVolume = H * L * W
                    BBList.append(BBVolume)
                    
                    me = o.data
                    
                    # Creating to blender mesh
                    bm = bmesh.new()
                    bm.from_mesh(me)
                    bm.to_mesh(me)
                    
                    # Determining blender vertices
                    bbverts = [o.matrix_world @ Vector(bbvert) for bbvert in o.bound_box]
                    
                    # Possible backwards comptability for Blender v < 2.8
                    #bbverts = [o.matrix_world * Vector(bbvert) for bbvert in o.bound_box]
                    
                    plain_verts = [vert.to_tuple() for vert in bbverts]
                    volume = bm.calc_volume()
                    area = {o:sum([f.area for f in o.data.polygons])}
                    me.update()
                    bm.clear()
                    
                    
                    vol_list.append(volume)
                    
                    # Progress Update Print Statements
                    print (o.name)
                    print (volume)
                    print (BBVolume)
                    
                    # Sorting by Volume of Mesh
                    Input = vol_list
                    Output = sorted(Input, key = lambda x:float(x))
                    
                    # Sorting by voume of bounding box
                    Input = BBList
                    SortedBBList = sorted(Input, key = lambda x:float(x))
                
                # More Progress Update Print Statements
                print (Output)
                print (SortedBBList)
                
                # Making sure each imported x3d has 5 mesh objects
                #    1. Cell Surface
                #    2. ER Surface
                #    3. ER Nodes
                #    4. Mitochondria Surface
                #    5. Mitochondria Nodes
                if len(mesh_obs) == 5:
                    for o in mesh_obs:
                        # Recomputing Bounding Boxes, COULD BE OMITTED?
                        me = o.data
                        bm = bmesh.new()
                        bm.from_mesh(me)
                        bm.to_mesh(me)
#                        H = bpy.data.objects[o.name].dimensions.x
#                        L = bpy.data.objects[o.name].dimensions.y
#                        W = bpy.data.objects[o.name].dimensions.z
#                        BBVolume = H * L * W
                        volume = bm.calc_volume()
                        
                        # Output list is sorted Cell Volumes
                        # If the volume of mesh object is equal to the LARGEST mesh volume, IT IS A CELL!
                        if volume == Output [-1]:
                            # Label object as 'Cell'
                            ob = bpy.context.scene.objects[o.name]    
                            bpy.ops.object.select_all(action='DESELECT')
                            bpy.context.view_layer.objects.active = ob  
                            ob.select_set(True) 
                            ob.name = 'Cell'
                            # And on the 8th day, god created cells
                            print ('Cell was created')
                    
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
                        bm = bmesh.new()
                        bm.from_mesh(me)
                        bm.to_mesh(me)
                        
                        # Volume of the Cell
                        VolCell = bm.calc_volume()
                        
                        # Creating blender mesh of mesh object
                        me = o.data
                        bm = bmesh.new()
                        bm.from_mesh(me)
                        bm.to_mesh(me)
                        
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
                        
                        # Checking if bounding box of mesh object is equal to bounding box of cell (while avoiding floating point error)
                        # Also the volume of the object must be smaller than volume of the cell
                        if round(BBVolume, 0) == round(BBCell, 0) and volume < VolCell:
                            # Progress Update print statement
                            print ('Volume:', volume)
                            
                            # When there are a lot of ER Nodes or poor ER signal,
                            # Comparing BB of object and BB of cell and ensuring volume of object < volume of cell is not sufficient
                            # To account for this edge case:
                            # 1. make a copy of object
                            # 2. Select object and separate into loose components
                            # 3. Count number of loose components
                            # 4. If number of loose components < 100, it's definitely not ER nodes
                            ob = bpy.context.scene.objects[o.name]
                            ob.select_set(True)
                            ob.name = 'Copy'
                            bpy.ops.object.select_all(action='DESELECT')
                            objectToSelect = bpy.data.objects['Copy']
                            objectToSelect.select_set(True)    
                            bpy.context.view_layer.objects.active = objectToSelect
                            bpy.ops.mesh.separate(type='LOOSE')
                            objs = [obj for obj in bpy.context.scene.objects if obj.name.startswith("Copy")]
                            n = len(objs)
                            if n <= 200:
                                ob.name = 'ER'
                            
                            for o in objs:
                                bpy.ops.object.join()                
                                bpy.ops.object.select_all(action='DESELECT')
                                bpy.context.view_layer.objects.active = ob  
                                ob.select_set(True) 
                                
                            
                            # TO DO: Simplify this to n = len(objs)
#                            for o in objs:
#                                bm = bmesh.new()
#                                me = o.data
#                                bm.from_mesh(me)
#                                bm.to_mesh(me)
#                                volume = bm.calc_volume()
#                                MitoOut.append(volume)
#                                me.update()
#                                bm.clear()
#                                n=n+1
#                            print (n)
#                            print (MitoOut)
                            

                        # If the volume of bounding box of object is second smallest,
                        # it is likely to be the mitochondria
                        if BBVolume == SortedBBList[1]:
                            MitoOut= []
                            
                            # Progress Update print statement
                            print ('Volume:', volume)
                            
                            # When there are a lot of ER Nodes or poor ER signal,
                            # Comparing BB of object and BB of cell and ensuring volume of object < volume of cell is not sufficient
                            # To account for this edge case:
                            # 1. make a copy of object
                            # 2. Select object and separate into loose components
                            # 3. Count number of loose components
                            # 4. If number of loose components < 100, it's definitely not ER nodes
                            ob = bpy.context.scene.objects[o.name]
                            ob.select_set(True)
                            ob.name = 'Copy'
                            bpy.ops.object.select_all(action='DESELECT')
                            objectToSelect = bpy.data.objects['Copy']
                            objectToSelect.select_set(True)    
                            bpy.context.view_layer.objects.active = objectToSelect
                            bpy.ops.mesh.separate(type='LOOSE')
                            n=0
                            objs = [obj for obj in bpy.context.scene.objects if obj.name.startswith("Copy")]
                            for o in objs:
                                # Count number of mitochondrial fragments
                                # Add volume of mitochondrial fragement to list
                                bm = bmesh.new()
                                me = o.data
                                bm.from_mesh(me)
                                bm.to_mesh(me)
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
                                ob.select_set(True) 
                            if n <= 100:
                                ob.name = 'Mitochondria'
                                # Append mitochondrial fragment volume list
                                OtherOut.append(MitoOut)
                                
                        # Smallest bouding box by volume are the mitochondrial nodes
                        if BBVolume == SortedBBList [-5]:
                            
                            # Progress Update print statements
                            print ('Volume:', volume)
                            
                            ob = bpy.context.scene.objects[o.name]  
                            bpy.ops.object.select_all(action='DESELECT')
                            bpy.context.view_layer.objects.active = ob  
                            ob.select_set(True) 
                            ob.name = 'MitoNodes'
                        
                        # If only one thing is left unlabeled, its the ER nodes
                        obj = [obj for obj in bpy.context.scene.objects if obj.name.startswith("Shape_IndexedFaceSet")]  
                        if len(obj) == 1:
                            o = obj [0]
                            ob = bpy.context.scene.objects[o.name]  
                            bpy.ops.object.select_all(action='DESELECT')
                            bpy.context.view_layer.objects.active = ob  
                            ob.select_set(True) 
                            ob.name = 'ERNodes'
                        
                        obj = [obj for obj in bpy.context.scene.objects if obj.name.startswith("Copy")]  
                        if len(obj) == 1:
                            o = obj [0]
                            ob = bpy.context.scene.objects[o.name]  
                            bpy.ops.object.select_all(action='DESELECT')
                            bpy.context.view_layer.objects.active = ob  
                            ob.select_set(True) 
                            ob.name = 'ERNodes'
                            
                        me.update()
                        bm.clear()
                        
                    # Everything is labelled now!
                    
                    meshes = [o for o in context.scene.objects if o.type == 'MESH']
                    for o in meshes : 
                        #Get bounding box dimensions
                        H = bpy.data.objects[o.name].dimensions.x
                        L = bpy.data.objects[o.name].dimensions.y
                        W = bpy.data.objects[o.name].dimensions.z
                        
                        ob = bpy.context.scene.objects[o.name]  
                        bpy.ops.object.select_all(action='DESELECT')
                        bpy.context.view_layer.objects.active = ob  
                        ob.select_set(True) 
                        bpy.ops.object.origin_set(type='ORIGIN_CENTER_OF_VOLUME')
                        print (ob.location) 
                        location = ob.location
                        
                        me = o.data
                        
                        # Creating to blender mesh
                        bm = bmesh.new()
                        bm.from_mesh(me)
                        bm.to_mesh(me)
                        
                        # Determining blender vertices
                        bbverts = [o.matrix_world @ Vector(bbvert) for bbvert in o.bound_box]
                        
                        # Possible backwards comptability for Blender v < 2.8
                        #bbverts = [o.matrix_world * Vector(bbvert) for bbvert in o.bound_box]
                        
                        plain_verts = [vert.to_tuple() for vert in bbverts]
                        volume = bm.calc_volume()
                        area = {o:sum([f.area for f in o.data.polygons])}
                        me.update()
                        bm.clear()
                        
                        
                        # appending info
                        out_list.append(area)
                        out_list.append(volume)
                        out_list.append(H)
                        out_list.append(L)
                        out_list.append(W)
                        out_list.append(location) 
                        out_list.append(plain_verts)
                    
                    
                    
                    
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
                    
                    # this creates a 4 x 4 x 4 box
                    n = 4
                    
                    # create a box with the same rotation and center point and dimensions as the cell
                    bpy.ops.mesh.primitive_cube_add(size=2, enter_editmode=False, 
                        location=(global_bbox_center), 
                        scale=(bpy.data.objects['Cell'].dimensions), 
                        rotation=(bpy.data.objects['Cell'].rotation_euler))
                    cube = bpy.context.object
                    
                    # Name the cube Cube
                    cube.name = 'Cube'
                    
                    
                    context = bpy.context
                    ob = context.object
                    
                    # Progress update print statement
                    print (ob.dimensions)
                    
                    size = 4 * max(ob.dimensions)
                    mw = ob.matrix_world
                    
                    # Defining the vertices of bounding box as vectors
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

                    o, x, y, z = bbox_axes(ob)        
                    
                    # Adds a face centered on bounding box center that has 4X the dimensions of the bounding box
                    bpy.ops.mesh.primitive_plane_add(
                            location=mw @ bbox_center(ob),
                            size=size)
                            
                    # converting plane to cube with volume
                    # chopper is now a cubic bounding box that's huge (4x bigger than largest BB dimension)
                    # IS THIS ACTUALLY A CUBE?? yes it is a cube
                    # Check what is 'size'??
                    chopper = context.object
                    m = chopper.modifiers.new("Sol", type='SOLIDIFY')
                    
                    #This converts the plane into a cube that bisects the bounding box
                    m.thickness = size
                    chopper.select_set(False)
                    
                    # The main idea behind this, even though we don't yet understand
                    # It is meant to make a box and divide it into n x n x n
                    # It does this by taking the difference between the chopper box and the bounding box at user defined segments
                    def chop(ob, start, end, segments):
                        slices = []
                        planes = [(f, start.lerp(end, f / segments)) 
                                for f in   range(1, segments)]

                        for i, p in planes:
                            #Inverting the size creates a cube the bisects the other half of the bounding box
                            m.thickness = -size
                            bm = ob.modifiers.new("BOOL",type="BOOLEAN")
                            bm.object = chopper
                            bm.operation = 'DIFFERENCE'
                            M = (mw @ end - mw @ start).to_track_quat('Z', 'X').to_matrix().to_4x4()
                            M.translation = mw @ p

                            chopper.matrix_world = M
                            cp = ob.copy()
                            cp.data = cp.data.copy()
                            context.scene.collection.objects.link(cp)
                            bpy.ops.object.modifier_apply({"object" : cp}, modifier="BOOL")
                            slices.append(cp)
                            m.thickness = size
                            bpy.ops.object.modifier_apply(
                                    {"object" : ob}, modifier = 'BOOL')
                        slices.append(ob)
                        return slices

                    segments_x = n
                    segments_y = n
                    segments_z = n
                    
                    # divide up the box we named Cube into n x n x n smaller boxes
                    for ox in chop(ob, o, x, segments_x):
                        for oy in chop(ox, o, y, segments_y):
                            chop(oy, o, z, segments_z)
                    
                    # Get rid of chopper object
                    bpy.data.objects.remove(chopper)
                    
                    
                    
                    
                    
                    # Collates all the smaller cubes we made
                    objs = [obj for obj in bpy.context.scene.objects if obj.name.startswith("Cube")]
                    
                    # Extract out the objects labelled Mitochondria
                    objects = bpy.data.objects   
                    Cell = objects['Cell']
                    Mitochondria = objects['Mitochondria']
                    bm = bmesh.new()

                    # Iterating through small cube array
                    for o in objs:
                        # Naming everything
                        # Getting vertices from the cube
                        CubeName = o.name
                        Cube = objects[o.name]
                        bbverts = [o.matrix_world @ Vector(bbvert) for bbvert in o.bound_box]
                        plain_verts = [vert.to_tuple() for vert in bbverts]
                        bool = Cube.modifiers.new(type="BOOLEAN", name="MitoDist")
                        bool.object = Mitochondria
                        
                        # Getting volume of intersection between cube and mitochondria
                        bool.operation = 'INTERSECT'
                        bpy.ops.object.modifier_apply(
                                    {"object": Cube},
                                    modifier=bool.name)

                        # Add triangulation for volume
                        def triangulate_object(obj):
                            me = obj.data
                            # Get a BMesh representation
                            bm = bmesh.new()
                            bm.from_mesh(me)

                            bmesh.ops.triangulate(bm, faces=bm.faces[:])
                            area = sum(f.calc_area() for f in bm.faces)
                            volume = bm.calc_volume()
                            print (o.name)
                            DistData.append (o.name)
                            print(area)
                            DistData.append (area)
                            print (volume)
                            DistData.append (volume)
                            print (plain_verts)
                            DistData.append (plain_verts)
                                # Finish up, write the bmesh back to the mesh
                            bm.to_mesh(me)
                            bm.free()
                        
                        # Create triangulated mesh
                        triangulate_object(objects[o.name])
                        
                        # Remove the cube
                        bpy.data.objects.remove(bpy.context.scene.objects[CubeName], do_unlink = True)
                        
                    
                    ###Repeat for difference between cube and Mito
                    # create a box with the same rotation and center point and dimensions as the cell
                    bpy.ops.mesh.primitive_cube_add(size=2, enter_editmode=False, 
                    location=(global_bbox_center), 
                    scale=(bpy.data.objects['Cell'].dimensions), 
                    rotation=(bpy.data.objects['Cell'].rotation_euler))
                    cube = bpy.context.object
                    
                    # Name the cube Cube
                    cube.name = 'Cube'
                    
                    
                    context = bpy.context
                    ob = context.object
                    
                    # Progress update print statement
                    print (ob.dimensions)
                    
                    size = 4 * max(ob.dimensions)
                    mw = ob.matrix_world
                    
                    # Defining the vertices of bounding box as vectors
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

                    o, x, y, z = bbox_axes(ob)        
                    
                    # Adds a face centered on bounding box center that has 4X the dimensions of the bounding box
                    bpy.ops.mesh.primitive_plane_add(
                            location=mw @ bbox_center(ob),
                            size=size)
                            
                    # converting plane to cube with volume
                    # chopper is now a cubic bounding box that's huge (4x bigger than largest BB dimension)
                    # IS THIS ACTUALLY A CUBE?? yes it is a cube
                    # Check what is 'size'??
                    chopper = context.object
                    m = chopper.modifiers.new("Sol", type='SOLIDIFY')
                    
                    #This converts the plane into a cube that bisects the bounding box
                    m.thickness = size
                    chopper.select_set(False)
                    
                    # The main idea behind this, even though we don't yet understand
                    # It is meant to make a box and divide it into n x n x n
                    # It does this by taking the difference between the chopper box and the bounding box at user defined segments
                    def chop(ob, start, end, segments):
                        slices = []
                        planes = [(f, start.lerp(end, f / segments)) 
                                for f in   range(1, segments)]

                        for i, p in planes:
                            #Inverting the size creates a cube the bisects the other half of the bounding box
                            m.thickness = -size
                            bm = ob.modifiers.new("BOOL",type="BOOLEAN")
                            bm.object = chopper
                            bm.operation = 'DIFFERENCE'
                            M = (mw @ end - mw @ start).to_track_quat('Z', 'X').to_matrix().to_4x4()
                            M.translation = mw @ p

                            chopper.matrix_world = M
                            cp = ob.copy()
                            cp.data = cp.data.copy()
                            context.scene.collection.objects.link(cp)
                            bpy.ops.object.modifier_apply({"object" : cp}, modifier="BOOL")
                            slices.append(cp)
                            m.thickness = size
                            bpy.ops.object.modifier_apply(
                                    {"object" : ob}, modifier = 'BOOL')
                        slices.append(ob)
                        return slices

                    segments_x = n
                    segments_y = n
                    segments_z = n
                    
                    # divide up the box we named Cube into n x n x n smaller boxes
                    for ox in chop(ob, o, x, segments_x):
                        for oy in chop(ox, o, y, segments_y):
                            chop(oy, o, z, segments_z)
                    
                    # Get rid of chopper object
                    bpy.data.objects.remove(chopper)
                    
                    
                    
                    
                    
                    # Collates all the smaller cubes we made
                    objs = [obj for obj in bpy.context.scene.objects if obj.name.startswith("Cube")]
                    
                    # Extract out the objects labelled Mitochondria
                    objects = bpy.data.objects   
                    Cell = objects['Cell']
                    Mitochondria = objects['Mitochondria']
                    bm = bmesh.new()

                    # Iterating through small cube array
                    for o in objs:
                        # Naming everything
                        # Getting vertices from the cube
                        CubeName = o.name
                        Cube = objects[o.name]
                        bbverts = [o.matrix_world @ Vector(bbvert) for bbvert in o.bound_box]
                        plain_verts = [vert.to_tuple() for vert in bbverts]
                        bool = Cube.modifiers.new(type="BOOLEAN", name="MitoDist")
                        bool.object = Mitochondria
                        
                        print (o.name)
                        ContactDistData.append (o.name)
                        
                        me = o.data
                        # Get a BMesh representation
                        bm = bmesh.new()
                        bm.from_mesh(me)

                        bmesh.ops.triangulate(bm, faces=bm.faces[:])
                        cubearea = sum(f.calc_area() for f in bm.faces)
                    
            
                        # Getting volume of intersection between cube and mitochondria
                        bool.operation = 'DIFFERENCE'
                        bpy.ops.object.modifier_apply(
                                    {"object": Cube},
                                    modifier=bool.name)

                        # Add triangulation for volume
                        def triangulate_object(obj):
                            me = obj.data
                            # Get a BMesh representation
                            bm = bmesh.new()
                            bm.from_mesh(me)

                            bmesh.ops.triangulate(bm, faces=bm.faces[:])
                            area = sum(f.calc_area() for f in bm.faces)
                            volume = bm.calc_volume()
                           
                            print(cubearea)
                            ContactDistData.append (cubearea)
                            print(area)
                            ContactDistData.append (area)
                            print (plain_verts)
                            ContactDistData.append (plain_verts)
                                # Finish up, write the bmesh back to the mesh
                            bm.to_mesh(me)
                            bm.free()
                        
                        # Create triangulated mesh
                        triangulate_object(objects[o.name])
                        
                        # Remove the cube
                        bpy.data.objects.remove(bpy.context.scene.objects[CubeName], do_unlink = True)
                    
                    
                    ###SAME PROCEDURE FOR CELL DISTRIBUTION
                    # create a box with the same rotation and center point and dimensions as the cell
                    bpy.ops.mesh.primitive_cube_add(size=2, enter_editmode=False, 
                        location=(global_bbox_center), 
                        scale=(bpy.data.objects['Cell'].dimensions), 
                        rotation=(bpy.data.objects['Cell'].rotation_euler))
                    cube = bpy.context.object
                    
                    # Name the cube Cube
                    cube.name = 'Cube'
                    
                    
                    context = bpy.context
                    ob = context.object
                    
                    # Progress update print statement
                    print (ob.dimensions)
                    
                    size = 4 * max(ob.dimensions)
                    mw = ob.matrix_world
                    
                    # Defining the vertices of bounding box as vectors
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

                    o, x, y, z = bbox_axes(ob)        
                    
                    # Adds a face centered on bounding box center that has 4X the dimensions of the bounding box
                    bpy.ops.mesh.primitive_plane_add(
                            location=mw @ bbox_center(ob),
                            size=size)
                            
                    # converting plane to cube with volume
                    # chopper is now a cubic bounding box that's huge (4x bigger than largest BB dimension)
                    # IS THIS ACTUALLY A CUBE?? yes it is a cube
                    # Check what is 'size'??
                    chopper = context.object
                    m = chopper.modifiers.new("Sol", type='SOLIDIFY')
                    
                    #This converts the plane into a cube that bisects the bounding box
                    m.thickness = size
                    chopper.select_set(False)
                    
                    # The main idea behind this, even though we don't yet understand
                    # It is meant to make a box and divide it into n x n x n
                    # It does this by taking the difference between the chopper box and the bounding box at user defined segments
                    def chop(ob, start, end, segments):
                        slices = []
                        planes = [(f, start.lerp(end, f / segments)) 
                                for f in   range(1, segments)]

                        for i, p in planes:
                            #Inverting the size creates a cube the bisects the other half of the bounding box
                            m.thickness = -size
                            bm = ob.modifiers.new("BOOL",type="BOOLEAN")
                            bm.object = chopper
                            bm.operation = 'DIFFERENCE'
                            M = (mw @ end - mw @ start).to_track_quat('Z', 'X').to_matrix().to_4x4()
                            M.translation = mw @ p

                            chopper.matrix_world = M
                            cp = ob.copy()
                            cp.data = cp.data.copy()
                            context.scene.collection.objects.link(cp)
                            bpy.ops.object.modifier_apply({"object" : cp}, modifier="BOOL")
                            slices.append(cp)
                            m.thickness = size
                            bpy.ops.object.modifier_apply(
                                    {"object" : ob}, modifier = 'BOOL')
                        slices.append(ob)
                        return slices

                    segments_x = n
                    segments_y = n
                    segments_z = n
                    
                    # divide up the box we named Cube into n x n x n smaller boxes
                    for ox in chop(ob, o, x, segments_x):
                        for oy in chop(ox, o, y, segments_y):
                            chop(oy, o, z, segments_z)
                    
                    # Get rid of chopper object
                    bpy.data.objects.remove(chopper)
                    
                    
                    
                    # Collates all the smaller cubes we made
                    objs = [obj for obj in bpy.context.scene.objects if obj.name.startswith("Cube")]
                    
                    # Extract out the objects labelled Mitochondria
                    objects = bpy.data.objects   
                    Cell = objects['Cell']
                    bm = bmesh.new()

                    # Iterating through small cube array
                    for o in objs:
                        # Naming everything
                        # Getting vertices from the cube
                        CubeName = o.name
                        Cube = objects[o.name]
                        bbverts = [o.matrix_world @ Vector(bbvert) for bbvert in o.bound_box]
                        plain_verts = [vert.to_tuple() for vert in bbverts]
                        bool = Cube.modifiers.new(type="BOOLEAN", name="CellDist")
                        bool.object = Cell
                        
                        # Getting volume of intersection between cube and mitochondria
                        bool.operation = 'INTERSECT'
                        bpy.ops.object.modifier_apply(
                                    {"object": Cube},
                                    modifier=bool.name)

                        # Add triangulation for volume
                        def triangulate_object(obj):
                            me = obj.data
                            # Get a BMesh representation
                            bm = bmesh.new()
                            bm.from_mesh(me)

                            bmesh.ops.triangulate(bm, faces=bm.faces[:])
                            area = sum(f.calc_area() for f in bm.faces)
                            volume = bm.calc_volume()
                            print (o.name)
                            CellDistData.append (o.name)
                            print(area)
                            CellDistData.append (area)
                            print (volume)
                            CellDistData.append (volume)
                            print (plain_verts)
                            CellDistData.append (plain_verts)
                                # Finish up, write the bmesh back to the mesh
                            bm.to_mesh(me)
                            bm.free()
                        
                        # Create triangulated mesh
                        triangulate_object(objects[o.name])
                        
                        # Remove the cube
                        bpy.data.objects.remove(bpy.context.scene.objects[CubeName], do_unlink = True)
                    
                    # We now want to get TOTAL surface area of mitochondria-ER contact and number of contacts along with the coordinates of their bounding box
                    
                    # we get select the mitochondria intersections
                    objects = bpy.data.objects   
                    context = bpy.context
                    Mitochondria = objects['Mitochondria']
                    ER = objects['ER']  

                    #Apply boolean modifier so that mitochondria now refers to the mito ER intersections
                    bool = Mitochondria.modifiers.new(type="BOOLEAN", name="Overlap")
                    bool.object = ER
                    bool.operation = 'INTERSECT'
                    bpy.ops.object.modifier_apply(
                                    {"object": Mitochondria},
                                    modifier=bool.name)

                    n=0
                    OverlapList = []
                    # split into individual intersections
                    mesh_obs = [o for o in bpy.context.scene.objects if o.name == 'Mitochondria']
                    print (mesh_obs)

                    for o in mesh_obs:
                        bpy.ops.object.select_all(action='DESELECT')
                        objectToSelect = bpy.data.objects['Mitochondria']
                        objectToSelect.select_set(True)  
                        print("Splitting mesh")
                        bpy.ops.mesh.separate(type='LOOSE')
                                                

                    objs = [obj for obj in bpy.context.scene.objects if obj.name.startswith("Mitochondria")]
                    print (objs)

                    # iterating over each intersection
                    for o in objs:
                        bm = bmesh.new()
                        # All the data, may be more than needed
                        bbverts = [o.matrix_world @ Vector(bbvert) for bbvert in o.bound_box]
                        me = o.data
                        bm.from_mesh(me)
                        bm.to_mesh(me)
                        plain_verts = [vert.to_tuple() for vert in bbverts]
                        volume = bm.calc_volume()
                        area = {o:sum([f.area for f in o.data.polygons])}
                        me.update()
                        bm.clear()
                        OverlapList.append(area)
                        OverlapList.append(volume)
                        OverlapList.append(plain_verts)
                        #counting number of iterations
                        n=n+1

                    print (OverlapList)
                    print ("Number or Mitochondrial-ER contact sites in this cell:")
                    print (n)
                    
                    
                    
                    
                # if there is NOT 5 mesh objects
                else:
                    print (SmallFileName +' is an incomplete file.')
                    # TO ADD: Compile names of filenames that failed to print at the very end
                    
                file = open(Desktop+'/MitER/Data/'+SmallFileName +'-CellStats.csv', 'w', newline='')
                with file:
                    write = csv.writer(file)
                    write.writerows([[output] for output in out_list])
                file = open(Desktop+'/MitER/Data/'+SmallFileName +'-MitoComponents.csv', 'w', newline='')
                with file:
                    write = csv.writer(file)
                    write.writerows([[output] for output in OtherOut])
                file = open(Desktop+'/MitER/Data/'+SmallFileName +'-Distribution.csv', 'w', newline='')
                with file:
                    write = csv.writer(file)
                    write.writerows([[output] for output in DistData])
                file = open(Desktop+'/MitER/Data/'+SmallFileName +'-CellDistribution.csv', 'w', newline='')
                with file:
                    write = csv.writer(file)
                    write.writerows([[output] for output in CellDistData])
                file = open(Desktop+'/MitER/Data/'+SmallFileName +'-ContactDistribution.csv', 'w', newline='')
                with file:
                    write = csv.writer(file)
                    write.writerows([[output] for output in ContactDistData])
                file = open(Desktop+'/MitER/Data/'+SmallFileName +'-Overlap.csv', 'w', newline='')
                with file:
                    write = csv.writer(file)
                    write.writerows([[output] for output in OverlapList])
                

        # TO ADD: Print list of filenames that failed
        return {'FINISHED'}



def register():
    bpy.utils.register_class(OT_TestOpenFilebrowser)


# what is this for?
def unregister():
    bpy.utils.unregister_class(OT_TestOpenFilebrowser)


# This is what Runs the program
register()
name = bpy.ops.test.open_filebrowser('INVOKE_DEFAULT')

