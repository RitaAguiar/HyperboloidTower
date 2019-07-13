from khepri.rhino import *

delete_all_shapes()

#--------------------------------#

#DEFINITIONS

p = u0() #tower base point
r = 0.1 #cylinders radius
rb = 5.3 #tower base radius
rt = 3 #tower top radius
d_fi = pi/2 #angle difference between top and base of cylinders
n_cyl = 12 #number of cylinders
h = 20  #tower height
h_slab = 0.3 #slab height
n_step = 19 #number of steps per floor
e_step = 0.2 #step thickness
h_floor = e_step * n_step #floor height
n_floors = floor((h - h_floor) / h_floor) #number of floors-platforms of stairs
h_rail = 1 #handrail height
r_rail = 0.01 #handrail radius
l = 3 #steps length
w = 0.6 #steps width
alfa = 0 #start angle
d_alfa = pi/18 #angle between steps

#LAYERS

slab_layer = create_layer('Slab')
stair_layer = create_layer('Stair')
column_layer = create_layer('Column')
structure_layer = create_layer('Structure')
platform_layer = create_layer('Platform')
posts_layer = create_layer('Posts')
person_layer = create_layer('Person')

#--------------------------------#

#FUNCTIONS

#structural slabs

def slab(p, r, h_slab):
    with current_layer(slab_layer):
        cylinder(p, r, p + vz(h_slab))

#structural cylindrical column

def central_column(p, w, h, h_slab):
    with current_layer(column_layer):
        cylinder(p, 2*w/3, p.z + (h - h_slab))

#tower cylinders

def hyperboloid(r, rb, rt, h, d_fi, n_cyl):
    def pair_cylinders(fi):
        with current_layer(structure_layer):
            cylinder(p + vpol(rb, fi), r, p + vcyl(rt , fi + d_fi, h))
            cylinder(p + vpol(rb, fi), r, p + vcyl(rt , fi - d_fi, h))
    map(pair_cylinders, division(0, 2*pi, n_cyl, False))

#stair steps with different dimensions

def spiral_stair(p, l, w, e_step, alfa, d_alfa, n_steps, n_floors, n_floor):
    def stair_rec(z_d, alfa_d, l_d):
        pt_i = p + vz(z_d)
        with current_layer(stair_layer):
            right_cuboid(pt_i, w, e_step, pt_i + vpol(l_d, alfa_d))
    map(stair_rec,
        division(e_step/2, e_step*(n_step - 0.5), n_step - 1),
        division(alfa, alfa + d_alfa * (n_step - 1), n_step - 1),
        division(l*(n_floors - n_floor + 2)/n_floors, l*(n_floors - n_floor + 1)/n_floors, n_step - 1)
        )

#stair platforms with different dimensions

def platform(p, l, e_step, h_slab, n_step, n_floor):
    h_floor = e_step * n_step
    l_plat = l*(n_floors - n_floor + 1)/ n_floors
    with current_layer(platform_layer):
        subtraction(
        cylinder(p + vz(h_floor), l_plat, p + vz(h_floor + h_slab)),
        box(p + vxz(- l_plat, h_floor), p + vxyz(l_plat, l_plat, h_floor + h_slab)))

#stairs handrail

def handrail(p, r, z, h_rail, n_floors, n_floor):
    def pt(ang, z, r_cir):
        with current_layer(posts_layer):
            cylinder(p + vcyl(r_cir, ang, z + e_step/2),
                r_rail,
                p + vcyl(r_cir, ang, z + h_rail))
            cylinder(p + vcyl(r_cir, ang, z + h_rail),
                    r_rail,
                    p + vcyl(r_cir, ang + d_alfa, z + h_rail))
    map(pt,
        division(alfa, n_step*d_alfa - d_alfa, n_step - 1),
        division(h_slab, z + h_slab - e_step, n_step - 1),
        division(l*(n_floors - n_floor + 2)/n_floors,
             l*(n_floors - n_floor + 1)/n_floors,
             n_step - 1))
    map(pt,
        division(n_step*d_alfa - d_alfa,
                 2*(n_step*d_alfa - d_alfa),
                 n_step - 1, False),
        division(h_floor + h_slab - e_step,
                 h_floor + h_slab - e_step,
                 n_step - 1, False),
        division(l*(n_floors - n_floor + 1)/n_floors,
                 l*(n_floors - n_floor + 1)/n_floors,
                 n_step - 1, False))

#last stair handrail

def top_handrail(p, ang, l_floor, h_rail, n_floors, n_floor):
    dist = 2*sin(d_alfa/2)
    l_rail = l_floor + h_slab - e_step/2
    l_t_rail = l_floor + h_rail + e_step/2
    def pt(r):
        with current_layer(posts_layer):
            cylinder(p + vcyl(r, ang, l_rail),
            r_rail,
            p + vcyl(r, ang, l_t_rail))
            cylinder(p + vcyl(r, ang, l_rail),
            r_rail,
            p + vcyl(r - l*(n_floors - n_floor + 2)/n_floors*dist, ang, l_t_rail))
    map(pt, division(0, l*(n_floors - n_floor + 2)/n_floors, round(1/dist)))

#stairs with platforms

def stair(p, l, w, e_step, h_slab, alfa, d_alfa, n_step, n_floors):
    h_floor = e_step*n_step
    def stair_rec(n_floor):
        spiral_stair(p + vz(h_slab + h_floor*n_floor), l, w, e_step, alfa, d_alfa, n_step, n_floors, n_floor)
    map(stair_rec,
    division(0, n_floors, n_floors, False))
    def platform_rec(n_floor):
        platform(p + vz(h_floor*n_floor), l, e_step, h_slab, n_step, n_floor)
    map(platform_rec,
    division(0, n_floors, n_floors, False))
    def rail_rec(n_floor):
        handrail(p + vz(h_floor*n_floor), l, h_floor, h_rail, n_floors, n_floor)
    map(rail_rec,
    division(0, n_floors, n_floors, False))
    top_handrail(p, alfa, n_floors*h_floor, h_rail, n_floors, n_floors)

#building generation

def hyperboloid_tower():
    h_floor = e_step*n_step
    slab(p, rb, h_slab)
    central_column(p, w, h, h_slab)
    stair(p, l, w, e_step, h_slab, alfa, d_alfa, n_step, n_floors)
    slab(p + vz(h - h_slab), rt, h_slab)
    hyperboloid(r, rb, rt, h, d_fi, n_cyl)

#--------------------------------#

#EXECUTIONS

hyperboloid_tower()
