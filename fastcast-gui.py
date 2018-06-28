#!/usr/bin/env python3

import argparse
import time
import sys
import copy
import math

import numpy
import pygame

import fastcast


class Vec3:
    def __init__(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z

class Eye:
    def __init__(self, position, orientation):
        self.position = position
        self.orientation = orientation

class RGB:
    def __init__(self, r, g, b):
        self.r = r
        self.g = g
        self.b = b

class Sphere:
    def __init__(self, center, radius, color):
        self.center = center
        self.radius = radius
        self.color = color

class Light:
    def __init__(self, position, intensity):
        self.position = position
        self.intensity = intensity

def rotate_point(angle, origo, point):
    (x0, y0, z0) = (point.x - origo.x, point.y - origo.y, point.z - origo.z)

    (sin_x, cos_x) = (math.sin(angle.x), math.cos(angle.x))
    (sin_y, cos_y) = (math.sin(angle.y), math.cos(angle.y))
    (sin_z, cos_z) = (math.sin(angle.z), math.cos(angle.z))

    # X axis.
    (x1, y1, z1) = (x0,
                    y0 * cos_x - z0 * sin_x,
                    y0 * sin_x + z0 * cos_x)
    # Y axis.
    (x2, y2, z2) = (z1 * sin_y + x1 * cos_y,
                    y1,
                    z1 * cos_y - x1 * sin_y)
    # Z axis.
    (x3, y3, z3) = (x2 * cos_z - y2 * sin_z,
                    x2 * sin_z + y2 * cos_z,
                    z2)

    (xp, yp, zp) = (origo.x + x3, origo.y + y3, origo.z + z3)
    return Vec3(xp, yp, zp)

_size = lambda s: tuple(map(int, s.split('x')))
arg_parser = argparse.ArgumentParser(description='use arrow keys')
arg_parser.add_argument('--size', type=_size, metavar='WIDTHxHEIGHT',
                        help='set the size of the window')
args = arg_parser.parse_args()
size = args.size or (800, 600)

fastcast = fastcast.fastcast(interactive=True)

pygame.init()
pygame.display.set_caption('fastcast')
screen = pygame.display.set_mode(size)
surface = pygame.Surface(size, depth=32)
font = pygame.font.SysFont('DejaVu Sans Mono, monospace', 18, bold=True)

eye_orig = Eye(Vec3(x=0.0, y=0.0, z=0.0), Vec3(x=0.0, y=0.0, z=0.0))
eye = copy.deepcopy(eye_orig)
screen_view_dist = 800.0
show_stats = True

spheres_test0 = [
    Sphere(center=Vec3(x=100.0, y=100.0, z=240.0),
           radius=120.0,
           color=RGB(r=0.9, g=0.1, b=0.8)),
    Sphere(center=Vec3(x=150.0, y=200.0, z=260.0),
           radius=100.0,
           color=RGB(r=1.0, g=0.0, b=0.0)),
    Sphere(center=Vec3(x=-150.0, y=-200.0, z=160.0),
           radius=100.0,
           color=RGB(r=0.1, g=0.2, b=0.9)),
]
for i in range(50):
    spheres_test0.append(
        Sphere(center=Vec3(x=200.0 + math.tan(i) * 53.0, y=-200.0 + math.sin(i) * 50.0, z=240.0 - i * 2.0),
               radius=20.0 * math.sqrt(abs(math.tan(i))),
               color=RGB(r=abs(math.tan(i)), g=0.8, b=abs(math.cos(i)))))

lights_test0 = [
    Light(position=Vec3(x=300, y=200, z=50), intensity=80000),
    Light(position=Vec3(x=-400, y=-100, z=150), intensity=20000),
]

def show_text(what, where):
    text = font.render(what, 1, (255, 255, 255))
    screen.blit(text, where)

def render():
    spheres = spheres_test0
    lights = lights_test0

    sphere_center_xs = numpy.fromiter(map(lambda s: s.center.x, spheres), dtype='float32')
    sphere_center_ys = numpy.fromiter(map(lambda s: s.center.y, spheres), dtype='float32')
    sphere_center_zs = numpy.fromiter(map(lambda s: s.center.z, spheres), dtype='float32')
    sphere_radiuses = numpy.fromiter(map(lambda s: s.radius, spheres), dtype='float32')
    sphere_color_rs = numpy.fromiter(map(lambda s: s.color.r, spheres), dtype='float32')
    sphere_color_gs = numpy.fromiter(map(lambda s: s.color.g, spheres), dtype='float32')
    sphere_color_bs = numpy.fromiter(map(lambda s: s.color.b, spheres), dtype='float32')

    light_position_xs = numpy.fromiter(map(lambda l: l.position.x, lights), dtype='float32')
    light_position_ys = numpy.fromiter(map(lambda l: l.position.y, lights), dtype='float32')
    light_position_zs = numpy.fromiter(map(lambda l: l.position.z, lights), dtype='float32')
    light_intensities = numpy.fromiter(map(lambda l: l.intensity, lights), dtype='float32')

    # ori_x = math.cos(eye.orientation.x)
    # ori_y = 0
    # ori_z = math.sin(eye.orientation.x)
    
    start = time.time()
    frame = fastcast.main(size[0], size[1], screen_view_dist,
                          eye.position.x, eye.position.y, eye.position.z,
                          eye.orientation.x, eye.orientation.y, eye.orientation.z,
                          # ori_x, ori_y, ori_z,
                          sphere_center_xs, sphere_center_ys, sphere_center_zs,
                          sphere_radiuses,
                          sphere_color_rs, sphere_color_gs, sphere_color_bs,
                          light_position_xs, light_position_ys, light_position_zs,
                          light_intensities).get()
    end = time.time()
    pygame.surfarray.blit_array(surface, frame)
    screen.blit(surface, (0, 0))

    texts = []
    texts.append('(Press h to hide/show stats)')
    if show_stats:
        texts.extend([
            ' Futhark call: {:.02f} ms'.format((end - start) * 1000),

            '      Spheres: {}'.format(len(spheres_test0)),

            '     Position: ({:.02f}, {:.02f}, {:.02f})'.format(
                eye.position.x, eye.position.y, eye.position.z),

            '  Orientation: ({:.02f}, {:.02f}, {:.02f})'.format(
                eye.orientation.x, eye.orientation.y, eye.orientation.z),

            '  Screen view: {:.02f}'.format(screen_view_dist),
        ])
    for text, i in zip(texts, range(len(texts))):
        show_text(text, (10, 10 + i * 20))

    pygame.display.flip()

keydown = {}
while True:
    spheres_test0[2].center.x += 0.3
    lights_test0[0].position.x -= 0.3
    lights_test0[0].position.y -= 0.2

    render()
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()
        elif event.type == pygame.KEYDOWN:
            if event.key == pygame.K_h:
                show_stats = not show_stats
            elif event.key == pygame.K_HOME:
                eye = copy.deepcopy(eye_orig)
            elif event.key == pygame.K_q:
                sys.exit()
            else:
                keydown[event.key] = True
        elif event.type == pygame.KEYUP:
            try:
                del keydown[event.key]
            except KeyError:
                pass

    if keydown.get(pygame.K_DOWN):
        p = copy.deepcopy(eye.position)
        p.z -= 5
        eye.position = rotate_point(eye.orientation, eye.position, p)
    if keydown.get(pygame.K_UP):
        p = copy.deepcopy(eye.position)
        p.z += 5
        eye.position = rotate_point(eye.orientation, eye.position, p)
    if keydown.get(pygame.K_LEFT):
        eye.orientation.y -= 0.02
    if keydown.get(pygame.K_RIGHT):
        eye.orientation.y += 0.02
    if keydown.get(pygame.K_PAGEDOWN):
        eye.position.y += 5
    if keydown.get(pygame.K_PAGEUP):
        eye.position.y -= 5
    if keydown.get(pygame.K_MINUS) or keydown.get(pygame.K_KP_MINUS):
        screen_view_dist += 5
    if keydown.get(pygame.K_PLUS) or keydown.get(pygame.K_KP_PLUS):
        screen_view_dist -= 5
