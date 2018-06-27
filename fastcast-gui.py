#!/usr/bin/env python3

import argparse
import time
import sys
import copy

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
font = pygame.font.SysFont('DejaVu Sans Mono, monospace', 26, bold=True)
pygame.key.set_repeat(1, 0)

eye_orig = Eye(Vec3(x=0.0, y=0.0, z=0.0), Vec3(x=0.0, y=0.0, z=0.0))
eye = copy.deepcopy(eye_orig)
screen_view_dist = 800.0

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
        Sphere(center=Vec3(x=-600.0 + i * 13.0, y=-400.0 + i * 50.0, z=240.0 - i * 10.0),
               radius=20.0,
               color=RGB(r=0.9, g=0.1, b=0.8)))

def show_text(what, where):
    text = font.render(what, 1, (255, 255, 255))
    screen.blit(text, where)

def render():
    spheres = spheres_test0
    sphere_center_xs = numpy.fromiter(map(lambda s: s.center.x, spheres), dtype='float32')
    sphere_center_ys = numpy.fromiter(map(lambda s: s.center.y, spheres), dtype='float32')
    sphere_center_zs = numpy.fromiter(map(lambda s: s.center.z, spheres), dtype='float32')
    sphere_radiuses = numpy.fromiter(map(lambda s: s.radius, spheres), dtype='float32')
    sphere_color_rs = numpy.fromiter(map(lambda s: s.color.r, spheres), dtype='float32')
    sphere_color_gs = numpy.fromiter(map(lambda s: s.color.g, spheres), dtype='float32')
    sphere_color_bs = numpy.fromiter(map(lambda s: s.color.b, spheres), dtype='float32')

    start = time.time()
    frame = fastcast.main(size[0], size[1], screen_view_dist,
                          eye.position.x, eye.position.y, eye.position.z,
                          eye.orientation.x, eye.orientation.y, eye.orientation.z,
                          sphere_center_xs, sphere_center_ys, sphere_center_zs,
                          sphere_radiuses,
                          sphere_color_rs, sphere_color_gs, sphere_color_bs).get()
    end = time.time()
    pygame.surfarray.blit_array(surface, frame)
    screen.blit(surface, (0, 0))

    show_text(' Futhark call: {:.02f} ms'.format((end - start) * 1000),
              (10, 10))
    show_text('     Position: ({:.02f}, {:.02f}, {:.02f})'.format(
        eye.position.x, eye.position.y, eye.position.z),
              (10, 40))
    show_text('  Orientation: ({:.02f}, {:.02f}, {:.02f})'.format(
        eye.orientation.x, eye.orientation.y, eye.orientation.z),
              (10, 70))
    show_text('Draw distance: {:.02f}'.format(screen_view_dist),
              (10, 100))

    pygame.display.flip()

while True:
    spheres_test0[2].center.x += 0.3
    render()
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()
        elif event.type == pygame.KEYDOWN:
            if event.key == pygame.K_q:
                sys.exit()
            if event.key == pygame.K_RIGHT:
                eye.position.x += 1
            if event.key == pygame.K_LEFT:
                eye.position.x -= 1
            if event.key == pygame.K_UP:
                eye.position.y -= 1
            if event.key == pygame.K_DOWN:
                eye.position.y += 1
            if event.key == pygame.K_PAGEDOWN:
                eye.position.z -= 1
            if event.key == pygame.K_PAGEUP:
                eye.position.z += 1
            if event.key in [pygame.K_MINUS, pygame.K_KP_MINUS]:
                screen_view_dist -= 1
            if event.key in [pygame.K_PLUS, pygame.K_KP_PLUS]:
                screen_view_dist += 1
            if event.key == pygame.K_HOME:
                eye = copy.deepcopy(eye_orig)
