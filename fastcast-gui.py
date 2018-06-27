#!/usr/bin/env python

import fastcast

import numpy
import pygame
import time
import sys

fastcast = fastcast.fastcast(interactive=True)
size = (800, 600)

pygame.init()
pygame.display.set_caption('fastcast')
screen = pygame.display.set_mode(size)
surface = pygame.Surface(size, depth=32)
font = pygame.font.Font(None, 36)
pygame.key.set_repeat(1, 0)

def showText(what, where):
    text = font.render(what, 1, (255, 255, 255))
    screen.blit(text, where)

def render():
    start = time.time()
    frame = fastcast.main().get()
    end = time.time()
    pygame.surfarray.blit_array(surface, frame)
    screen.blit(surface, (0, 0))

    speedmessage = "Futhark call took %.2fms" % ((end-start)*1000,)
    showText(speedmessage, (10, 10))
    print(speedmessage)
    # locmessage = ("Position: (%.2f, %.2f, %.2f) Orientation: (%.2f, %.2f, %.2f)" %
    #               (eye['point']['x'], eye['point']['y'], eye['point']['z'],
    #                eye['vector']['x'], eye['vector']['y'], eye['vector']['z']))
    # showText(locmessage, (10, 40))

    pygame.display.flip()

try:
    while True: #for i in range(10):
        render()
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                sys.exit()
except KeyboardInterrupt:
    sys.exit()
    
        # elif event.type == pygame.KEYDOWN:
        #     if event.key == pygame.K_RIGHT:
        #         eye['point']['x'] += 1
        #     if event.key == pygame.K_LEFT:
        #         eye['point']['x'] -= 1
        #     if event.key == pygame.K_UP:
        #         eye['point']['y'] -= 1
        #     if event.key == pygame.K_DOWN:
        #         eye['point']['y'] += 1
        #     if event.key == pygame.K_PAGEDOWN:
        #         eye['point']['z'] -= 1
        #     if event.key == pygame.K_PAGEUP:
        #         eye['point']['z'] += 1
        #     if event.key == pygame.K_HOME:
        #         eye = orig_eye
        #     if event.unicode == 'q' and not changed_bounce_limit:
        #         bouncelimit = max(bouncelimit-1,1)
        #         changed_bounce_limit = True
        #     if event.unicode == 'w' and not changed_bounce_limit:
        #         bouncelimit += 1
        #         changed_bounce_limit = True
        # elif event.type == pygame.KEYUP:
        #     if event.key in [pygame.K_q, pygame.K_w]:
        #         changed_bounce_limit = False
