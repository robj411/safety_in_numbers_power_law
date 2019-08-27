

import random
import time
import numpy

import pygame
from pygame.locals import *

class Common(object):
    def __init__(self,x,WINDOW_WIDTH,WINDOW_HEIGHT,STEP_SIZE):
        self.x = x
        self.lastmove = 0
        self.WINDOW_WIDTH = WINDOW_WIDTH
        self.WINDOW_HEIGHT = WINDOW_HEIGHT
        self.STEP_SIZE = STEP_SIZE
        self.x = random.randint(0,WINDOW_WIDTH-STEP_SIZE)
        self.y = random.randint(0,WINDOW_HEIGHT-STEP_SIZE)
    
    def moveDown(self):
        self.direction = 0
    def moveLeft(self):
        self.direction = 1
    def moveUp(self):
        self.direction = 2
    def moveRight(self):
        self.direction = 3

    # Function sets the random mechanism for the ghost to move
    def random(self):
	persistrv = random.randint(0,5)
        #leftboundary = (self.lastmove==0 and self.y==WINDOW_HEIGHT - STEP_SIZE)
        #rightboundary = (self.lastmove==1 and self.x==0)
        #bottomboundary = (self.lastmove==2 and self.y==0)
        #topboundary = (self.lastmove==3 and self.x==WINDOW_WIDTH - STEP_SIZE)
        if persistrv < 5:# and not leftboundary and not rightboundary and not bottomboundary and not topboundary:
            self.direction = self.lastmove
        else:
            options = [0,1,2,3]
            #if self.x == WINDOW_WIDTH - STEP_SIZE:
            #   options.pop(3)
            #if self.y == 0:
            #    options.pop(2)
            #if self.x == 0:
            #    options.pop(1)
            #if self.y == WINDOW_HEIGHT - STEP_SIZE:
            #    options.pop(0)
            randomnumber = numpy.random.choice(options, size=1)

            if randomnumber==0:
                self.moveDown()
            if randomnumber==1:
                self.moveLeft()
            if randomnumber==2:
                self.moveUp()
            if randomnumber==3:
                self.moveRight()

    # Function draws the icon
    def draw(self, surface):
        pygame.draw.rect(surface, self.colour, [self.x, self.y, self.STEP_SIZE, self.STEP_SIZE], 0)

    # Function for movement
    def update(self):

        self.updateCount = self.updateCount + 1
        if self.updateCount > self.updateCountMax:

            # Updates the position of the ghost
            if self.direction == 0: # Down
                if self.y + self.STEP_SIZE >= self.WINDOW_HEIGHT:
                    self.y = 0#WINDOW_HEIGHT - STEP_SIZE
                else:
                    self.y = self.y + self.STEP_SIZE
            if self.direction == 1: # Left
                if self.x - self.STEP_SIZE < 0:
                    self.x = self.WINDOW_WIDTH - self.STEP_SIZE#0
                else:
                    self.x = self.x - self.STEP_SIZE
            if self.direction == 2: # Up
                if self.y - self.STEP_SIZE < 0:
                    self.y = self.WINDOW_HEIGHT - self.STEP_SIZE#0
                else:
                    self.y = self.y - self.STEP_SIZE

            if self.direction == 3: # Right
                if self.x + self.STEP_SIZE >= self.WINDOW_WIDTH:
                    self.x = 0#WINDOW_WIDTH - STEP_SIZE
                else:
                    self.x = self.x + self.STEP_SIZE

            self.updateCount = 0
            self.lastmove = self.direction

# Defines the class for the bike (pacman)
class Bike(Common):
    x = 0
    y = 0
    direction = 0

    updateCountMax = 2
    updateCount = 0
    colour = (255,0,255)

    def __init__(self,WINDOW_WIDTH,WINDOW_HEIGHT,STEP_SIZE):
        Common.__init__(self,"Bike",WINDOW_WIDTH,WINDOW_HEIGHT,STEP_SIZE)

# Defines the class for the ghost
class Motor(Common):
    x = 0
    y = 0
    direction = 0

    updateCountMax = 4
    updateCount = 0
    colour = (255,255,0)

    def __init__(self,WINDOW_WIDTH,WINDOW_HEIGHT,STEP_SIZE):
        Common.__init__(self,"Motor",WINDOW_WIDTH,WINDOW_HEIGHT,STEP_SIZE)


# Defines the class for the Game
class Game:

    # Function determines whether two objects have collided by comparing where the origin corners of
    # the shape are and the total size of each object 
    def isCollision(self, x1, y1, x2, y2, bsize):
        if x1 < x2 + bsize and x1 + bsize > x2 and y1 < y2 + bsize and y1 + bsize > y2:
                return True
        return False

# Defines the class app the controls all the logic and game function
class App(object):
    count = 0

    def __init__(self,WINDOW_WIDTH,WINDOW_HEIGHT,STEP_SIZE,PROBABILITY,cyclists,motorists,showwindow):
        self._running = True
        self._display_surf = None
        self._image_surf = None
        self._pacman_surf = None
        self.windowWidth = WINDOW_WIDTH
        self.windowHeight = WINDOW_HEIGHT
        self.STEP_SIZE = STEP_SIZE
        self.PROBABILITY = PROBABILITY
        self.showwindow = showwindow
        #if showwindow:
        #    import pygame
        #    from pygame.locals import *

        self.game = Game()
        self.bike = []
        for i in range(cyclists):
            self.bike.append(Bike(WINDOW_WIDTH,WINDOW_HEIGHT,STEP_SIZE))
        self.motor = []
        for i in range(motorists):
            self.motor.append(Motor(WINDOW_WIDTH,WINDOW_HEIGHT,STEP_SIZE))

    # Function initializes all imported pygame modules, sets screen size and caption, and loads image resources
    def on_init(self):
        pygame.init()
        self._display_surf = pygame.display.set_mode((self.windowWidth, self.windowHeight), pygame.HWSURFACE)

        pygame.display.set_caption('Collision simulation')
        self._running = True
        #picture = pygame.image.load("ghost.png")
        #picture = pygame.transform.scale(picture, (self.STEP_SIZE, self.STEP_SIZE))
        #self._image_surf = picture.convert()
        #picture = pygame.image.load("pacman.png")
        #picture = pygame.transform.scale(picture, (self.STEP_SIZE, self.STEP_SIZE))
        #self._pacman_surf = picture.convert()
        #rect = picture.get_rect()

    # Function determines the game logic for how things move
    def on_loop(self):
        for i in range(len(self.bike)):
            self.bike[i].random()
        for i in range(len(self.motor)):
            self.motor[i].random()
        for i in range(len(self.bike)):
            self.bike[i].update()
        for i in range(len(self.motor)):
            self.motor[i].update()

        # Determines collision happened.
        for i in range(len(self.bike)):
            for j in range(len(self.motor)):
                if self.game.isCollision(self.bike[i].x, self.bike[i].y, self.motor[j].x, self.motor[j].y, self.STEP_SIZE):
                    if(numpy.random.choice(range(10), size=1) < 10*pow(len(self.bike),self.PROBABILITY-1)):
                        self.count += 1     # Increase count by one
                    self.bike[i] = Bike(self.windowWidth,self.windowHeight,self.STEP_SIZE)
        pass

    # Function draws the game components to screen
    def on_render(self):
        self._display_surf.fill((0, 0, 0))                          # Creates a black canvas
        for i in range(len(self.bike)):
            self.bike[i].draw(self._display_surf)     # Draws bikes
        for i in range(len(self.motor)):
            self.motor[i].draw(self._display_surf)    # Draws motorists
        myfont = pygame.font.SysFont('arial', 20)
        displaytext = "Count: " + str(self.count)
        textsurface = myfont.render(displaytext, False, (255, 255, 255))
        self._display_surf.blit(textsurface,(0,0))
        pygame.display.flip()                                       # Updates the display surface to screen

    # Function uninitializes all pygame modules
    def on_cleanup(self):
        pygame.quit()

    def on_execute(self):
        if self.showwindow == True:
            if self.on_init() == False:
                self._running = False
        
        iterations = 0;
        while (self._running and iterations < 500):
            iterations += 1
            if self.showwindow == True:
                pygame.event.pump()

            self.on_loop()
            if self.showwindow == True:
                self.on_render()

            #time.sleep(10.0 / 1000.0);
        if self.showwindow == True:
            self.on_cleanup()


def main_function(WINDOW_WIDTH,WINDOW_HEIGHT,STEP_SIZE,PROBABILITY,cyclists,motorists,showwindow):
    theApp = App(WINDOW_WIDTH,WINDOW_HEIGHT,STEP_SIZE,PROBABILITY,cyclists,motorists,showwindow)
    theApp.on_execute()
    return(theApp.count)




