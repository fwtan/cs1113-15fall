import runWorld as rw
import drawWorld as dw
import pygame as pg
from random import randint
################################################################

# This program is an interactive simulation/game. A cat starts
# to move across the screen. The direction of movement is reversed
# on each "mouse down" event.
#
# The state of the cat is represented by a tuple (pos, delta-pos).
# The first element, pos, represents the x-coordinate of the cat.
# The second element, delta-pos, represents the amount that the
# position changes on each iteration of the simulation loop.
#
# For example, the tuple (7,1) would represent the cat at x-coord,
# 7, and moving to the right by 1 pixel per "clock tick."
# 
# The initial state of the cat in this program is (0,1), meaning that the cat
# starts at the left of the screen and moves right one pixel per tick.
#
# Pressing a mouse button down while this simulation run updates the cat state
# by leaving pos unchanged but reversing delta-pos (changing 1 to -1 and vice
# versa). That is, pressing a mouse key reverses the direction of the
# cat.
#
# The simulation ends when the cat is allowed to reach either the left
# or the right edge of the screen.

################################################################

# Initialize world
name = "Cat Fun. Press the mouse (but not too fast)!"
width = 1000
height = 1000
rw.newDisplay(width, height, name)

################################################################

class image_wrapper:
    def __init__(self, img, px, py, vx, vy):
        self.set_image(img);
        self.set_position(px, py);
        self.set_velocity(vx, vy);

    def set_image(self, img):
        self.img_ = img;

    def set_position(self, px, py):
        self.px_ = px;
        self.py_ = py;

    def set_velocity(self, vx, vy):
        self.vx_ = vx;
        self.vy_ = vy;

    def move(self):
        self.px_ = self.px_ + self.vx_;
        self.py_ = self.py_ + self.vy_;

    def change_direction(self, a = 0):
        if a == 0:
            self.vx_ = -self.vx_;
        else:
            self.vy_ = -self.vy_;
            
        
        

    def bInside(self, l, r, t, b):
        return self.px_ >= l and self.px_ < r and self.py_ >= t and self.py_ < b;
        

class aniState:
    def __init__(self, img_w1, img_w2):
        self.set_image(img_w1, img_w2);

    def set_image(self, img_w1, img_w2):
        self.img_w1_ = img_w1;
        self.img_w2_ = img_w2;
    

# Display the state by drawing a cat at that x coordinate
# myimage = dw.loadImage("cat.bmp")
# myimage2 = dw.loadImage("rotunda.jpg")

# state -> image (IO)
# draw the cat halfway up the screen (height/2) and at the x
# coordinate given by the first component of the state tuple
#
def updateDisplay(state):
    dw.fill(dw.blue);
    dw.draw(state.img_w1_.img_, (state.img_w1_.px_, state.img_w1_.py_))
    dw.draw(state.img_w2_.img_, (state.img_w2_.px_, state.img_w2_.py_))


################################################################

# Change pos by delta-pos, leaving delta-pos unchanged
# Note that pos is accessed as state[0], and delta-pos
# as state[1]. Later on we'll see how to access state
# components by name (as we saw with records in Idris).
#
# state -> state
def updateState(state):
    state.img_w1_.move();
    state.img_w2_.move();
    return aniState(state.img_w1_, state.img_w2_);
    

################################################################

# Terminate the simulation when the x coord reaches the screen edge,
# that is, when pos is less then zero or greater than the screen width
# state -> bool
def endState(state):
    if (not state.img_w1_.bInside(0, width, 0, height) or not state.img_w2_.bInside(0, width, 0, height)):
        return True
    else:
        return False


################################################################

# We handle each event by printing (a serialized version of) it on the console
# and by then responding to the event. If the event is not a "mouse button down
# event" we ignore it by just returning the current state unchanged. Otherwise
# we return a new state, with pos the same as in the original state, but
# delta-pos reversed: if the cat was moving right, we update delta-pos so that
# it moves left, and vice versa. Each mouse down event changes the cat
# direction. The game is to keep the cat alive by not letting it run off the
# edge of the screen.
#
# state -> event -> state
#
def handleEvent(state, event):  
#    print("Handling event: " + str(event))
    if (event.type == pg.MOUSEBUTTONDOWN):
        state.img_w1_.change_direction(0)
        state.img_w2_.change_direction(1)
        
        
        return(aniState(state.img_w1_, state.img_w2_))
    else:
        return(state)

################################################################

# World state will be single x coordinate at left edge of world

# The cat starts at the left, moving right 
initState = aniState( image_wrapper(dw.loadImage("cat.bmp"),
                                    randint(100,400), randint(100,400),
                                    randint(-2,2), randint(-2,2)),
                      image_wrapper(dw.loadImage("rotunda.jpg"),
                                    randint(100,400), randint(100,400),
                                    randint(-2,2), randint(-2,2)))

# Run the simulation no faster than 60 frames per second
frameRate = 60

# Run the simulation!
rw.runWorld(initState, updateDisplay, updateState, handleEvent,
            endState, frameRate)
