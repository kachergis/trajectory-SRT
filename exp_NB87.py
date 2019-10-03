''' 
replicate NissenBulemer1987 with trajectory-tracking movement paradigm
Roy de Kleijn and George Kachergis, October 2013
'''

import sys, os, random, pygame, re
from textwrap import wrap
from pygame.locals import *
from socket import gethostname
from time import localtime, strftime

# Experiment variables
RSI = 500 # response-to-stimulus interval (ms)
stim_diam = 80 # width of the squares (or circles, if we change that)
trials_per_block = 20
num_test_trials = 10

# Define some colors
black    = (   0,   0,   0)
white    = ( 255, 255, 255)
red      = ( 255,   0,   0)
green    = (   0, 255,   0)

bgColor = white
textColor = black
FPS = 60 # max frames per second

# Initialize Pygame
pygame.init()
pygame.font.init()
Font = pygame.font.SysFont(None,32)

# Set the height and width of the screen
screen = pygame.display.set_mode((1024, 768), HWSURFACE|FULLSCREEN) 

nXDim, nYDim = screen.get_size()
center = (int(nXDim/2.0), int(nYDim/2.0))
# Set stimulus coordinates
stim_offset = 260
xPos = [center[0]-stim_offset-stim_diam/2, center[0]+stim_offset-stim_diam/2]*2 
yPos = [center[1]-stim_offset-stim_diam/2]*2 + [center[1]+stim_offset-stim_diam/2]*2 
# 0=upper_left, 1=upper_right, 2=lower_left, 3=lower_right
print(xPos)
print(yPos)




# This class represents the ball        
# It derives from the "Sprite" class in Pygame
class Block(pygame.sprite.Sprite):
    
    # Constructor. Pass in the color of the block, 
    # and its x and y position
    def __init__(self, color, width, height):
        # Call the parent class (Sprite) constructor
        pygame.sprite.Sprite.__init__(self) 

        # Create an image of the block, and fill it with a color.
        # This could also be an image loaded from the disk.
        self.image = pygame.Surface([width, height])
        self.image.fill(color)

        # Fetch the rectangle object that has the dimensions of the image
        # Update the position of this object by setting the values 
        # of rect.x and rect.y
        self.rect = self.image.get_rect()

def get_ordering(file_name):
    ORDER_DIR = "."
    raw = open(os.path.join(ORDER_DIR, file_name), 'r')
    train = []
    for l in raw:
        r = l.strip().split()
        for i in range(len(r)):
            r[i] = int(r[i])
        train.append(r)
    raw.close()
    return train

def pause(pause, logFile):
    waittime = 0
    time_stamp = pygame.time.get_ticks()
    while waittime < pause:
        pygame.event.clear()
        if pygame.key.get_pressed()[pygame.K_ESCAPE]:
            logFile.close(); pygame.quit(); sys.exit()
        waittime = pygame.time.get_ticks() - time_stamp

def subject_info(rand_seed):
    #subj = raw_input('Enter Subject Number: ')
    #subj = int(subj)
    random.seed(rand_seed)
    
    if not os.path.isdir(os.path.join(os.getcwd(), 'data')):
        os.mkdir(os.path.join(os.getcwd(), 'data'))
    
    files = os.listdir(os.path.join(os.curdir, 'data'))
    subj = len(files)+1
    
    logFilename = os.path.join(os.getcwd(), 'data', 'data_s') + str(subj) + ".txt"
    
    # Open data files
    logFile = open(logFilename, 'w')
    logFile.write('Participant: %d\n' % (subj))
    logFile.write('MachineID: %s\n' % (gethostname()))
    logFile.write('Start Time: %s\n' % (strftime("%a, %b %d, %Y %H:%M:%S", localtime() )))
    logFile.write('Seed: %s\n' % rand_seed)
    # [s_num, trial, cond, total_time, trial_time, prevTarget, curTarget, nextTarget, hit_target_pos, t_targethit, touched_object, correct_touch, pos[0], pos[1], score]
    logFile.write('subject\ttrial\tcond\ttotalTime\ttrialTime\tprevTarget\tcurTarget\tnextTarget\thitTargetPos\ttimeTargetHit\tobjTouched\tCorrect\txPos\tyPos\n') # \tscore
    logFile.flush()
    logFile.close()
    pause(1000, logFile) # write the file...
    logFile = open(logFilename, 'a')
    return subj, logFile


def wait4space():
    pygame.event.clear()
    while 1:
        event = pygame.event.poll()
        if pygame.key.get_pressed()[pygame.K_SPACE]:
            break
        if pygame.key.get_pressed()[pygame.K_LSHIFT] and pygame.key.get_pressed()[pygame.K_BACKQUOTE]:
            pygame.quit(); sys.exit()    

def DisplayText(sText, loc=(nXDim,nYDim), clrText=textColor, nFontSize=32, nCharPerRow=57):
    screen.fill(bgColor)
    sSegText = wrap(sText, nCharPerRow)
    iVLoc = (nYDim / 2) - nFontSize * len(sSegText)/2
    for sLine in sSegText:
        Text = Font.render(sLine, True, clrText)
        screen.blit(Text,((nXDim/2.0)-(Text.get_width()/2.0),iVLoc))
        iVLoc += nFontSize
    pygame.display.flip()
    wait4space()

def rest_break():
    DisplayText("Rest your eyes for a moment and press SPACE to continue when you're ready. Thanks!")
    wait4space()
    screen.fill(bgColor)


def goodbye(logFile):
    logFile.write('End Time: %s\n' % (strftime("%a, %b %d, %Y %H:%M:%S", localtime() )))
    logFile.flush()
    logFile.close()
    screen.fill(white)
    pygame.event.clear()
    endScript = '''Please go to the experimenter and let them know that you are finished.  \
If you have any questions, please do not hesitate to ask the experimenter.'''
    DisplayText(endScript)
    pygame.display.flip()
    while 1:
        event = pygame.event.poll()
        if pygame.key.get_pressed()[pygame.K_ESCAPE]:
            break
    pygame.quit()
    raw_input('Press enter to exit.')
    sys.exit()

def write_line(logFile, line=[]):
    string = ""
    for i in line:
        string += str(i)+'\t'
    logFile.write( string.strip('\t') + '\n' )

def get_random_trials(locs_per_trial, num_trials):
    # make random trials with no repeating stimuli
    N = num_trials * locs_per_trial
    stim = [1,2,3,4]
    alls = []
    alls.append(random.choice(stim))
    for i in range(1,N):
        tmp = stim[:]
        tmp.remove(alls[i-1])
        alls.append(random.choice(tmp))
    stim_list = []
    for i in range(num_trials):
        stim_list.append(alls[i*locs_per_trial:(i+1)*locs_per_trial])
    return stim_list


def main():
    s_num, logFile = subject_info(random.randint(10000, 99999))
    if s_num%2==0:
        order_file = "NissenBulemer1987.txt" #"seq1.txt"
        stim_list = get_ordering(order_file)
        cond = "NB87"
    else:
        stim_list = get_random_trials(10, 80)
        cond = "random"
    pygame.mouse.set_visible(0) # Hide mouse cursor

    
    stim_per_trial = len(stim_list[0])
    DisplayText('Quickly and accurately move your mouse to any square that turns green. Press SPACE to continue.')

    pygame.mouse.set_pos(center)

    total_group = pygame.sprite.Group()
    block_list = pygame.sprite.Group()
    distractor_group = pygame.sprite.Group()
    target_group = pygame.sprite.Group()


    # set up response locations
    for i in range(len(xPos)):
        block = Block(red, stim_diam, stim_diam) 
        block.id = i
        block.rect.x = xPos[i]
        block.rect.y = yPos[i]
        block_list.add(block)
        total_group.add(block)
        distractor_group.add(block)

    player = Block(black, 12, 12) # cursor
    total_group.add(player)

    # Used to manage how fast the screen updates
    clock = pygame.time.Clock()

    score = 0 # score just goes down and down; only +1 the *moment* you arrive at a target
    output = [] # buffer data here, write it out at rest breaks
    total_time = 0 # time
    prevTarget = -1
    curTarget = -1 # set when target is green
    nextTarget = stim_list[0][0] # update when curTarget changes (when it turns green)
    t_targethit = -1
    num_trials = len(stim_list)

    for trial in range(num_trials): 
        hit_target_pos = -1
        num_targets_hit = 0
        touched_object = -1 
        correct_touch = -1
        
        # Clear the screen
        screen.fill(white)

        if (trial+1)%trials_per_block==0:
                for line in output:
                    write_line(logFile, line)
                output = []
                rest_break()
                t_targethit = -1 # don't carry back to prev trial
                prevTarget = -1
                curTarget = -1
                nextTarget = stim_list[trial][0]-1

        trial_start = pygame.time.get_ticks()
        
        done=False # within-trial loop

        while done==False:
            for event in pygame.event.get(): # User did something
                if event.type == pygame.QUIT: # If user clicked close
                    done=True # Flag that we are done so we exit this loop
            milliseconds = clock.tick(FPS) # Limit to FPS frames per second
            total_time += milliseconds # time since start of exp / 1000.0 # in seconds
            trial_time = pygame.time.get_ticks() - trial_start  # time since beginning of trial

            # Clear the screen
            screen.fill(white)

            pos = pygame.mouse.get_pos() # [x,y]
            
            # Set the player object to the mouse location
            player.rect.x=pos[0]
            player.rect.y=pos[1]
            
            # target or distractor collisions?
            target_hit_list = pygame.sprite.spritecollide(player, target_group, False)
            distractor_hit_list = pygame.sprite.spritecollide(player, distractor_group, False)      
            
            # quit experiment if left shift + ~ are pressed
            if pygame.key.get_pressed()[pygame.K_ESCAPE]:
                logFile.close(); pygame.quit(); sys.exit()
                #done = True # skip to production phase. For debugging only

            # set cur, prev, and nextTarget, and make cur green
            if pygame.time.get_ticks() - t_targethit > RSI:
                for block in block_list:
                    if block.id==stim_list[trial][num_targets_hit]-1:
                        block.image.fill(green)
                        target_group.add(block) # adds target block to target block list
                        distractor_group.remove(block)
                        curTarget = block.id
                        if num_targets_hit > 0:
                            prevTarget = stim_list[trial][num_targets_hit-1]-1
                        elif trial>0: # no targets hit on this trial, but look on prev at last stim
                            prevTarget = stim_list[trial-1][stim_per_trial-1]-1

                        if num_targets_hit < stim_per_trial-1: # <stim_per_trial-1?
                            nextTarget = stim_list[trial][num_targets_hit+1]-1
                        elif trial<(num_trials-1): # no targets remain on this trial, but next trial
                            nextTarget = stim_list[trial+1][0]-1


            # Check the list of collisions.
            for block in distractor_hit_list:
                score -=1
                #print('Wrong! Touched ' , str(block.id))
                touched_object = str(block.id)
                if touched_object!=curTarget:
                    correct_touch = 0

            for block in target_hit_list:
                block.image.fill(red)
                #print('Correct! Touched ' , str(block.id))
                score += 1
                hit_target_pos = stim_list[trial][num_targets_hit]-1                
                num_targets_hit += 1
                touched_object = block.id
                correct_touch = 1
                target_group.remove(block)
                distractor_group.add(block)
                t_targethit = pygame.time.get_ticks()

            if num_targets_hit==stim_per_trial:
                done = True # last target is hit; go to next trial

            # Draw all sprites and update screen
            total_group.draw(screen)
            pygame.display.flip()

            line = [s_num, trial, cond, total_time, trial_time, prevTarget, curTarget, nextTarget, hit_target_pos, t_targethit, touched_object, correct_touch, pos[0], pos[1]] # , score
            output.append(line)


    for line in output:
        write_line(logFile, line) 

    # -------------PRODUCTION PHASE-------------
    DisplayText('Thanks! You will be done soon. In this phase, move your cursor to where you think the next square will turn green. Please try your best! Press SPACE to continue.')
    cond = "test"

    # Reset stuff
    output = [] # buffer data here, write it out at rest breaks
    pygame.mouse.set_pos(center)

    prevTarget = -1
    curTarget = -1 # set when target is green
    nextTarget = stim_list[0][0] # update when curTarget changes (when it turns green)
    t_targethit = -1
    last_new_object = -1

    for trial in range(num_test_trials):
        hit_target_pos = -1
        num_targets_hit = 0
        touched_object = -1 
        correct_touch = -1
        
        # Clear the screen
        screen.fill(white)

        if (trial+1)%trials_per_block==0:
                for line in output:
                    write_line(logFile, line)
                output = []
                rest_break()
                t_targethit = -1 # don't carry back to prev trial
                prevTarget = -1
                curTarget = -1
                nextTarget = stim_list[trial][0]-1

        trial_start = pygame.time.get_ticks()
        
        done=False # within-trial loop

        while done==False:
            for event in pygame.event.get(): # User did something
                if event.type == pygame.QUIT: # If user clicked close
                    done=True # Flag that we are done so we exit this loop
            milliseconds = clock.tick(FPS) # Limit to FPS frames per second
            total_time += milliseconds # time since start of exp / 1000.0 # in seconds
            trial_time = pygame.time.get_ticks() - trial_start  # time since beginning of trial

            # Clear the screen
            screen.fill(white)

            pos = pygame.mouse.get_pos() # [x,y]
            
            # Set the player object to the mouse location
            player.rect.x=pos[0]
            player.rect.y=pos[1]
            
            # target or distractor collisions?
            target_hit_list = pygame.sprite.spritecollide(player, target_group, False)
            distractor_hit_list = pygame.sprite.spritecollide(player, distractor_group, False)      
            
            # quit experiment if left shift + ~ are pressed
            if pygame.key.get_pressed()[pygame.K_ESCAPE]:
                logFile.close(); pygame.quit(); sys.exit()
                #done = True # skip to production phase. For debugging only

            # set cur, prev, and nextTarget, and make touched distractor green
            if pygame.time.get_ticks() - t_targethit > RSI:
                for block in block_list:
                    if block.id==stim_list[trial][num_targets_hit]-1:
                        if len(distractor_hit_list) > 0 and last_new_object!=touched_object:
                            block.image.fill(green) # only turn green if subject moves to a wrong object
                        target_group.add(block) # adds target block to target block list
                        distractor_group.remove(block)
                        curTarget = block.id
                        if num_targets_hit > 0:
                            prevTarget = stim_list[trial][num_targets_hit-1]-1
                        elif trial>0: # no targets hit on this trial, but look on prev at last stim
                            prevTarget = stim_list[trial-1][stim_per_trial-1]-1

                        if num_targets_hit < stim_per_trial-1: 
                            nextTarget = stim_list[trial][num_targets_hit+1]-1
                        elif trial<(num_trials-1): # no targets remain on this trial, but next trial
                            nextTarget = stim_list[trial+1][0]-1


            # Check the list of collisions.
            for block in distractor_hit_list:
                score -=1
                #print('Wrong! Touched ' , str(block.id))
                touched_object = block.id
                if touched_object!=curTarget:
                    correct_touch = 0

            for block in target_hit_list:
                block.image.fill(red)
                #print('Correct! Touched ' , str(block.id))
                score += 1
                hit_target_pos = stim_list[trial][num_targets_hit]-1                
                num_targets_hit += 1
                touched_object = block.id
                correct_touch = 1
                target_group.remove(block)
                distractor_group.add(block)
                t_targethit = pygame.time.get_ticks()
                if last_new_object!=touched_object:
                    last_new_object = touched_object

            if num_targets_hit==stim_per_trial:
                done = True # last target is hit; go to next trial

            # Draw all sprites and update screen
            total_group.draw(screen)
            pygame.display.flip()

            line = [s_num, trial, cond, total_time, trial_time, prevTarget, curTarget, nextTarget, hit_target_pos, t_targethit, touched_object, correct_touch, pos[0], pos[1]] # , score
            output.append(line)


    for line in output:
        write_line(logFile, line) 
    goodbye(logFile)




if __name__ == '__main__':
    main()
