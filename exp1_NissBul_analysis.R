require(lattice)
# analyzing trajectories of sequential action learning experiments
# (motion-SRT paradigm aka statistical movement learning)
# George Kachergis 11.11.2013

load("exp1_NissBul_traj_data.RData") # exp1 - 20 Ss, 10 NissBul, 10 random seqs

# Loc doesn't change until it reaches the next stimulus
# Correct is only 1 at the first time S touches the target, then 0 until S leaves that location (then -1)

preprocess <- function(d) {
	d$score <- NULL
	d$trial = d$trial + 1
	Ntr = length(unique(d$trial)) # 80
	d$half = ifelse(d$trial < (Ntr/2), "First", "Second")
	test = subset(d, cond=="test") # 5% - test trials!
	# test trials take a lot longer than the ~14000ms of training trials
	#prevTarget / timeTargetHit # identity / ms since prev location was touched
	#d$TargetHL = NA # 500ms after a target is reached, the next target stimulus turns green
	d$Time <- cut(d$trialTime, seq(-1,max(d$trialTime),20),labels=F) # levels=seq(-1,max(all$TrTime),20) # 20ms bins..
	d$Time50 <- cut(d$trialTime, 500, labels=F) # 50 time bins per trial (unequal sizes, then)
	d$curTarget = as.factor(d$curTarget)
	# 20 trials/block
	# 0=upper_left, 1=upper_right, 2=lower_left, 3=lower_right
	stim_diam = 80 
	stim_offset = 260
	center=c(1024,768) / 2
	xPos = rep(c(center[1]-stim_offset-stim_diam/2, center[1]+stim_offset-stim_diam/2), 2)
	yPos = c(rep(center[2]-stim_offset-stim_diam/2, 2), rep(center[2]+stim_offset-stim_diam/2, 2)) 
	NB = c(4,2,3,1,3,2,4,3,2,1) - 1  # 3 1 2 0 2 1 3 2 1 0
	orientation = c("v","d","v","v","d","v","h","d","h", "d") # vertical, diagonal, horizontal
	dir = c("u","ld","u","d","lu","d","l","ru","l", "rd") # up, down, left, right, left-down, etc
	
	d$orient = NA
	d$dir = NA
	NB_t = c(NB, 3)  # add transition to beginning
	for(i in 1:(length(NB_t)-1)) {
		movind = with(d, which(curTarget==NB_t[i] & nextTarget==NB_t[i+1]))
		d[movind,]$orient = orientation[i]
		d[movind,]$dir = dir[i]
	}
	
	# need to transform x and y coordinates based on prevTarg and nextTarg
	# move to 0,0, then flip around axes if necessary
	#d$x0 = d$xPos - 217
	#d$y0 = d$yPos - 89
	# leave diagonal for now (need to subtract x that depends on their y?)
	# switch x and y of 
	
	return(d)
}


d = preprocess(exp1)

test = subset(d, cond=="test") # random = odd subjects, NB87 = even
d = subset(d, cond!="test")
nb = subset(d, cond=="NB87")
rand = subset(d, cond=="random")



plot_horiz_move <- function(d, name="") {
	if(name!="") {
		png(paste(name,".png",sep=""), width=600, height=400, pointsize=12)
		plot.new()
		}
	agg <- with(d, aggregate(yPos, list(curTarget=curTarget, prevTarget=prevTarget, Time=Time), mean))
	print(xyplot(Time~x|curTarget, groups=prevTarget, data=agg, auto.key=T, type='l', ylab="Time (ms)"))
update(trellis.last.object(), strip = strip.custom(strip.names = TRUE, strip.levels = TRUE, bg="white"), par.strip.text = list(cex = 1.0))
	if(name!="") dev.off()
	
	agg <- with(d, aggregate(yPos, list(curTarget=curTarget, prevTarget=prevTarget, Time=Time), mean))
	print(xyplot(Time~x|curTarget, groups=prevTarget, data=agg, auto.key=T, type='l', ylab="Time (ms)"))
	print(qplot(Time, x, data=agg, facets = ~ curTarget + prevTarget))
	# qplot(mpg, wt, data = mtcars, colour = cyl)
	}

traditional_rt <- function(d) {	
	#tt = subset(d, subject==1 & trial==20)
	#wt = subset(d, subject==13 & trial==7) # 84 Correct==1
	#require(car)
	#scatterplotMatrix(tt[,5:14])
	
	hits = subset(d, Correct==1)
	subjs = unique(hits$subject)
	hits$targetRT = NA # how long to reach target (after it turns green)
	for(s in subjs) {
		inds = with(hits, which(subject==s))
		targetRTs = diff(hits[inds,"timeTargetHit"])
		hits[inds,]$targetRT = c(NA,targetRTs)
	}
	
	hits = subset(hits, !is.na(targetRT)) # 26797 down to 26777
	
	# diff(unique(wt$timeTargetHit)) # alternate method; can do subject by subject -- re-do this later
	# table(hits$subject, hits$trial) # some subjects have far more than 10 hits per trial; early arrival??
	hits = subset(hits, targetRT>0) # remove the weird ones (with >>10 hits per trial)
	hits = subset(hits, targetRT<3040) # remove 79 slow moves (mean=1147, sd=1893)
	hits = subset(hits, prevTarget!=-1) # only 2 points
	
	hits$Block <- cut(hits$trial, 8,labels=F) 
	ha <- with(hits, aggregate(targetRT, list(Block=Block, cond=cond), mean))
	ha_s <- with(hits, aggregate(targetRT, list(Block=Block, cond=cond, subject=subject), mean))
	ha_sd <- with(ha_s, aggregate(x, list(Block=Block, cond=cond), sd))
	ha$SE = ha_sd$x / sqrt(10-1) # 10 subjects per condition
	dodge <- position_dodge(width=.2)
	limits <- with(ha, aes(ymax=x+SE, ymin=x-SE))
	# ggplot has useful functions: cut_interval = equal-length intervals, cut_number = equal number of points
	a = qplot(Block, x, data=ha, colour=cond, position=dodge) + labs(x="Blocks of 10 Training Trials", y="Response Time (ms)", fill="Condition") + geom_errorbar(limits,  width=0.2, position=dodge) # + ylim(0,.82)
	print(a)
	ggsave("exp1_basic_RT_result.pdf")
	dev.off()
	
	#nbhit = subset(hits, cond=="NB87")
	#ha <- with(nbhit, aggregate(targetRT, list(curTarget=curTarget, prevTarget=prevTarget, half=half, cond=cond), mean))
	#print(xyplot(x~curTarget|prevTarget, groups=half, data=ha, auto.key=T, type='l', ylab="Time (ms)"))
}

traditional_rt(d) # works, but the for loop takes a few minutes to run -- make it faster with subject loop and diff()!

# make a nice graph of 1 trial for introduction
graph_one_trial <- function(d, subject_num, trial_num, name="") {
	axlim = c(-20,600)
	tr2 = subset(d, subject==subject_num & trial==trial_num)
	tr2$xPos = tr2$xPos - 217
	tr2$yPos = tr2$yPos - 89
	if(name!="") {
		png(paste(name,".png",sep=""), width=540, height=560, pointsize=12)
		plot.new()
		}
	print(with(tr2, plot(xPos, yPos, type='b', pch=nextTarget+2, col=heat.colors(dim(tr2)[1])[Time], xlim=axlim, ylim=axlim, xlab="X Cursor Position", ylab="Y Cursor Position"))) # as.factor(ckey[curTarget])
	leftx = c(0,80,80,0); rightx = c(520,600,600,520)
	boty = c(0,0,80,80); topy = c(600,600,520,520)
	polygon(leftx, boty); polygon(leftx, topy)
	polygon(rightx, boty); polygon(rightx, topy)
	if(name!="") dev.off()
}

graph_one_trial(d, 1, 7, name="1trial_s1tr7_rand") # random
graph_one_trial(d, 6, 5, name="1trial_s6tr5_NB87") # NissBul

# mostly useful for NB87 condition - looking for context effects
plot_horiz_move <- function(dat, name="") {
	ckey = c('red','green','blue','magenta','cyan','orange')
	if(name!="") {
		png(paste(name,".png",sep=""), width=600, height=400, pointsize=12)
		plot.new()
		}
	dd = subset(dat, Time<430 & prevTarget!=-1)
	agg <- with(dd, aggregate(yPos, list(curTarget=curTarget, prevTarget=prevTarget, Time=Time50), mean))
	print(xyplot(Time~x|curTarget, groups=prevTarget, data=agg, auto.key=T, type='l', ylab="Time (ms)"))
update(trellis.last.object(), strip = strip.custom(strip.names = TRUE, strip.levels = TRUE, bg="white"), par.strip.text = list(cex = 1.0))
	if(name!="") dev.off()
	
	agg <- with(dd, aggregate(yPos, list(curTarget=curTarget, prevTarget=prevTarget, Time=Time50), mean))
	print(xyplot(Time~x|curTarget, groups=prevTarget, data=agg, auto.key=T, type='l', ylab="Time (ms)"))
	
	hor = subset(dd, orient=="h")
	agg <- with(hor, aggregate(xPos, list(curTarget=curTarget, prevTarget=prevTarget, Time=Time50), mean))
	print(xyplot(Time~x|curTarget, groups=prevTarget, data=agg, auto.key=T, type='l', ylab="Time (ms)"))
	
	diag = subset(d, orient=="d")
	
	ver = subset(d, orient=="v")
	agg <- with(ver, aggregate(yPos, list(curTarget=curTarget, prevTarget=prevTarget, Time=Time), mean))
	print(xyplot(Time~x|curTarget, groups=prevTarget, data=agg, auto.key=T, type='l', ylab="Time (ms)"))
	
	agg <- with(ver, aggregate(yPos, list(curTarget=curTarget,  half=half, Time=Time), mean))
	print(xyplot(Time~x|curTarget, groups=half, data=agg, auto.key=T, type='l', ylab="Time (ms)"))
	
	}

plot_horiz_move(nb)


# curTarget is where you should be headed right now (updated after it's turned green--I think)
# prevTarget is where you just left (or are still leaving)
# nextTarget is where you should go after you arrive where you're going now

# should be looking at prev to cur for movement direction
# cur to next for context effects (predictive)

# contrasts: u->ld vs. u->d;  d->lu vs. d->l;  
# good! also good would be: u->

# make a graph of each average movement -- UNFINISHED
cool_graphs <- function(d) {
	require(ggplot2)
	
	nb = subset(d, cond=="NB87")
	ag <- with(nb, aggregate(cbind(xPos,yPos), list(Time=Time, curTarget=curTarget, nextTarget=nextTarget, prevTarget=prevTarget), mean))
	# try qplot?

	ag <- with(nb, aggregate(cbind(xPos,yPos), list(Time=Time, Half=half, trial=trial), mean))	
	c <- ggplot(ag, aes(x=xPos, y=yPos, colour=factor(Half))) + stat_smooth() + geom_point()
	
	c <- ggplot(ag, aes(x=xPos, y=yPos, colour=factor(Half))) + stat_smooth() + geom_point()
	
	rand = subset(d, cond=="random")
	ag <- with(rand, aggregate(cbind(xPos,yPos), list(Time=Time, curTarget=curTarget, nextTarget=nextTarget, prevTarget=prevTarget), mean))
	c <- ggplot(ag, aes(x=xPos, y=yPos, colour=factor(curTarget), alpha=.05)) + stat_smooth() + geom_point()
	
	ag <- with(d, aggregate(cbind(xPos,yPos), list(Time=Time, curTarget=curTarget, nextTarget=nextTarget, prevTarget=prevTarget), mean))


	a = aggregate(cbind(xPos, yPos) ~ ., data=d, mean)
}


