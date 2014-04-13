#!/usr/bin/env ruby

# http://pytof.googlecode.com/svn-history/r513/trunk/junk/rand/code/fb/
# http://www.davideisenstat.com/fbpfaq/
# http://www.polygenelubricants.com/2010/01/facebook-puzzles.html
# http://github.com/mlbright/puzzles
# http://ntucker.me/we-are-swarm/
# http://kanwei.com/code/2009/03/21/facebook-swarm.html
# http://20bits.com/article/facebook-job-puzzles-prime-bits
# http://cautery.blogspot.com/2010/08/solving-facebook-gattaca-puzzle.html

%w{test/unit open-uri}.each { |e| require e }

# Bron–Kerbosch

=begin

We are the Swarm

Everyone you know anticipates the release of StarCraft 2 with vast eagerness. To help get back in the saddle of the greatest RTS Game Ever, you decide to use programming to help practice your decision making.

You are the Zerg Queen in command of the 654195331th legion of Zerg forces, the only side worth playing in StarCraft. The wise Overmind has given you the important task of cleaning up some remote planets for mineral mining operations. Apparently, some pesky Terran forces have decided to set up base defenses in these locations, prior to your arrival. With your limited forces, you must determine which Terran bases to attack. Your Zerg forces on each planet have free movement over that one planet, and may split up to attack any number of Terran bases on that planet. However, your Zerg ground forces have not yet evolved space flight and thus cannot travel from planet to planet to assist each other.

The Overmind has provided you with some valuable information about each base. For each Terran base, you are given the amount of minerals, and the strength of the Terran forces at that base. Terran communication channels have been disrupted so you do not have to worry about forces from one base aiding another. You know from past experiences that your odds of victory over a particular Terran base are equal to:

P(z,s) = e^(-63s+10)/(e^(-63s+10)+e^(-21z))

Where z is the strength of the Zerg forces, s is the strength of the Terran base, P(z,s) is the probability from (0-1) that the base will be taken over, and e is Euler's number. The expected amount of minerals gained from an attack on a Terran base is therefore equal to round(P(z,s) * m) where m is the amount of minerals at the Terran base. Use these probabilities to attack Terran forces that will likely result in the maximum amount of minerals available for mining.

The first line of input will indicate the number of planets (P). Then that many planets will follow. Each planet input will start with a line separated by spaces indicating the number of bases (B), following by number of zerg (Z) in that order. Following this there will be a line for each base, indicating the base's strength (S) and resource value in that order (V).

Valid values:

1 <= P <= 100
1 <= B <= 20000
0 <= Z <= 20000
0 <= S <= 20000
0 <= V <= 100000

Input Sample:

2
3 100
40 500
30 20
1 0
2
0 100
10000 2

Write a program that should output to standard out the deployment orders of your Zerg horde. Your program must be robust and fast enough to be able to handle large inputs (within the below bounds) within a matter of minutes.

=end
