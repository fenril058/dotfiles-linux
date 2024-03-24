;; https://gist.github.com/no-defun-allowed/068bb95c476201f009fcfeb135f21432
;; http://www.rtqe.net/ObliqueStrategies/Ed5.html
(defvar *oblique-strategy-list-by-Eno*
  '(Abandon normal instruments
            "Accept advice"
            "Accretion"
            "Allow an easement (an easement is the abandonment of a stricture)"
            "Always first steps"
            "Are there sections? Consider transitions"
            "Ask people to work against their better judgement"
            "Ask your body"
            "Be dirty"
            "Be extravagant"
            "Be less critical more often"
            "Breathe more deeply"
            "Change nothing and continue with immaculate consistency"
            "Courage!"
            "Cut a vital connection"
            "Decorate, decorate"
            "Define an area as 'safe' and use it as an anchor"
            "Destroy -nothing -the most important thing"
            "Discard an axiom"
            "Disciplined self-indulgence"
            "Disconnect from desire"
            "Discover the recipes you are using and abandon them"
            "Distorting time"
            "Do nothing for as long as possible"
            "Do something boring"
            "Do the words need changing?"
            "Do we need holes?"
            "Don't be afraid of things because they're easy to do"
            "Don't be frightened of cliches"
            "Don't be frightened to display your talents"
            "Don't break the silence"
            "Don't stress one thing more than another"
            "Emphasise differences"
            "Emphasise the flaws"
            "Faced with a choice do both (given by Diter Rot)"
            "Gardening, not architecture"
            "Give the game away"
            "Give way to your worst impulse"
            "Go outside. Shut the door"
            "Go to an extreme, move back to a more comfortable place"
            "Honour thy error as a hidden intention"
            "How would you have done it?"
            "Humanise something free of error"
            "In total darkness or in a very large room, very quietly"
            "Is it finished?"
            "Is there something missing?"
            "Just carry on"
            "Listen to the quiet voice"
            "Look at the order in which you do things"
            "Look closely at the most embarrassing details and amplify them"
            "Make a blank valuable by putting it in an exquisite frame"
            "Make a sudden, destructive unpredictable action; incorporate"
            "Make an exhaustive list of everything you might do and do the last thing on the list"
            "Make it more sensual"
            "Make something implied more definite (reinforce, duplicate)"
            "Not building a wall but making a brick"
            "Once the search is in progress, something will be found"
            "Only a part, not the whole"
            "Only one element of each kind"
            "Overtly resist change"
            "Question the heroic approach"
            "Remember .those quiet evenings"
            "Remove ambiguities and convert to specifics"
            "Remove specifics and convert to ambiguities"
            "Repetition is a form of change"
            "Retrace your steps"
            "Reverse"
            "Short circuit (example; a man eating peas with the idea that they will improve his virility shovels them straight into his lap)"
            "Simple subtraction"
            "Simply a matter of work"
            "Slow preparation..Fast execution"
            "State the problem in words as clearly as possible"
            "Take a break"
            "Take away the elements in order of apparent non importance"
            "The inconsistency principle"
            "The most important thing is the thing most easily forgotten"
            "Think -inside the work -outside the work"
            "Tidy up"
            "Towards the insignificant"
            "Trust in the you of now"
            "Try faking it! (given by Stewart Brand)"
            "Turn it upside down"
            "Use 'unqualified' people"
            "Use an old idea"
            "Use an unacceptable colour"
            "Use filters"
            "Voice nagging suspicions"
            "Water"
            "What are you really thinking about just now? Incorporate"
            "What mistakes did you make last time?"
            "What to increase? What to reduce?"
            "What to maintain?"
            "What would your closest friend do?"
            "What wouldn't you do?"
            "When is it for?"
            "Where's the edge? Where does the frame start?"
            "Which elements can be grouped?"
            "Which frame would make this look right?"
            "Who should be doing this job? How would they do it?"
            "Work at a different speed"
            "Would anybody want it?"
            "You are an engineer"
            "You don't have to be ashamed of using your own ideas"
            )
  "Brian Eno's Oblique Strategies cards.")

(defun oblique-strategies-by-Eno ()
  "Show a message from Brian Eno's Oblique Strategies cards Ed5."
  (interactive)
  (message "%s"
           (nth
            (random (length *oblique-strategy-list-by-Eno*))
            *oblique-strategy-list-by-Eno*)))

(provide 'oblique-strategies-ed5)