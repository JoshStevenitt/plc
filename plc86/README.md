# plc86

stack is now set up! 

Make sure you're in the plc86 directory when doing stack commands. The git is saved to just plc. Do this with "cd plc86"

Currently the code is in app/Main.hs. If you need to change it for the interpreter, you'll need to do "stack build", and then "stack exec -- plc86-exe Test.txt"

To do individual tasks, do "stack exec -- plc86-exe submission1-tasks/taskX.txt"
