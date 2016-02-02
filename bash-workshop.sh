# a bash/shell scripting tutorial, particularly compatible with the Mac terminal

# if you're on Windows, get PuTTy:
# http://www.chiark.greenend.org.uk/~sgtatham/putty/download.html
# and connect to our good friend blake!

# navigate your file system - list the contents of the current directory
ls
# list them vertically with file details
ls -l
# order them by last modified time
ls -l -t
# or file size
ls -l -S
# also display 'hidden' files (all files starting with a '.')
ls -a -l
# '.' always refers to the *current* directory, '..' to the parent one
# which directory are we in right now? (that's your home directory!)
pwd
# go one level up, look what's going on there, then go into your home dir again
cd ..
# you can try out tab completion with the directory name: type in only the first
# letter of the directory name (this will be your username), if the sequence of
# letters you've typed so far is unambiguous the shell will autocomplete/expand
# it as far as possible
cd y # hit 'tab' now
cd yourusername
# if it doesn't auto-complete, hit tab a second time, there might be ambiguity
# and it will print you all the possible options - in that case, simply type
# another letter to disambiguate further, then try hitting tab again

# multiple single-letter '-' options can be combined together:
ls -al
# the order doesn't matter, on the Mac add color coding of files with -G
ls -laG
# let's define a shell alias so we don't have to retype the options all the time
alias ll="ls -aGl"
ll
# list files of a directory you're not in (don't forget to use tab-completion!)
ll /usr/lib
# use handy wildcards and regex expressions
ll *.jpg
ll DSCN[0-3]*.jpg
ll /usr/bin/[y-z]*

# create a directory
mkdir newdir
cd newdir # remember autocompletion, 'cd n' followed by tab should do the trick!
# create a new (empty) file in the directory
touch newfile
# (NB the command is called 'touch' because, when called on an existing file, it
# sets the file's 'last modified' times to now, without actually modifying it)
# move/rename the file
mv newfile thefile
# copy it
cp thefile anotherfile
# delete/remove the original
rm thefile
# go up one directory and try to delete the directory you created
cd ..
rm newdir
# explicitly tell 'rm' to do recursive deletion of everything in the directory
rm -r newdir

# interacting with file contents:
# print all content on the screen
cat /var/log/system.log # type until /var/log/ then hit tab twice for other logs
# if you only want to see the end of a file (handy for tracking log files):
tail /var/log/system.log
# use the '--help' option implemented by most commands to find out about options
tail --help
# often there are two different (short+long) options to do the same thing
# 'tail' implements two ways to change the default of 10 lines:
tail --lines=5 /var/log/system.log
tail -n 5 /var/log/system.log
# the 'follow' option (-f) blocks the shell to wait for (and echo) new lines:
tail -f --lines=20 /var/log/system.log
# you can 'interrupt' (kill) the program running in the foreground with Ctrl+C


# a handy word/character count program:
wc /var/log/system.log
# can also be run interactively:
wc
# once you're done typing text, indicate the 'end of input' with Ctrl+D

# like tail, most commands have loads of --options, documented in their manual:
man wc
# in 'man' scroll with up/down, search using '/searchterm', exit with q
man cowsay

# the shell allows you to 'pipe' one program's output into another as input:
# filter the lines of the system.log for occurrences of 'wlan'
cat /var/log/system.log | grep wlan
cat Player346_instr | grep zop
# 'grep' actually filters based on regular expressions and we can also pipe
# multiple commands through each other: how many lines contain abbreviated zop?
cat Player346_instr | grep zop[^a] | wc -l
# make a cow with 'xx' eyes say the last line of the system log
tail -n 1 /var/log/system.log | cowsay -e 'xx'

# you can also redirect a program's output to a file
# '>' writes to a file, *overwriting* any previous content:
echo "foo" > somefile
# '>>' writes to a file, *appending* to any current content:
echo "bar" >> somefile
# append the file's word count information to itself
wc somefile >> somefile
cat somefile
# when you want to run a command but for some reason want to hide its output,
# you often redirect the output to a virtual 'trash' file (not actually stored)
cat somereallylongfilewhocares > /dev/null


# a shell such as bash (Bourne Again SHell) is really a programming environment:
# variables start with a $, type "echo $" and hit tab twice to show you possible
# tab completions which will list you all defined variables, pick some and
# print their current values
echo $PWD # hit tab twice right after the '$' to see what other variables exist

# you can mix+match variables and other strings
echo "My username is $USER, my home directory is $HOME"
# within " quotes variables will be resolved, within ' quotes the $ sign is
# just a $ sign
echo 'There is a variable called $USER'
# Within " quotes you can 'escape' special characters with a backslash
echo "The value of the \$USER variable is $USER, and here's a backslash: \\"
# when you want a variable directly adjacent to other characters, use { }
echo "Myusernameis${USER}andIdon'tlikeusingspaces"
# NB when you want to navigate to a file containing spaces, you can either
# surround the name with " quotes - if you don't, you will have to escape spaces
# and other special characters with a backslash, e.g.:
touch some\ file\ with\ spaces\ \(and\ parentheses\).txt


# the commands we used so far aren't 'hardcoded' - they are really little
# programs and scripts stored somewhere on the disk. when you run a command the
# shell simply looks for an executable with the matching name in the 'path':
echo $PATH
# the 'which' command can tell us about where the shell finds the executables:
which ls
which ll # this is just an alias we defined, not actually a program on disk!
which man
which which

# every command we run also has a return value or 'exit status', the shell
# stores the last executed command's exit status in the special variable #?
echo $?
which thisisnotaprogram
echo $?
# 0 = everything ok, other values = some error, code depends on the program
# (often documented in its 'man' page) note: if we look at $? again it will be 0
# now (because the last command was 'echo', which executed successfully)
echo $?


# customising the shell - go back to your home directory (also known as '~')
cd ~
mkdir myscripts
cd myscripts
# there are interactive editors for the shell too
nano myfirstshellscript
# the most important operations are shown at the bottom, e.g. Ctrl+O to save

# put something like the following into the script - this example includes most
# of the basic control structures you need (more on loops further down). 

#!/bin/bash
# the 'shebang' (#!) at the start of the file specifies which interpreter to use
echo "Hello $USER to this script of name $0"
echo "This script was passed $# arguments"
echo "The first argument is \"$1\""
if [ "$1" == "foo" ]; then
  echo "You've passed the foo!"
else if [ -f "$1"]; then
  echo "Found file $1 on disk, its word count is:"
  wc "$1"
else
  echo "The foo is not with you."
fi
# the "[ ... ]" bit after the "if" might look like special bash syntax, but
# "[" is actually just another program on disk called by the shell! bash runs
# the command and, depending on the program's exit status (which will depend on
# the options/logical tests you pass it) will either execute the 'if' or the
# 'else' branch. all options are again documented on the man page: man [

# check if the first argument is a filename
if [ -f "$1" ]; then
  # the 'file' command inspects the header of a file to figure out what it is
  # within a script, assign variables with VARNAME= and access them via $
  FILETYPE=`file "$1"`
  echo "$1 is a file of type $FILETYPE that has `wc -l $1` lines"
fi
echo "The second and third are \"$2\" and \"$3\""
echo "The complete list of arguments is: $@"

# 'if' simply looks at the exit status of whatever command before the ;
if which foo; then
  echo "There is an executable called 'foo' in the path"
fi 
# END OF SCRIPT


# adjust file permissions (as shown by ls -l) to make it executable:
chmod +x myfirstshellscript # don't bother typing the full name, tab-completion!
# execute the script with two arguments
./myfirstshellscript foo bar
./myfirstshellscript /var/log/system.log
# note that running a script like this is the same as running all the commands
# in it in the shell - this means (1) you can also type "if" control structures
# directly into the shell, but it's typically more useful in scripts (2) you can
# also call interactive commands (such as "wc" without arguments) from a shell
# script, which will block the execution until the command terminates. to start
# a permanent process in the background (i.e. without blocking the shell until
# it terminates), put a "&" after the command
someblockingcommand -w --options='possibly' > "output in this file pls" &

# list all processes currently running on your machine
ps -ef 
# inspect running processes' memory & cpu usage - exit again with q
top
# kindly ask a process to terminate, based on its process id (shown by top/ps)
kill 1234
# if that doesn't work, forcibly ask the process to terminate instead
kill -9 1234
# you can also kill processes based on the name of their executable
killall firefox

# use 'echo' and output redirection to create a python script that is executable
# from the command line:
echo '#!/usr/bin/python
print("HHHHHSSSSSHSSS")
print("SSSSS")' > pythonscript
chmod +x pythonscript
./pythonscript


# modify your path to make your scripts more easily accessible: most shells load
# one or more files when they start up: often the global /etc/profile followed
# by user-specific ~/.profile or ~/.bash_profile files, on MacOS:
nano .profile
# add the following to prepend your myscripts folder to the shell's PATH lookup
export PATH=~/myscripts:$PATH
# also 
alias ll='ls -al'
# after updating your .profile you'll need to restart the shell (close+reopen)


# restarted/started a new terminal? check whether the path modification worked:
echo $PATH
# now you can execute your script from everywhere, try it with autocompletion!
myfir # hit tab! when autocompleting a command, the shell looks through the path

# the easiest way to install new programs is through a package manager, for Mac:
# Homebrew: http://brew.sh/
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
# this installs the 'brew' program which can be used to install other programs.
# available homebrew packages can be browsed here: http://brewformulas.org

# 'wget' is a handy program for downloading files, websites and much much more!
brew install wget
# go to your myscript folder, then download a bash-script for processing latex:
wget https://raw.githubusercontent.com/kevinstadler/bash-scripts/master/remove-latex-comments

# why command line tools, pt. I: functionality! user interfaces are tedious (and
# not that fun) to program, so many useful tools/libraries never receive one (or
# only much later). a few examples of handy stuff:

# extract embedded images from a pdf 
pdfimages -list file.pdf
pdfimages -j file.pdf image-prefix


# implementations for remote/network file system interaction (CIFS) - mount
# your uni home drive to a local folder, allowing you to interact with files as
# if they were stored locally:
mkdir ~/unihomedrive
sudo mount.cifs //chss.datastore.ed.ac.uk/chss/ppls/users/s0897960/ ~/unihomedrive -o soft,username=s0897960,domain=ED
# check what file systems are mounted and their usage (disk free):
df 
# disconnect the drive again
umount -l ~/unihomedrive

# sound + video editing/conversion with ffmpeg (or its fork avconv):
brew install ffmpeg
# interactive playback - exit by pressing escape
ffplay somesoundorvideo.mp3
# process audio/video - just show information
ffmpeg -i somefile.mp3
# format conversion
ffmpeg -i somefile.mp3 output.wav
# separate the channels
ffmpeg -i stereo.mp3 -map_channel 0.0.0 left.wav -map_channel 0.0.1 right.wav
# cut out 30 seconds 1:12 into the file, leave file format+encoding unchanged
ffmpeg -i somefile.mp3 -ss 00:01:12 -t 30 -acodec copy 30secondcutout.mp3

# extract the audio track from a video (and track how long it takes)
time ffmpeg -i somevideo.avi -acoded copy audiotrack.mp3

# flip a video
ffmpeg -i input.m4v -vf vflip flipped.m4v
# flip it horizontally and make it blurry
ffmpeg -i input.m4v -vf "hflip,boxblur" blurredmirror.m4v
# find out what other filters are available:
ffmpeg -filters

# ImageMagick's (IM) 'convert' for image manipulation and generation:
brew install imagemagick
# http://www.imagemagick.org/script/command-line-options.php
convert inputfile.pdf -o writethefilehere.png
convert inputfile.jpg -flip flippedimageandotherfiletype.gif


# why command line tools, pt. II: automation!

# handy scripting and shell expansion
echo picture{1,2,3,4,5}.jpg
cp file.txt{,.backup}

# http://www.cyberciti.biz/faq/bash-for-loop/
# Looping I:
for (( i=1; c<=5; c++ )); do
  echo "We've reached number $i"
done


cd "some directory with .JPG files in it"
# Looping II:
for file in "*.JPG"; do
  echo $file
done

# Practical looping: parsing a CSV file to do image concatenation with IM
# NB $IFS is a shell-specific variable that specifies what 'separating
# character' is used when looping through a string/list of results. by default
# it is just a space, so for a CSV file we set it to "," (or ", "), then pipe
# the CSV file content through the loop (see the done < "inputfile" at the end)
while IFS=", " read BS S FS J FA A BA;
do
  echo "Going to combine into file combined$BS$S$FS$J$FA$A${BA}.JPG"
  time convert BS${BS}.JPG S${S}.JPG FS${FS}.JPG J${J}.JPG FA${FA}.JPG A${A}.JPG BA${BA}.JPG +append combined$BS$S$FS$J$FA$A${BA}.JPG
done < "ball permutations BS_S_FS_J_FA_A_BS.txt"

# other possibly useful commands:
# this one should be fun on blake
uptime
who
# tools for running (long) simulations or other batch jobs in the background,
# without them being interrupted/terminated when you exit the shell or log out
# of the server (refer to the man pages or do some googling for details)
nohup
screen
