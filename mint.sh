#! /bin/sh
################################################################################
# Created: Thursday, January  9 2014
#
# Description:
# 

sudo apt-get -y install git cmake g++ fltk1.1-dev libjpeg8-dev libpng-dev libglu1-mesa-dev libltdl-dev

# Build stuff
sudo apt-get -y install cmake-curses-gui ruby libboost-program-options-dev
# Needed (CPATH is header files, LIBRARY_PATH linker files)
export CPATH=/usr/include/gdk-pixbuf-2.0:/usr/include/glib-2.0:/usr/lib/x86_64-linux-gnu/glib-2.0/include

# Useful stuff to search for say header files
sudo apt-get -y install apt-file

# Setup emacs
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt-get update
sudo apt-get install emacs26

sudo rm /etc/alternatives/emacs
sudo ln -s /usr/bin/emacs26 /etc/alternatives/emacs

sudo apt-get -y install emacs-goodies-el

# gitk
sudo apt-get -y install gitk
sudo apt-get -y install git-gui

sudo apt-get -y install robocode htop

# Lein
sudo apt-get -y install leiningen # Not in Mint 18.3

# Git setup
git config --global user.email "ekofoed@gmail.com"
git config --global user.name "gafas66"

# Update and upgrade
sudo apt-get -y update
sudo apt-get -y upgrade
sudo dpkg --configure -a # Fix things not installed

# Install java 8
#sudo apt-get install python-software-properties
#sudo add-apt-repository ppa:webupd8team/java
#sudo apt-get update
#sudo apt-get install oracle-java8-installer
#sudo apt-get install oracle-java8-set-default

mkdir -p ~/bin
cp ~/init/lein.sh ~/bin/lein
chmod a+x ~/bin/lein

echo "Install guest additions"
echo "Install player"
echo "Install gazebo"

# .................../´¯/) 
# ................,/¯..// 
# .............../..../ / 
# ......./´¯/'...'/´¯¯`¸ 
# ..../'/.../..../......./¨¯\ 
# ..('(...´(..´......,~/'...') 
# ...\.................\/..../ 
# ....''...\.......... _.´ 
# ......\..............( 
# ........\.............\

# End of file
################################################################################
# Local Variables:
# comment-column: 60
# End:
################################################################################
