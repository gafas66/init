#! /bin/sh
################################################################################
# Created: Thursday, January  9 2014
#
# Description:
# 

# Setup emacs
sudo apt-get -y install emacs emacs-goodies-el

# Install chrome
cd /tmp
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
sudo dpkg -i google-chrome-stable_current_amd64.deb
# NOTE gives errors which can be ignored - fixed by next line
sudo apt-get -y -f install

# # gazebo
# sh -c 'echo "deb http://packages.osrfoundation.org/gazebo/ubuntu saucy main" > /etc/apt/sources.list.d/gazebo-latest.list'
# wget http://packages.osrfoundation.org/gazebo.key -O - | sudo apt-key add -
# apt-get update
# apt-get -y install gazebo-current

# ROS
#sh -c 'echo "deb http://packages.ros.org/ros/ubuntu trusty main" > /etc/apt/sources.list.d/ros-latest.list'
#wget https://raw.githubusercontent.com/ros/rosdistro/master/ros.key -O - | apt-key add -
#apt-get update
#apt-get install ros-indigo-desktop-full
#sh -c 'echo "deb http://packages.ros.org/ros/ubuntu trusty main" > /etc/apt/sources.list.d/ros-latest.list'
#wget https://raw.githubusercontent.com/ros/rosdistro/master/ros.key -O - | sudo apt-key add -
#apt-get update
#apt-get install ros-indigo-stage
#sudo apt-get install robot-player <= not available

# gitk
sudo apt-get -y install gitk
sudo apt-get -y install git-gui

# NOTE Security = already installed on debian
#apt-get -y install gpg

# NOTE BTSYNC localhost:8888 (probably)

# wget -O bittorrent_sync_x64.tar.gz http://download-new.utorrent.com/endpoint/btsync/os/linux-x64/track/stable
# tar -xzvf bittorrent_sync_x64.tar.gz 
# #cd bittorrent_sync_x64 
# sudo mv btsync /opt 
# sudo ln -sf /opt/btsync /usr/bin/btsync 
# sudo btsync
# # FIXME Does btsync autostart?

sudo apt-get -y install robocode htop
#apt-get -y install eclipse


# Lein
wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
chmod +x lein
mkdir ~/bin
mv lein ~/bin/

cd
lein new hello
cd hello
lein deps

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
