#! /bin/sh
################################################################################
# Created: Thursday, November  2 2017
# Time-stamp: <2017-11-04 18:03:40 erik>
# Author: Erik Kofoed, ESK# Description:
#

# Install player / stage on mint

# For development
#sudo apt-get -y install cmake
sudo apt-get -y install build-essential
sudo apt-get -y install libpng-dev libjpeg-dev libxxf86vm1 libxxf86vm-dev libxi-dev libxrandr-dev libfltk1.3-dev
sudo apt-get -y install mesa-common-dev mesa-utils-extra libgl1-mesa-dev libglapi-mesa
sudo apt-get -y install freeglut3-dev libltdl-dev
sudo apt-get -y install git cmake g++ fltk1.1-dev libjpeg8-dev libpng12-dev libglu1-mesa-dev libltdl-dev


sudo apt-get -y install build-essential autotools-dev cpp libboost-dev  libboost-thread-dev  libboost-signals-dev  libltdl7 libltdl7-dev libgnomecanvas2-0 libgtk2.0-dev libjpeg62-dev libtool swig libgsl-dev libgsl2

sudo apt-get -y install libxmu-dev libxi-dev python-dev ruby ruby-dev

tmp=$(mktemp -d)
cd $tmp

svn checkout https://svn.code.sf.net/p/playerstage/svn/code/player/trunk player
cd player
mkdir build ;cd build ; cmake ../
make
sudo make install

cd $tmp

mkdir stage4
cd stage4
git clone git://github.com/rtv/Stage.git
cmake Stage
make
sudo make install

cd ;rm -fr $tmp

echo '
#Player/Stage
export PATH=$PATH:"/usr/local/lib64"
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:"/usr/local/lib":"/usr/local/lib64"
export PLAYERPATH="/usr/local/lib":"/usr/local/lib64"
export STAGEPATH="/usr/local/lib":"/usr/local/lib64"
' > /tmp/x
echo "cat /tmp/x >> /etc/bash.bashrc" > /tmp/y ;chmod a+x /tmp/y ; sudo /tmp/y

# End of file
################################################################################
# Local Variables:
# comment-column: 60
# End:
################################################################################
