#! /bin/sh
################################################################################
# Created: Thursday, November  2 2017
# Time-stamp: <2017-11-02 14:08:51 erik>
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

tmp=$(mktemp -d)
cd $tmp

wget sourceforge.net/projects/playerstage/files/Player/3.0.2/player-3.0.2.tar.gz
wget sourceforge.net/projects/playerstage/files/Stage/3.2.2/Stage-3.2.2-Source.tar.gz

tar -xzvf player-3.0.2.tar.gz
tar -xzvf Stage-3.2.2-Source.tar.gz 

cd player-3.0.2
mkdir build ;cd build ; cmake ../
make
sudo make install

cd $tmp

mkdir stage4
cd stage4
git clone git://github.com/rtv/Stage.git
#export STG=$HOME/stg
#cmake -DCMAKE_INSTALL_PREFIX=$STG Stage
cmake Stage
make
sudo make install


# End of file
################################################################################
# Local Variables:
# comment-column: 60
# End:
################################################################################
