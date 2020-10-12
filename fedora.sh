#!/bin/env sh
# Default root stuff

#yum distro-sync
yum -y install kernel-devel kernel-headers dkms gcc gcc-c++

yum -y install emacs emacs-goodies emacs-color-theme
yum -y install ucblogo

# Get most popular office package for linux

yum -y install libreoffice.x86_64

#v=1.5.7-1
#wget http://sourceforge.net/projects/projectlibre/files/ProjectLibre/1.5.7/projectlibre-$v.rpm
#yum -y install projectlibre-$v.rpm
#rm -f projectlibre-$v.rpm

# Various stuff
yum -y install xscreensaver-base xscreensaver-extras xscreensaver-extras-base xscreensaver-gl-base xscreensaver-gl-extras

# NOTE Security
yum -y install gpg

################################################################################
