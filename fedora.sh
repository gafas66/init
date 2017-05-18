#!/bin/env sh
# Default root stuff

yum distro-sync
yum -y install kernel-devel kernel-headers dkms gcc gcc-c++

yum -y install emacs emacs-goodies emacs-color-theme
yum -y install ucblogo
yum -y install @Robotics

# Chrome
cat <<EOF > /etc/yum.repos.d/google-chrome.repo
[google-chrome]
name=google-chrome - 64-bit
baseurl=http://dl.google.com/linux/chrome/rpm/stable/x86_64
enabled=1
gpgcheck=1
gpgkey=https://dl-ssl.google.com/linux/linux_signing_key.pub
EOF

yum -y install google-chrome-stable <<EOF
y
EOF

# Network pptp (for vpn)

yum -y install NetworkManager-pptp
yum -y install NetworkManager-pptp-gnome
yum -y install pptp
yum -y install pptp-setup

# Various root stuff
gem install ruby-player

yum -y install libreoffice.x86_64

v=1.5.7-1
wget http://sourceforge.net/projects/projectlibre/files/ProjectLibre/1.5.7/projectlibre-$v.rpm
yum -y install projectlibre-$v.rpm
rm -f projectlibre-$v.rpm

# bittorent sync
#wget http://www.bittorrent.com/sync/downloads/complete/os/x64_glibc

# Various fun stuff
yum -y install xscreensaver-base xscreensaver-extras xscreensaver-extras-base xscreensaver-gl-base xscreensaver-gl-extras

# NOTE Security
yum -y install gpg

# NOTE BTSYNC localhost:8888 (probably)

wget -O bittorrent_sync_x64.tar.gz http://download-new.utorrent.com/endpoint/btsync/os/linux-x64/track/stable
tar -xzvf bittorrent_sync_x64.tar.gz 
#cd bittorrent_sync_x64 
mv btsync /opt 
ln -sf /opt/btsync /usr/bin/btsync 
btsync

################################################################################
