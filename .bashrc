# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

. ~/.alias

if [ -d /home/ekofoed/modulefiles ]; then
    module use /home/ekofoed/modulefiles
    module load prj/PS1
fi

export PATH=${HOME}/bin:${HOME}/usr/local/bin:${PATH}

git config --global user.email "ekofoed@gmail.com"
git config --global user.name  "ESK"

# KLUDGE gazeebo
#export LIBGL_ALWAYS_SOFTWARE=1

if [ -f /opt/ros/hydro/setup.bash ];then . /opt/ros/hydro/setup.bash ;fi

export LD_LIBRARY_PATH=/usr/local/lib
#export PLAYERPATH+=/usr/local/lib
#export STAGEPATH+=/usr/local/lib

# End
