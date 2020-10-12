# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

. ~/.alias

export PATH=${HOME}/bin:${HOME}/usr/local/bin:${PATH}

git config --global user.email "ekofoed@gmail.com"
git config --global user.name  "ESK"

# End
