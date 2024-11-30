# .bashrc
################################################################################

source $HOME/.alias
source $HOME/.alias_local

export PATH=${HOME}/bin:${HOME}/usr/local/bin:${PATH}

export CDPATH=
export CDPATH="$CDPATH:/home/EPI/project/EUPILOT/VEC/usr/esk"
export CDPATH="$CDPATH:/home/EPI/project/STXMOD/users"
alias c="echo \$CDPATH | sed 's/:/\n/g'"

module use /home/kofoed/modulefiles

git config --global user.email "ekofoed@gmail.com"
git config --global user.name  "ESK"

# NOTE Avoid silly warnings for commands in need of proper terminal
[[ -z ${INSIDE_EMACS+x} ]] || x=$TERM && export TERM=eterm-color
module load prj/vec/1
[[ -z ${INSIDE_EMACS+x} ]] || export TERM=$x && unset x

export __RED="\[\033[0;31m\]"
export __GREEN="\[\033[0;32m\]"
export __BROWN="\[\033[0;33m\]"
export __BLUE="\[\033[0;34m\]"
export __PURPLE="\[\033[0;35m\]"
export __CYAN="\[\033[0;36m\]"
export __LIGHT_GRAY="\[\033[0;37m\]"
export __LIGHT_BLUE="\[\033[1;34m\]"
export __LIGHT_GREEN="\[\033[1;32m\]"
export __LIGHT_CYAN="\[\033[1;36m\]"
export __LIGHT_RED="\[\033[1;31m\]"
export __LIGHT_PURPLE="\[\033[1;35m\]"
export __YELLOW="\[\033[1;33m\]"
export __WHITE="\[\033[1;37m\]"
export __RESTORE="\[\033[0m\]" #0m restores to the terminal's default colour

#PROMPT_DIRTRIM=1
PROMPT_COMMAND='\
BRANCH="";\
if git branch &> /dev/null; then \
    BRANCH="git:$(git branch 2> /dev/null | grep \* | cut -d " " -f 2)";\
fi;\
PS1="${__GREEN}\s ${__YELLOW}${BRANCH}${__RESTORE} \w\n\h $ ";'

# End
################################################################################
