# .bashrc
################################################################################


if [[ $__RUN_BASH ]];then
    __x=1
else
    
    export __RUN_BASH=1
    
    export PATH=${HOME}/bin:${HOME}/usr/local/bin:${PATH}
    . ~/.alias

    git config --global user.email "ekofoed@gmail.com"
    git config --global user.name  "ESK"

    export __GREEN="\[\033[0;32m\]"
    export __CYAN="\[\033[0;36m\]"
    export __RED="\[\033[0;31m\]"
    export __PURPLE="\[\033[0;35m\]"
    export __BROWN="\[\033[0;33m\]"
    export __LIGHT_GRAY="\[\033[0;37m\]"
    export __LIGHT_BLUE="\[\033[1;34m\]"
    export __LIGHT_GREEN="\[\033[1;32m\]"
    export __LIGHT_CYAN="\[\033[1;36m\]"
    export __LIGHT_RED="\[\033[1;31m\]"
    export __LIGHT_PURPLE="\[\033[1;35m\]"
    export __YELLOW="\[\033[1;33m\]"
    export __WHITE="\[\033[1;37m\]"
    export __RESTORE="\[\033[0m\]" #0m restores to the terminal's default colour

    if [ -z $SCHROOT_CHROOT_NAME ]; then
        SCHROOT_CHROOT_NAME=" "
    fi

    function prompt_command {
        export PS1=$(~/init/.bashrc "OFF")
    }
    PROMPT_DIRTRIM=1
    export PROMPT_COMMAND=prompt_command

fi

BRANCH=""
if which git &>/dev/null; then
    BRANCH=$(git branch 2>/dev/null | grep \* | cut -d " " -f 2)
else
    BRANCH="(git not installed)"
fi
if [[ $BRANCH != "" ]];then BRANCH="git:$BRANCH ";fi

if [[ ! -z __x ]];then
    echo "\u@\h ${__GREEN}BASH ${__YELLOW}${BRANCH}${__RESTORE}${SCHROOT_CHROOT_NAME}${__BLUE}\w \
${__RESTORE}\$ "                                                                                

fi

# End
