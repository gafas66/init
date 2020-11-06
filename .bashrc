# .bashrc
################################################################################

#echo "$__SHLVL1 : $SHLVL"

if [[ $__SHLVL1 == $SHLVL ]];then

    # This executes only first time

    source $HOME/.alias
    export PATH=${HOME}/bin:${HOME}/usr/local/bin:${PATH}

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
        # This executes second and future times, as command below is SHLVL+1
        export PS1=$($HOME/init/.bashrc)
    }
    PROMPT_DIRTRIM=1
    export PROMPT_COMMAND=prompt_command
fi

# This section always executes

BRANCH=""
if which git &>/dev/null; then
    BRANCH=$(git branch 2>/dev/null | grep \* | cut -d " " -f 2)
else
    BRANCH="(git not installed)"
fi
if [[ $BRANCH != "" ]];then BRANCH="git:$BRANCH ";fi

if [[ $__SHLVL1 == $SHLVL ]];then

    export PS1="\u@\h ${__GREEN}BASH ${__YELLOW}${BRANCH}${__RESTORE}${SCHROOT_CHROOT_NAME}${__BLUE}\w \
${__RESTORE}\$ "                                                                                
else
    echo "\u@\h ${__GREEN}BASH ${__YELLOW}${BRANCH}${__RESTORE}${SCHROOT_CHROOT_NAME}${__BLUE}\w \
${__RESTORE}\$ "                                                                                
fi

# End
################################################################################
