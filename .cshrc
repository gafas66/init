#! /bin/csh
################################################################################
# Created: Thursday, November  5 2020
# Author: Erik Kofoed, Synaptics
# Description:
# 

if (! $?__CSHRC_RUN) then

    setenv __CSHRC_RUN 1
    
    set _yellow = "%{\033[33m%}"
    set _green  = "%{\033[32m%}"
    set _red    = "%{\033[31m%}"
    set _off    = "%{\033[0m%}"

    set hostn = `hostname | awk -F. '{print $1}'`
    alias setprompt 'source ~/init/.cshrc'

    alias cd 'chdir \!* && setprompt'
    
endif

unset GBRANCH; unsetenv GBRANCH
setenv GBRANCH "sh -c 'git branch --no-color 2> /dev/null' | grep \* | cut -d ' ' -f 2"
set BRANCH="`$GBRANCH`"
if ($BRANCH != "") then
    set BRANCH="git:$BRANCH "
endif

set prompt = "`whoami`@${hostn} ${_red}CSH${_off} ${_yellow}$BRANCH${_off}%c % "

# End of file
################################################################################
# Local Variables:
# comment-column: 60
# End:
################################################################################
