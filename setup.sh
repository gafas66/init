for file in .bashrc .emacs.d .alias .irbrc .bash_profile .gnupg
do
	if [ -e ~/$file ];then rm -fr ~/$file ;fi
	ln -s ~/init/$file ~/$file 
done
