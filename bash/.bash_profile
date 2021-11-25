#
# Configuration for login shell
#
# When you login (type username and password) via console, either sitting at the
# machine, or remotely via ssh, =.bash_profile= is executed to configure your
# shell before the initial command prompt.
#

if [[ -f "${HOME}/.bashrc" ]]
then
	source "${HOME}/.bashrc"
fi
