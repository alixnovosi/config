# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# We don't want to output anything unless we're sure that this shell is
# interactive; otherwise we could interfere with programs like scp. Therefore
# we check for the "i" (interactive option) before running anything that might
# produce output.
case "$-" in
	*i*)

        # Only show fortune for the first time I log in.
        if [[ "`who | grep amichaud | wc -l`" -le 1 ]]; then

            # Only show users if I'm not the only one logged in.
            if [[ "`who | grep amichaud | wc -l`" -ne "`who | wc -l`" ]]; then
		        # display who else is logged in
		        users|tr ' ' '\n'|uniq|tr '\n' ' '|awk '{print $0} END {print ""}'
            fi

		    # fortune of the day - short message to brighten up your login
		    which fortune &> /dev/null && fortune -s
        fi
		;;
esac

mkdir -p $HOME/src/go/src
mkdir -p $HOME/src/go/bin

export GOPATH=$HOME/src/go
export PATH=$PATH:$GOPATH/bin

if [ -f "$HOME/.bashrc" ]; then
    . "$HOME/.bashrc"
fi
