# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

export EDITOR="emacsclient -c"
export EMACSDIR=~/.config/emacs
export BROWSER=google-chrome

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/rakudo/share/perl6/site/bin" ] ; then
    PATH="$HOME/rakudo/share/perl6/site/bin:$PATH"
fi
if [ -d "$HOME/rakudo/bin" ] ; then
    PATH="$HOME/rakudo/bin:$PATH"
fi
# rbenv, to install and support several ruby versions
if [ -d "$HOME/.rbenv/bin" ] ; then
    PATH="$HOME/.rbenv/bin:$PATH"
fi
# if [ -d "$HOME/doom/bin" ] ; then
#     PATH="$HOME/doom/bin:$PATH"
# fi
if [ -d "$HOME/.config/emacs/bin" ] ; then
    PATH="$HOME/.config/emacs/bin:$PATH"
fi
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# Numenta visualizations
# export HTM_SERVER="$HOME/projects/htm-community/nupic-history-server"
# export CELL_VIZ="$HOME/projects/numenta/cell-viz"
# export HTM_VIZ="$HOME/projects/htm-community/htm-school-viz"

export NODE_PATH=${NODE_PATH}:$(npm root -g)

# Common lisp
sbcl_install_dir="$HOME/.roswell/impls/x86-64/linux/sbcl-bin/2.2.7"
export SBCL_HOME="${sbcl_install_dir}/lib/sbcl"
sbcl_bin="${sbcl_install_dir}/bin"
if [ -d "${sbcl_bin}" ] ; then
    PATH="${sbcl_bin}:$PATH"
fi
roswell_bin="$HOME/.roswell/bin"
if [ -d "${roswell_bin}" ] ; then
    PATH="${roswell_bin}:$PATH"
fi

# if [[ -x /usr/bin/google-drive-ocamlfuse ]]; then
#     mount | grep -q google_drive
#     if [[ $? -ne 0 ]]; then
#         google-drive-ocamlfuse ~/google_drive
#     fi
# fi

export PATH="$HOME/.cargo/bin:$PATH"

export PATH="$HOME/.poetry/bin:$PATH"

# perl5
PATH="$HOME/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="$HOME/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="$HOME/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"$HOME/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=$HOME/perl5"; export PERL_MM_OPT;
eval "$(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib)"

# gpg has to be started here (or in .xinitrc.exwm), if we want to have encryption in exwm
gpg-connect-agent /bye
SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
export SSH_AUTH_SOCK
if [ -z "$DISPLAY" -a "$(tty)" = '/dev/tty5' ]; then
    exec /usr/bin/startx ~/.xinitrc.exwm >/tmp/startx.log 2>&1
fi
