# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

export EDITOR="emacsclient -c"
# export EDITOR=emacs
export EMACSDIR=~/.config/emacs
#export BROWSER=google-chrome
export BROWSER=vivaldi
export PRINTER=HL-L2445DW

# See /etc/default/keyboard, https://github.com/flatpak/flatpak/issues/5341 Language keyboard Layout Bug
# If this does not work, then try adding to /etc/environment.
export XKB_DEFAULT_MODEL=pc104
export XKB_DEFAULT_LAYOUT=us
export XKB_DEFAULT_VARIANT=altgr-intl

# Chinese input method
export XMODIFIERS=@im=fcitx5
export GTK_IM_MODULE=fcitx5
export QT_IM_MODULE=fcitx5

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	  source "$HOME/.bashrc"
    fi
fi

if [ -d "$HOME/Android/Sdk/tools" ] ; then
    PATH="$HOME/Android/Sdk/tools:$PATH"
fi

if [ -d "$HOME/Android/Sdk/tools/bin" ] ; then
    PATH="$HOME/Android/Sdk/tools/bin:$PATH"
fi

if [ -d "$HOME/Android/Sdk/platform-tools" ] ; then
    PATH="$HOME/Android/Sdk/platform-tools:$PATH"
fi

if [ -d "$HOME/Android/Sdk/build-tools/33.0.1" ] ; then
    PATH="$HOME/Android/Sdk/build-tools/33.0.1:$PATH"
fi

if [ -d "/usr/lib/android-ndk" ] ; then
    PATH="/usr/lib/android-ndk:$PATH"
fi

if [ -d "$HOME/Android/Sdk/emulator" ] ; then
    PATH="$HOME/Android/Sdk/emulator:$PATH"
fi

export ANDROID_HOME=$HOME/Android/Sdk

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
if [ -d "$HOME/.config/emacs/bin" ] ; then
    PATH="$HOME/.config/emacs/bin:$PATH"
fi
dot_local_bin="$HOME/.local/bin"
if [ -d "${dot_local_bin}" ] ; then
    PATH="${dot_local_bin}:$PATH"
fi
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

export NODE_PATH=${NODE_PATH}:$(npm root -g)

# Common lisp
sbcl_install_dir="$HOME/.roswell/impls/x86-64/linux/sbcl-bin/2.2.9"
export SBCL_HOME="${sbcl_install_dir}/lib/sbcl"
sbcl_bin="${sbcl_install_dir}/bin"
if [ -d "${sbcl_bin}" ] ; then
    PATH="${sbcl_bin}:$PATH"
fi
roswell_bin="$HOME/.roswell/bin"
if [ -d "${roswell_bin}" ] ; then
    PATH="${roswell_bin}:$PATH"
fi

if [ -d "$HOME/.cargo/bin" ] ; then
    PATH="$HOME/.cargo/bin:$PATH"
fi

if [ -d "$HOME/gpt4all/bin" ] ; then
    PATH="$HOME/gpt4all/bin:$PATH"
fi

if [ -d "$HOME/.poetry/bin" ] ; then
    PATH="$HOME/.poetry/bin:$PATH"
fi

export JULIA_LOAD_PATH="/home/jw/projects/julia:$JULIA_LOAD_PATH"
# >>> juliaup initialize >>>

# !! Contents within this block are managed by juliaup !!

case ":$PATH:" in
    *:/home/jw/.juliaup/bin:*)
        ;;

    *)
        export PATH=/home/jw/.juliaup/bin${PATH:+:${PATH}}
        ;;
esac

# <<< juliaup initialize <<<

if [ -d "$HOME/perl5/bin" ] ; then
    PATH="$HOME/perl5/bin${PATH:+:${PATH}}"
    PERL5LIB="$HOME/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
    PERL_LOCAL_LIB_ROOT="$HOME/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
    PERL_MB_OPT="--install_base \"$HOME/perl5\""; export PERL_MB_OPT;
    PERL_MM_OPT="INSTALL_BASE=$HOME/perl5"; export PERL_MM_OPT;
    eval "$(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib)"
fi

# gpg has to be started here (or in .xinitrc.exwm), if we want to have encryption in exwm
gpg-connect-agent /bye
SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
export SSH_AUTH_SOCK

fcitx5 -d > /tmp/jw_fcitx5.log 2>&1

#if [ -z "$DISPLAY" ] && [ "$(tty)" = '/dev/tty4' ]; then
#    exec /usr/bin/sway >/tmp/startsway.log 2>&1
#fi

# if [ -z "$DISPLAY" ] && [ "$(tty)" = '/dev/tty5' ]; then
#     exec /usr/bin/startx ~/.xinitrc.exwm >/tmp/startx.log 2>&1
# fi
. "$HOME/.cargo/env"
