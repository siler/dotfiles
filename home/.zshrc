# Update path first to prioritize /usr/local/bin
export GOPATH=$HOME/code/go
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin:$GOPATH/bin

# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="siler"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git go composer postgres python pip)

source $ZSH/oh-my-zsh.sh

# Source activated keys
[ -z "$HOSTNAME" ] && HOSTNAME=`uname -n`
[ -f $HOME/.keychain/$HOSTNAME-sh ] && . $HOME/.keychain/$HOSTNAME-sh
[ -f $HOME/.keychain/$HOSTNAME-sh-gpg ] && . $HOME/.keychain/$HOSTNAME-sh-gpg

# Source scripts in profile
PROFILE_DIR=$HOME/.profile

if [ -d $PROFILE_DIR/bin ]; then PATH=$PROFILE_DIR/bin:$PATH; fi
for f in $PROFILE_DIR/*; do source $f; done

# If the secure folder is mounted, source scripts there
CUDA_PROFILE_DIR=$HOME/cuda/.profile
if [ -d $CUDA_PROFILE_DIR ]; then
	if [ -d $CUDA_PROFILE_DIR/bin ]; then PATH=$CUDA_PROFILE_DIR/bin; fi
	for f in $CUDA_PROFILE_DIR/*; do source $f; done
fi

setopt no_hist_verify

alias homeshick="source $HOME/.homesick/repos/homeshick/bin/homeshick.sh"
