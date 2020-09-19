unbind-key -n C-a
set -g prefix ^A
set -g prefix2 F12
bind a send-prefix
bind-key C-a send-prefix

# X11 clipboard
bind C-c run "tmux save-buffer - | xclip -i -sel clipboard"
bind C-v run "tmux set-buffer \"$(xclip -o -sel clipboard)\"; tmux paste-buffer"
#bind -t emacs-copy y copy-pipe "xclip -i -sel clip"
#bind-key -t emacs-copy MouseDragEnd1Pane copy-pipe "xclip -se c -i"
#bind-key -t emacs-copy MouseDragEnd1Pane copy-pipe "xclip -selection clipboard -i"
bind -T root MouseDown2Pane run -b "xclip -selection clipboard -o | tmux load-buffer - && tmux paste-buffer -s ' '"
# When mouse mode is off, mouse works like in a regular terminal window.
bind m run "\
    tmux show-options -g | grep -q "mouse\\s*on"; \
    if [ \$? = 0 ]; \
    then  \
        toggle=off;  \
    else  \
        toggle=on;  \
    fi;  \
    tmux display-message \"mouse is now: \$toggle\";  \
    tmux set-option -w mouse \$toggle; \
    tmux set-option -g mouse \$toggle; \
    "
