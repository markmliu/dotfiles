# Setup fzf
# ---------
if [[ ! "$PATH" == */home/mliu/.fzf/bin* ]]; then
  export PATH="$PATH:/home/mliu/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/mliu/.fzf/shell/completion.bash" 2> /dev/null

# Key bindings
# ------------
source "/home/mliu/.fzf/shell/key-bindings.bash"

