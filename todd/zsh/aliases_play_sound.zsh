# aliases_play_sound.zsh — play sound aliases

# ---------------------------------------------------------------------------
# Dynamically build play aliases for system sounds
# ---------------------------------------------------------------------------
if [[ "$(uname)" == "Darwin" ]]; then
  _sound_dir="/System/Library/Sounds"
elif [[ "$(uname)" == "Linux" ]]; then
  _sound_dir="/usr/share/sounds/freedesktop/stereo"
fi

if [[ -n "$_sound_dir" && -d "$_sound_dir" ]]; then
  for _sound_file in "$_sound_dir"/*; do
    # Strip path and extension, lowercase, remove spaces/dashes
    _sound_name="${_sound_file:t:r}"
    _alias_name="play${_sound_name:l:gs/ //:gs/-//}"
    alias "$_alias_name"="afplay '$_sound_file'"
  done
fi
unset _sound_file _sound_name _alias_name _sound_dir

# ---------------------------------------------------------------------------
# List all play* aliases and functions
# ---------------------------------------------------------------------------
alias aplay="alias | grep '^play' | sort"
