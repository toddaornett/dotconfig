# node aliases and functions for development
cmd='npm run build'
alias nb="echo \"$cmd\" && $cmd"

cmd='npm run dev'
alias nd="echo \"$cmd\" && $cmd"

cmd='npm run test-dev'
alias nj="echo \"$cmd\" && $cmd"

cmd='npm run lint'
alias nl="echo \"$cmd\" && $cmd"

cmd='npm run start-dev'
alias nsd="echo \"$cmd\" && $cmd"

cmd='npm run start'
alias nsr="echo \"$cmd\" && $cmd"

cmd='npm run test'
alias nt="echo \"$cmd\" && $cmd"

# list mostly cargo related aliases
alias anode="alias | grep '^n' | grep -v '^nvim'"
