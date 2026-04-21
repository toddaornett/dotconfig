# bun aliases

cmd='bun run build'
alias brb="echo \"$cmd\" && $cmd"

cmd='bun run build-storybook'
alias brbsb="echo \"$cmd\" && $cmd"

cmd='bun run check'
alias brc="echo \"$cmd\" && $cmd"

cmd='bun run dev'
alias brd="echo \"$cmd\" && $cmd"

cmd='bun run format:check'
alias brfc="echo \"$cmd\" && $cmd"

cmd='bun run format:write'
alias brfw="echo \"$cmd\" && $cmd"

cmd='bun run lint'
alias brl="echo \"$cmd\" && $cmd"

cmd='bun run lint -- --quiet'
alias brlq="echo \"$cmd\" && $cmd"

cmd='bun run lint:fix'
alias brlf="echo \"$cmd\" && $cmd"

cmd='bun run start'
alias brs="echo \"$cmd\" && $cmd"

cmd='bun run storybook'
alias brsb="echo \"$cmd\" && $cmd"

# list mostly cargo related aliases
alias abun="alias | grep '^br'"
