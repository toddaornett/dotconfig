# node aliases and functions for developmentnp() {

function np() {
  if [ "$#" -ne 1 ]; then
    echo "Usage: np <project-name>"
    return 1
  fi

  local project_name="$1"

  if [ -d "$project_name" ]; then
    echo "Error: Directory '$project_name' already exists."
    return 1
  fi

  mkdir -p "$project_name" || {
    echo "Error: Failed to create directory '$project_name'."
    return 1
  }
  cd "$project_name" || {
    echo "Error: Failed to navigate to directory '$project_name'."
    return 1
  }

  git init --initial-branch=main || {
    echo "Error: Failed to initialize git repository."
    return 1
  }

  if ! npx license "$(npm get init.license)" -o "$(npm get init.author.name)" >LICENSE 2>/dev/null; then
    echo "Warning: Failed to create LICENSE file. Skipping..."
  fi

  if ! npx gitignore node 2>/dev/null; then
    echo "Warning: Failed to create .gitignore file. Creating basic .gitignore..."
    echo "node_modules/\n.gitignore\nnpm-debug.log\n.env" >.gitignore
  fi

  if ! npx covgen "$(npm get init.author.email)" 2>/dev/null; then
    echo "Warning: Failed to create CODE_OF_CONDUCT file. Skipping..."
  fi

  npm init -y >/dev/null || {
    echo "Error: Failed to initialize npm project."
    return 1
  }

  jq '.type = "module" | .engines.node = ">=18.0.0" | .scripts.start = "node index.js" | .scripts.test = "echo \"No test specified\" && exit 1"' package.json >tmp.json && mv tmp.json package.json

  echo "// Node.js project entry point\nconsole.log('Hello, $project_name!');" >index.js

  echo "save-exact=true\npackage-lock=true" >.npmrc

  # Stage and commit changes
  git add -A || {
    echo "Error: Failed to stage files for git commit."
    return 1
  }
  git commit -m "Initial commit" --no-verify || {
    echo "Error: Failed to create initial git commit."
    return 1
  }

  echo "Node.js project '$project_name' created successfully!"
}

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
