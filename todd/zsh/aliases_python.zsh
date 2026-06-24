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

  python3 -m venv .venv && source .venv/bin/activate || {
    echo "Error: Failed to create virtual environment."
    return 1
  }

  git init --initial-branch=main || {
    echo "Error: Failed to initialize git repository."
    return 1
  }

  gibo dump Python macOS > .gitignore

  echo "# project dependencies\n" > requirements.txt

  cat > README.md << EOF
# ${project_name}

## Requirements

- Python 3.9+

## Setup

\`\`\`bash
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
\`\`\`

## Usage

\`\`\`bash
python3 ${project_name}.py
\`\`\`

## License

MIT
EOF

  git add -A || {
    echo "Error: Failed to stage files for git commit."
    return 1
  }

  git commit -m "Initial commit" --no-verify || {
    echo "Error: Failed to create initial git commit."
    return 1
  }

  echo "Python project '${project_name}' created successfully!"
}
