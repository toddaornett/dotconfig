function ltrim() {
  sed -E 's/^[[:blank:]]+//'
}

function rtrim() {
  sed -E 's/[[:blank:]]+$//'
}

function trim() {
  ltrim | rtrim
}

# convert arabic number to roman
function d2r() {
  emulate -L zsh
  setopt KSH_ARRAYS
  local num
  local result
  local i
  local values
  local numerals
  values=(1000 900 500 400 100 90 50 40 10 9 5 4 1)
  numerals=("M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I")

  if [[ $# -eq 0 ]]; then
    echo "Usage: d2r <number>"
    return 1
  fi

  num=$1

  if ! [[ "$num" =~ ^[0-9]+$ ]]; then
    echo "Error: provide a positive integer."
    return 1
  fi

  result=""
  for ((i = 0; i < ${#values[@]}; i++)); do
    while ((num >= values[i])); do
      result+="${numerals[i]}"
      ((num -= values[i]))
    done
  done

  echo "$result"
}

# convert roman number to arabic
r2d() {
  if [ $# -eq 0 ]; then
    echo "Usage: r2d <roman numeral>"
    return 1
  fi

  local input result matched sym value i len
  input=$(printf '%s' "$1" | tr '[:lower:]' '[:upper:]')
  result=0

  emulate -L zsh
  setopt KSH_ARRAYS
  local -a symbols
  local -a values
  symbols=("M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I")
  values=(1000 900 500 400 100 90 50 40 10 9 5 4 1)

  while [ -n "$input" ]; do
    matched=0
    i=0
    while [ $i -lt ${#symbols[@]} ]; do
      sym="${symbols[$i]}"
      value="${values[$i]}"
      len=${#sym}
      if [ "${input:0:$len}" = "$sym" ]; then
        result=$((result + value))
        input="${input:$len}"
        matched=1
        break
      fi
      i=$((i + 1))
    done
    if [ "$matched" -eq 0 ]; then
      echo "Error: Invalid Roman numeral."
      return 1
    fi
  done
  echo "$result"
}
