# split a zip archive into multiple parts
# with each part named with roman numerals
function zsplit() {
  local zip_file
  local output_dir
  local base_name
  local file_name
  local input_file
  local part

  # Check for command-line arguments
  if [[ $# -eq 1 ]]; then
    zip_file="$1"
    output_dir="."
  elif [[ $# -eq 2 ]]; then
    zip_file="$1"
    output_dir="$2"
  else
    # Prompt for zip archive filename in zsh-compatible way
    echo -n "Enter the zip archive filename: "
    read -r zip_file
    output_dir="."
  fi

  # Check if the file exists and is a .zip file
  if [[ ! -f "$zip_file" ]]; then
    echo "Error: File '$zip_file' not found."
    exit 1
  fi

  # Check if the file has a .zip extension
  if [[ "$zip_file" != *.zip ]]; then
    echo "Error: '$zip_file' is not a .zip file."
    exit 1
  fi

  # Check if the output directory exists, create it if it doesn't
  if [[ ! -d "$output_dir" ]]; then
    mkdir -p "$output_dir" || {
      echo "Error: Failed to create output directory '$output_dir'."
      exit 1
    }
  fi

  # Get the base name by removing the .zip extension
  base_name="${zip_file%.zip}"
  # Get the filename without the path for output naming
  file_name=$(basename "$base_name")

  # Split the zip file into 100MB parts, outputting to the specified directory, suppressing verbose output
  input_file="$(realpath $zip_file)"
  cd "${output_dir}"
  split -b 100000k "$input_file" "${file_name}-x"

  # Check if the split operation was successful
  if [[ $? -ne 0 ]]; then
    echo "Error: Failed to split '$zip_file'."
    exit 1
  fi

  # Rename the split parts with Roman numerals (no hyphen)
  i=1
  for part in "${output_dir}/${file_name}-x"*; do
    if [[ -f "$part" ]]; then
      roman=$(d2r "$i")
      new_name="${output_dir}/${file_name}${roman:l}.zip"
      mv "$part" "$new_name"
      if [[ $? -eq 0 ]]; then
        echo "Renamed '$part' to '$new_name'"
      else
        echo "Error: Failed to rename '$part' to '$new_name'"
      fi
      ((i++))
    fi
  done

  # Check if any parts were found
  if [[ $i -eq 1 ]]; then
    echo "Error: No split parts were found."
    exit 1
  fi
}

# join split files back into zip archive where
# each part is sequenced with Roman numerals
function zjoin() {
  local base_name
  local -a input_files
  local -a valid_files
  local -A file_order
  local -a sorted_files
  local expected_num
  local file
  local roman
  local num

  # Check if exactly one argument (base name) is provided
  if [ $# -ne 1 ]; then
    echo "Error: Exactly one base name must be provided. Usage: $0 <basename>" >&2
    return 1
  fi

  base_name="$1"

  # Collect and validate input files
  input_files=(${base_name}*.zip)
  if [ ${#input_files[@]} -eq 0 ]; then
    echo "Error: No files found matching '${base_name}*.zip'." >&2
    return 1
  fi

  valid_files=()
  for file in "${input_files[@]}"; do
    if [ ! -f "$file" ]; then
      echo "Warning: File '$file' not found, skipping." >&2
      continue
    fi
    valid_files+=("$file")
  done

  # Check if there are any valid files
  if [ ${#valid_files[@]} -eq 0 ]; then
    echo "Error: No valid .zip files found for base name '$base_name'." >&2
    return 1
  fi

  # Validate and collect files with Roman numerals
  for file in "${valid_files[@]}"; do
    # Extract Roman numeral after base name
    roman="${file#${base_name}}"
    roman="${roman%.zip}"
    if [ -z "$roman" ] || ! echo "$roman" | grep -qE '^[ivxlcdm]+$'; then
      echo "Warning: '$file' does not match pattern '${base_name}<roman>.zip', skipping." >&2
      continue
    fi
    num=$(r2d "$roman") || {
      echo "Warning: Invalid Roman numeral '$roman' in '$file', skipping." >&2
      continue
    }
    if [ -n "${file_order[$num]}" ]; then
      echo "Warning: Duplicate Roman numeral '$roman' in '$file', skipping." >&2
      continue
    fi
    file_order[$num]="$file"
    echo "Debug: Found '$file' with Roman numeral '$roman' ($num)" >&2
  done

  # Check if any valid files remain
  if [ ${#file_order[@]} -eq 0 ]; then
    echo "Error: No files with valid Roman numerals found for base name '$base_name'." >&2
    return 1
  fi

  # Sort files by numerical order and warn about gaps
  sorted_files=()
  expected_num=1
  for num in $(for k in "${(@k)file_order}"; do echo "$k"; done | sort -n); do
    while [ "$expected_num" -lt "$num" ]; do
      echo "Warning: Missing Roman numeral for part $expected_num, proceeding with available files." >&2
      ((expected_num++))
    done
    sorted_files+=("${file_order[$num]}")
    ((expected_num++))
  done

  # Combine the split files into a single ZIP using cat
  cat "${sorted_files[@]}" || {
    echo "Error: Failed to combine split files. Ensure all files are valid split ZIP parts." >&2
    return 1
  }
}
