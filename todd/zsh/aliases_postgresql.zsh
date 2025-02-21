# postgresql tools

function pgc() {
  local db_env=${1:-$DB_DEFAULT_ENV}
  local db_name=${2:-$DB_DEFAULT_NAME}
  local default_db_user=${DB_DEFAULT_USER:-$USER}
  local db_user=${3:-$default_db_user}
  local postgres_default_port="5432"
  local default_db_port=${DB_DEFAULT_PORT:-$postgres_default_port}
  local db_port=${4:-$default_db_port}
  if [[ $db_env = "local" ]]; then
    db_user=${3:-$USER}
  fi
  local passpath=db/$db_name/$db_env/$db_user
  export PGPASSWORD="$(pass $passpath | head -1)"
  local db_host=$(pass $passpath | grep host | cut -d ' ' -f2)
  local database_url
  if [ $db_port != $postgres_default_port ]; then
    database_url="postgres://$db_user@$db_host:$db_port/$db_name"
  else
    database_url="postgres://$db_user@$db_host/$db_name"
  fi
  echo "Connecting to $database_url"
  psql "$database_url"
  unset PGPASSWORD
}

function pgs() {
  pg_ctl -D $(brew --prefix)/var/$(brew list | grep postgres) start
}

function pge() {
  pg_ctl -D $(brew --prefix)/var/$(brew list | grep postgres) stop
}
