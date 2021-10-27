#!/usr/bin/env bash
cd "$(dirname "$0")"

echo "Running $0"

APP_NAME=$(basename $(realpath ../))

app_pid=${APP_NAME}.pid

if [[ -f $app_pid ]]; then
  printf "$APP_NAME is running, killing ... "
  pkill -F $app_pid # TODO: FIX
  printf "Done."
fi

printf "\nStarting $APP_NAME ... "
Rscript ../dev/run_dev.R > $APP_NAME.log 2>&1 &
#echo $(Rscript ../dev/run_dev.R)
echo $! > $app_pid
printf "Done.\n"
