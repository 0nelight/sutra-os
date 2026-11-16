#!/usr/bin/env bash

set -uo pipefail

HARD_CAP="${STOP_HARD_CAP:-10}"
( sleep "$HARD_CAP" && kill -KILL $$ 2>/dev/null ) </dev/null >/dev/null 2>&1 &
WATCHDOG_PID=$!
trap 'pkill -P "$WATCHDOG_PID" 2>/dev/null; kill "$WATCHDOG_PID" 2>/dev/null; true' EXIT

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

RUN_DIR=".run"
. "$ROOT/bin/ports.sh"
TERM_GRACE="${STOP_GRACE:-1}"
SHADOW_DAEMON_PIDFILE="$ROOT/clients/frontend/.shadow-cljs/server.pid"

log() { printf '==> %s\n' "$*"; }

pid_alive() { [ -n "${1:-}" ] && kill -0 "$1" 2>/dev/null; }

read_pid() { cat "$1" 2>/dev/null || true; }

pid_matches() {
  local pid="$1"; shift
  [ -n "$pid" ] || return 1
  local comm p
  comm="$(cat "/proc/$pid/comm" 2>/dev/null || true)"
  [ -n "$comm" ] || return 1
  for p in "$@"; do
    case "$comm" in $p) return 0 ;; esac
  done
  return 1
}

stop_pid_with_grace() {
  local pid="$1" label="$2"
  pid_alive "$pid" || return 0
  log "Stopping $label (pid $pid)"
  pkill -TERM -P "$pid" 2>/dev/null || true
  kill -TERM "$pid" 2>/dev/null || true
  local i
  for i in $(seq 1 "$TERM_GRACE"); do
    pid_alive "$pid" || return 0
    sleep 1
  done
  log "$label didn't exit on SIGTERM, sending SIGKILL"
  pkill -KILL -P "$pid" 2>/dev/null || true
  kill -KILL "$pid" 2>/dev/null || true
}


if [ -f "$RUN_DIR/server.pid" ]; then
  pid="$(read_pid "$RUN_DIR/server.pid")"
  if pid_alive "$pid" && pid_matches "$pid" java; then
    stop_pid_with_grace "$pid" "backend"
  elif pid_alive "$pid"; then
    log "backend pidfile points at PID $pid but it isn't a JVM (stale, recycled)"
  else
    log "backend not running (pidfile was stale)"
  fi
  rm -f "$RUN_DIR/server.pid"
else
  log "No pidfile for backend (not started here)"
fi

if [ -f "$RUN_DIR/egress.pid" ]; then
  pid="$(read_pid "$RUN_DIR/egress.pid")"
  if pid_alive "$pid" && pid_matches "$pid" java; then
    stop_pid_with_grace "$pid" "egress agent"
  elif pid_alive "$pid"; then
    log "egress pidfile points at PID $pid but it isn't a JVM (stale, recycled)"
  else
    log "egress agent not running (pidfile was stale)"
  fi
  rm -f "$RUN_DIR/egress.pid"
else
  log "No pidfile for egress agent (not started here, or shared)"
fi


shadow_running=0
wrapper_pid="$(read_pid "$RUN_DIR/shadow.pid")"
daemon_pid="$(read_pid "$SHADOW_DAEMON_PIDFILE")"
pid_alive "$wrapper_pid" && pid_matches "$wrapper_pid" "npm*" node && shadow_running=1
pid_alive "$daemon_pid" && pid_matches "$daemon_pid" java && shadow_running=1

if [ "$shadow_running" -eq 1 ]; then
  log "Stopping shadow-cljs (graceful)"
  ( cd "$ROOT/clients/frontend" && npx shadow-cljs stop >/dev/null 2>&1 ) || true

  if [ -f "$SHADOW_DAEMON_PIDFILE" ]; then
    daemon_pid="$(read_pid "$SHADOW_DAEMON_PIDFILE")"
    if pid_alive "$daemon_pid" && pid_matches "$daemon_pid" java; then
      log "shadow-cljs daemon still alive, killing directly"
      stop_pid_with_grace "$daemon_pid" "shadow daemon"
    fi
    rm -f "$SHADOW_DAEMON_PIDFILE"
  fi

  if [ -f "$RUN_DIR/shadow.pid" ]; then
    wrapper_pid="$(read_pid "$RUN_DIR/shadow.pid")"
    if pid_alive "$wrapper_pid" && pid_matches "$wrapper_pid" "npm*" node; then
      stop_pid_with_grace "$wrapper_pid" "shadow watch wrapper"
    fi
  fi
else
  log "shadow-cljs not running"
fi
rm -f "$RUN_DIR/shadow.pid"

log "Done."
