#!/usr/bin/env bash

set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

RUN_DIR="$ROOT/.run"
mkdir -p "$RUN_DIR"

. "$ROOT/bin/ports.sh"

BACKEND_PORT="$SUTRA_HTTP_PORT"
BACKEND_URL="http://localhost:${BACKEND_PORT}/"
BACKEND_READY_TIMEOUT=60   # seconds to wait for the backend to listen
BACKEND_SANITY_GRACE=1     # seconds to verify backend didn't crash on startup
SHADOW_DAEMON_PIDFILE="$ROOT/clients/frontend/.shadow-cljs/server.pid"

exec 9>"$RUN_DIR/.start.lock"
if ! flock -n 9; then
  printf 'Another ./bin/start.sh is running. If it is stuck, run ./bin/stop.sh first.\n' >&2
  exit 1
fi

log()  { printf '==> %s\n' "$*"; }
warn() { printf 'WARN: %s\n' "$*" >&2; }

pid_alive_file() {
  local pidfile="$1"; shift
  [ -f "$pidfile" ] || return 1
  local pid
  pid="$(cat "$pidfile" 2>/dev/null || true)"
  [ -n "$pid" ] && kill -0 "$pid" 2>/dev/null || return 1
  [ "$#" -eq 0 ] && return 0
  local comm p
  comm="$(cat "/proc/$pid/comm" 2>/dev/null || true)"
  for p in "$@"; do
    case "$comm" in $p) return 0 ;; esac
  done
  return 1
}


start_frontend() {
  if [ ! -d clients/frontend/node_modules ]; then
    log "Installing frontend deps (first run)"
    (cd clients/frontend && npm install)
  fi

  if pid_alive_file "$RUN_DIR/shadow.pid" "npm*" node; then
    log "shadow-cljs watch already running (pid $(cat "$RUN_DIR/shadow.pid"))"
  elif pid_alive_file "$SHADOW_DAEMON_PIDFILE" java; then
    warn "shadow-cljs daemon is alive (pid $(cat "$SHADOW_DAEMON_PIDFILE"))"
    warn "but no .run/shadow.pid — leftover from a previous session."
    warn "Stopping it so we can start clean."
    ( cd "$ROOT/clients/frontend" && npx shadow-cljs stop >/dev/null 2>&1 ) 9>&- || true
    if pid_alive_file "$SHADOW_DAEMON_PIDFILE" java; then
      kill -TERM "$(cat "$SHADOW_DAEMON_PIDFILE")" 2>/dev/null || true
      sleep 1
    fi
    rm -f "$SHADOW_DAEMON_PIDFILE"
  fi

  if ! pid_alive_file "$RUN_DIR/shadow.pid" "npm*" node; then
    log "Starting shadow-cljs watch"
    ( cd "$ROOT/clients/frontend" \
      && exec nohup npx shadow-cljs watch app </dev/null >"$RUN_DIR/shadow.log" 2>&1 ) 9>&- &
    echo $! >"$RUN_DIR/shadow.pid"
    sleep 1
    if ! pid_alive_file "$RUN_DIR/shadow.pid" "npm*" node; then
      warn "shadow-cljs exited immediately. Last log lines:"
      tail -20 "$RUN_DIR/shadow.log" >&2 || true
      exit 1
    fi
  fi
}

# Without clients/frontend the backend runs alone.
if [ -d clients/frontend ]; then
  start_frontend
fi

EGRESS_PORT=8731
EGRESS_URL="http://127.0.0.1:${EGRESS_PORT}"
EGRESS_KEYFILE="$RUN_DIR/egress-agent-rsa.pkcs8"
EGRESS_MASTER_FILE="$HOME/.sutra/egress-master"
if [ ! -s "$EGRESS_MASTER_FILE" ]; then
  ( umask 077; mkdir -p "$(dirname "$EGRESS_MASTER_FILE")"
    head -c 32 /dev/urandom | od -An -tx1 | tr -d ' \n' > "$EGRESS_MASTER_FILE" )
fi
export SUTRA_EGRESS_MASTER_FILE="$EGRESS_MASTER_FILE"

egress_listening() {
  curl -s -o /dev/null --max-time 2 \
    -H 'Content-Type: application/edn' -d '{}' "$EGRESS_URL/pubkey" 2>/dev/null
}

if pid_alive_file "$RUN_DIR/egress.pid" java; then
  log "Egress agent already running (pid $(cat "$RUN_DIR/egress.pid"))"
elif egress_listening; then
  log "Egress agent already listening on :$EGRESS_PORT (shared — reusing)"
else
  log "Starting egress agent ($EGRESS_URL)"
  ( exec env \
      SUTRA_EGRESS_IP=127.0.0.1 \
      SUTRA_EGRESS_PORT="$EGRESS_PORT" \
      SUTRA_EGRESS_KEYFILE="$EGRESS_KEYFILE" \
      nohup clojure -M:dev -m sutra.sources.egress </dev/null >"$RUN_DIR/egress.log" 2>&1 ) 9>&- &
  echo $! >"$RUN_DIR/egress.pid"
  sleep 1
  if ! pid_alive_file "$RUN_DIR/egress.pid" java; then
    warn "Egress agent exited immediately. Last log lines:"
    tail -20 "$RUN_DIR/egress.log" >&2 || true
    exit 1
  fi
fi


if pid_alive_file "$RUN_DIR/server.pid" java; then
  log "Backend already running (pid $(cat "$RUN_DIR/server.pid"))"
else
  log "Starting backend ($BACKEND_URL)"
  ( exec env \
      SUTRA_HTTP_PORT="$SUTRA_HTTP_PORT" \
      SUTRA_NREPL_PORT="$SUTRA_NREPL_PORT" \
      SUTRA_ALLOW_ANON=1 \
      nohup clojure -M:dev:run </dev/null >"$RUN_DIR/server.log" 2>&1 ) 9>&- &
  echo $! >"$RUN_DIR/server.pid"

  sleep "$BACKEND_SANITY_GRACE"
  if ! pid_alive_file "$RUN_DIR/server.pid" java; then
    warn "Backend exited immediately. Last log lines:"
    tail -30 "$RUN_DIR/server.log" >&2 || true
    exit 1
  fi

  no_wait=0
  if [ "${BACKEND_WAIT:-0}" = "1" ]; then
    no_wait=0
  elif [ "${BACKEND_NO_WAIT:-0}" = "1" ] || [ ! -t 1 ]; then
    no_wait=1
  fi
  if [ "$no_wait" = "1" ]; then
    log "Skipping readiness poll (non-TTY or BACKEND_NO_WAIT=1)"
  else
    log "Waiting for backend to listen on :${BACKEND_PORT} (up to ${BACKEND_READY_TIMEOUT}s)"
    ready=0
    for _ in $(seq 1 "$BACKEND_READY_TIMEOUT"); do
      if ! pid_alive_file "$RUN_DIR/server.pid" java; then
        warn "Backend exited before becoming ready. Last log lines:"
        tail -30 "$RUN_DIR/server.log" >&2 || true
        exit 1
      fi
      code="$(curl -s -o /dev/null -w '%{http_code}' "$BACKEND_URL" 2>/dev/null || true)"
      case "$code" in
        2*|3*) ready=1; break ;;
      esac
      sleep 1
    done
    if [ "$ready" -ne 1 ]; then
      warn "Backend pid is alive but port $BACKEND_PORT didn't respond in ${BACKEND_READY_TIMEOUT}s"
      warn "(continuing anyway — check $RUN_DIR/server.log)"
    fi
  fi
fi

cat <<EOF

Sutra-OS is up.  (instance offset $SUTRA_OFFSET)
  backend:         $BACKEND_URL   (nREPL :$SUTRA_NREPL_PORT)
  egress agent:    $EGRESS_URL   (shared, not offset)
  backend log:     tail -f .run/server.log
  egress log:      tail -f .run/egress.log
EOF
if [ -d clients/frontend ]; then
  echo "  shadow-cljs log: tail -f .run/shadow.log"
fi
echo "  stop with:       ./bin/stop.sh"
