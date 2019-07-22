#!/bin/sh

set -eu

if [[ -e tmp/stahl-bootstrap ]]; then
	# We're done!
	exit 0
elif command -v stahl-bootstrap >/dev/null; then
	cp `which stahl-bootstrap` tmp/stahl-bootstrap
else
	curl -LsSf $(curl -LsSf https://api.github.com/repos/remexre/stahl_bootstrap/releases/latest | jq -r '.assets[0].browser_download_url') -o tmp/stahl-bootstrap
	chmod +x tmp/stahl-bootstrap
fi
