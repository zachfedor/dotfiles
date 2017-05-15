#!/bin/sh

if [ -z "$PKEY" ]; then
    # if PKEY is not specified, run ssh using default keyfile
    ssh -i ~/.ssh/id_rsa-personal "$@"
else
    ssh -i "$PKEY" "$@"
fi

