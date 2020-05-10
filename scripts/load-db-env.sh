#!/bin/bash

[[ \
  "$PGUSER" != "" && \
  "$PGPASSWORD" != "" && \
  "$PGHOST" != "" && \
  "$PGPORT" != "" && \
  "$PGDATABASE" != "" \
]] && export DATABASE_URL="postgresql://$PGUSER:$PGPASSWORD@$PGHOST:$PGPORT/$PGDATABASE?ssl=true"
