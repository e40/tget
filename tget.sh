#! /bin/bash
#
# Run the locally built tget on a test database in this directory named
# test.db

./tget/tget --root $PWD \
    --db $PWD/test.db \
    --config $PWD/tget-config/config.cl \
    "$@"
    
