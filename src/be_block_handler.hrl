-include_lib("epgsql/include/epgsql.hrl").

-define (BIN_TO_B58(B), list_to_binary(libp2p_crypto:bin_to_b58((B)))).
-define (BIN_TO_B64(B), list_to_binary(base64:encode_to_string((B)))).
