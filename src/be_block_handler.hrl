-include_lib("epgsql/include/epgsql.hrl").

-define (BIN_TO_B58(B), list_to_binary(libp2p_crypto:bin_to_b58((B)))).
-define (NULL_OR_B58(B), be_block_handler:null_or_b58(B)).

-define (BIN_TO_B64(B), list_to_binary(base64:encode_to_string((B)))).
-define (NULL_OR_B64(B), be_block_handler:null_or_b64(B)).


-define (NULL_OR_VALUE(V), be_block_handler:null_or_value(V)).
-define (NULL_OR_H3(B), be_block_handler:null_or_h3(B)).
