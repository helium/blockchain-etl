-define (BIN_TO_B58(B), list_to_binary(libp2p_crypto:bin_to_b58((B)))).
-define (B58_TO_BIN(B), libp2p_crypto:b58_to_bin(binary_to_list((B)))).

-define (BIN_TO_B64(B), base64url:encode((B))).
-define (B64_TO_BIN(B), base64url:decode((B))).

-define (MAYBE_FN(F, V), be_follower:maybe_fn((F), (V))).
-define (MAYBE_UNDEFINED(V), be_follower:maybe_undefined((V))).
-define (MAYBE_B58(B), be_follower:maybe_b58((B))).
-define (MAYBE_B64(B), be_follower:maybe_b64((B))).
-define (MAYBE_H3(B), be_follower:maybe_h3((B))).

-define(CACHE_HEIGHT, height).
-define(CACHE_SYNC, sync).
-define(CACHE, be_cache).
