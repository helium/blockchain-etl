-module(be_peer_status).

-include("be_db_follower.hrl").

-export([
    peer_last_challenge/2,
    peer_time/2,
    peer_metadata/3,
    peer_height/2,
    peer_listen_addrs/2,
    peer_release_version/2
]).

%% A peer is considered stale if it's peerbook entry is more than 24 hours old
-define(STALE_PEER_TIME, 24 * 3600000).

-spec peer_last_challenge(libp2p_crypto:pubkey_bin(), blockchain_ledger_v1:ledger()) ->
    undefined | pos_integer().
peer_last_challenge(Address, Ledger) ->
    case blockchain_ledger_v1:find_gateway_info(Address, Ledger) of
        {error, _} -> undefined;
        {ok, GWInfo} -> blockchain_ledger_gateway_v2:last_poc_challenge(GWInfo)
    end.

peer_time(Address, PeerBook) ->
    case libp2p_peerbook:get(PeerBook, Address) of
        {ok, Peer} ->
            libp2p_peer:timestamp(Peer);
        {error, _} ->
            undefined
    end.

peer_metadata(Key, Address, PeerBook) ->
    case libp2p_peerbook:get(PeerBook, Address) of
        {ok, Peer} ->
            libp2p_peer:signed_metadata_get(Peer, Key, undefined);
        {error, _} ->
            undefined
    end.

peer_height(Address, PeerBook) ->
    case peer_metadata(<<"height">>, Address, PeerBook) of
        undefined ->
            undefined;
        Height when is_integer(Height) ->
            Height;
        Other ->
            lager:warning("Invalid block height for gateway ~s: ~p", [
                ?BIN_TO_B58(Address),
                Other
            ]),
            undefined
    end.

-spec peer_listen_addrs(libp2p_crypto:pubkey_bin(), libp2p_peerbook:peerbook()) ->
    [binary()] | undefined.
peer_listen_addrs(Address, PeerBook) ->
    case libp2p_peerbook:get(PeerBook, Address) of
        {ok, Peer} ->
            [list_to_binary(A) || A <- libp2p_peer:listen_addrs(Peer)];
        {error, _} ->
            undefined
    end.

-spec peer_release_version(libp2p_crypto:pubkey_bin(), libp2p_peerbook:peerbook()) ->
    binary() | undefined.
peer_release_version(Address, PeerBook) ->
    peer_metadata(<<"release_version">>, Address, PeerBook).
