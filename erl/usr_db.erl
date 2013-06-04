-module(usr_db).
-include("usr.hrl").
-export([create_tables/1, close_tables/0, add_usr/1, lookup_id/1,lookup_msisdn/1, update_usr/1, restore_backup/0]).

create_tables(FileName) ->
    ets:new(usrRam, [named_table, {keypos, #usr.msisdn}]),
    ets:new(usrIndex,[named_table]),
    dets:open_file(usrDisk, [{file, FileName}, {keypos, #usr.msisdn}]).

close_tables() ->
    ets:delete(usrRam),
    ets:delete(usrIndex),
    dets:close(usrDisk).

add_usr(#usr{msisdn=PhoneNo, id=CustId} = Usr) ->
    ets:insert(usrIndex, {CustId, PhoneNo}),
    update_usr(Usr).

update_usr(Usr) ->
    ets:insert(usrRam, Usr),
    dets:insert(usrDisk, Usr),
    ok.

lookup_id(CustId) ->
    case get_index(CustId) of
	{ok,PhoneNo} -> lookup_msisdn(PhoneNo);
	{error,instance} -> {error, instance}
    end.

lookup_msisdn(PhoneNo) ->
    case ets:lookup(usrRam, PhoneNo) of
	[Usr] -> {ok, Usr};
	[]    -> {error, instance}
    end.

get_index(CustId) ->
    case ets:lookup(usrIndex, CustId) of
	[{CustId,PhoneNo}] -> {ok,PhoneNo};
	[]                 -> {error,instance}
    end.
	    
	    
restore_backup() ->	    
    Insert = fun(#usr{msisdn=PhoneNo,id=CustId} = Usr) ->
		     ets:insert(usrIndex, {CustId, PhoneNo}),
		     ets:insert(usrRam, Usr),
		     continue
	     end,
    dets:traverse(usrDisk,Insert).
