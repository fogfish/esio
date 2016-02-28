%%
%%   Copyright (C) 2016 Zalando SE
%%
%%   This software may be modified and distributed under the terms
%%   of the MIT license.  See the LICENSE file for details.
%%

%%
%% default i/o timeout
-define(TIMEOUT, 5000).

%%
%% the namespace identifier used by library
-define(NID, <<"es">>).

%%
%% wild-card urn, matches any indexes
-define(WILDCARD,  {urn, ?NID, <<"*">>}).

%%
%% number of elements to read
-define(CONFIG_STREAM_CHUNK,  100).

%%
%% number of elements to write
-define(CONFIG_BULK_CHUNK,    500).

%%
%% default timeout to sync buffers
-define(CONFIG_T_SYNC,      60000).
