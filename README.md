serval
======

** configure

```erlang
serval_ctl:init_websvc(#{static_prefix => "/static/", api_prefix => "/api/",
			 static_fspath => "/tmp", port => 9000}).
}).

serval_ctl:set_safemodules([calendar]).

```
