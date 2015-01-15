elarm_mailer
============


Quick start
-----------
Take a look at `priv/elarm_mailer.app.dev` for a configuration example.  
`make devrun` will start up the application with the settings copied over from `priv/elarm_mailer.app.dev.


Testing
-------
`make test-deps test` will run the automated tests.

Configuration
-------------

```
{env,[{sender, "monitoring.tool@monitoring.example.com"},
      {recipients, ["admin@hq.example.com", "devops@devops.example.com]},
      {gen_smtp_options,
       [{relay, "mail.example.com"},
        {username, "monitoring.tool@monitoring.example.com"},
        {password, "foobar123"},
        {port, 25}]},
      {subscribed_alarms,
       [one_potato, two_potato]}
     ]},
```


TODO
----

Email formatting -- currently the email body is just a bunch of Erlang terms.
