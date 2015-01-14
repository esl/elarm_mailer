{application,elarm_mailer,
             [{description,[]},
              {vsn,"1"},
              {registered,[]},
              {applications,[kernel,stdlib,elarm]},
              {mod,{elarm_mailer_app,[]}},
              {env,[
                    {sendmail_command, "sendmail -vt"},
                    {sendmail_headers, [{from, "simon.zelazny@locahost"},
                                        {to, "simon.zelazny@localhost"}]},
                    {subscribed_alarms, [foo]}
                   ]},
              {modules,[elarm_mailer,elarm_mailer_app,elarm_mailer_sendmail,
                        elarm_mailer_sup]}]}.
