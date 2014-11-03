{application, tinypdf,
 [
  {description, "Tiny PDF"},
  {vsn, "0.0.1"},
  {modules, []},
  {registered, [tinypdf_sup]},
  {applications, [kernel, stdlib]},
  {mod, {tinypdf_app, []}},
  {env, []}
 ]
}.
