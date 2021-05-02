
# `rebar_sesterl`

## How to use

1. Generate `./rebar.config` from `./package.yaml` by Sesterl:

   ```console
   $ sesterl config ./
   ```

   The command above writes the following description to `rebar.config`:

   ```erlang
   {plugins, [
     {rebar_sesterl, {git, "https://github.com/gfngfn/rebar_sesterl_plugin.git", {branch, "master"}}}
   ]}.

   {src_dirs, ["src", "_generated"]}.
   ```

   Here, `./src` is used not only for putting Sesterl sources but also for `foo.app.src`.

2. Invoke:

   ```console
   $ rebar3 sesterl compile
   ```

   Then, by using `./package.yaml`, Sesterl generates Erlang code in `./_generated`, before `rebar3` compiles the resulting Erlang code.
