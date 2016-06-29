![palm tree](https://raw.githubusercontent.com/odo/palma/e1b776c94a944ce4ac056458e9292ec112919617/doc/palm_tree.png "palm tree")

# palma


palma is an erlang application to dynamically create pools of erlang worker processes, which can be called in a round-robin fashion using http://github.com/odo/revolver.

## building

```
git clone git://github.com/odo/palma.git
cd palma
./rebar get-deps compile
```

## usage

As an example we are using a pool of `palma_sample_worker` processes. `palma_sample_worker` is a simple gen_server that accepts an state on init and can return it along with its pid.

After starting palma itself, we create a pool by provinding a name, a pool size and a supervisor child specifications as described here: http://www.erlang.org/doc/man/supervisor.html#type-child_spec

Now we can retrieve pids from the pool and use them in gen_server:call/2 .
As we see, the different servers from the pool respond in turn.

`erl -pz ebin deps/*/ebin`

```erlang
1> palma:start().
{ok,[revolver,supervisor2,palma]}
2> palma:new(test_pool1, 3, {my_id, {palma_sample_worker, start_link, [hallo1]}, permanent, 1000, worker, [palma_sample_worker]}).
{ok,<0.46.0>}
3> gen_server:call(palma:pid(test_pool1), state).
{<0.43.0>,hallo1}
4> gen_server:call(palma:pid(test_pool1), state).
{<0.45.0>,hallo1}
5> gen_server:call(palma:pid(test_pool1), state).
{<0.44.0>,hallo1}
6> gen_server:call(palma:pid(test_pool1), state).
{<0.43.0>,hallo1}
7> palma:new(test_pool2, 3, {my_id, {palma_sample_worker, start_link, [hallo2]}, permanent, 1000, worker, [palma_sample_worker]}).
{ok,<0.57.0>}
8> gen_server:call(palma:pid(test_pool2), state).
{<0.54.0>,hallo2}
9> palma:stop(test_pool1).
ok
10> palma:stop(test_pool2).
ok
```

If you are creating pools on the fly, you can also use the syntax where you just ask for the pid and have the pool started implicitly if it's not running:

```erlang
1> palma:start().
2> ChildSpec = {my_id, {palma_sample_worker, start_link, [hallo1]}, permanent, 1000, worker, [palma_sample_worker]}.
3> palma:pid([test_pool1, 3, ChildSpec]).
<0.44.0>
4> palma:pid([test_pool1, 3, ChildSpec]).
<0.43.0>
```

You can also define all the relevant options in a callback module using the `palma_callback` behaviour:

```erlang
1> palma:start().
2> Options = #{name => my_sample_worker, initial_state => my_state}.
3> gen_server:call(palma:pid(palma_sample_worker, Options), state).
doing something global here...
{<0.49.0>,my_state}
4> gen_server:call(palma:pid(palma_sample_worker, Options), state).
{<0.48.0>,my_state}
5> gen_server:call(palma:pid(palma_sample_worker, Options), state).
{<0.42.0>,my_state}
```

## application structure

here is the supervision tree of the example:
![supervision tree](../master/doc/palma_tree.png?raw=true "supervision tree")

