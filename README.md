Palma
=====


Palma is an erlang application to dynamically create pools of erlang worker processes, which can be called in a round-robin fashion using http://github.com/odo/ballermann.

Building
-----

```
git clone git://github.com/odo/palma.git
cd palma
./rebar get-deps compile
```

Usage
-----

We are gonna use the palma_sample_worker which is a simple gen_server that accepts an state on init and can return it along with its pid.

After starting palma itself, we create a pool by provinding a name, a pool size and a supervisor child specifications as described here: http://www.erlang.org/doc/man/supervisor.html#type-child_spec

Now we can retrieve pids from the pool and use them in gen_server:call/2 .
As we see, the different servers from the pool respond in turn.

`erl -pz ebin deps/*/ebin`


```
1> palma:start().
ok
2> palma:new(test_pool1, 3, {my_id, {palma_sample_worker, start_link, [hallo1]}, permanent, 1000, worker, [palma_sample_worker]}).
{ok,<0.43.0>}
3>
=INFO REPORT==== 18-Dec-2013::17:05:09 ===
ballermann: Found 3 new processes of 3 total.
gen_server:call(palma:pid(test_pool1), state).
{<0.41.0>,hallo1}
4> gen_server:call(palma:pid(test_pool1), state).
{<0.40.0>,hallo1}
5> gen_server:call(palma:pid(test_pool1), state).
{<0.42.0>,hallo1}
6> gen_server:call(palma:pid(test_pool1), state).
{<0.41.0>,hallo1}
7> palma:new(test_pool2, 3, {my_id, {palma_sample_worker, start_link, [hallo2]}, permanent, 1000, worker, [palma_sample_worker]}).

=INFO REPORT==== 18-Dec-2013::17:05:29 ===
ballermann: Found 3 new processes of 3 total.
{ok,<0.54.0>}
8> gen_server:call(palma:pid(test_pool2), state).
{<0.53.0>,hallo2}
```