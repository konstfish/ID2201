https://www.erlang.org/doc/system/getting_started.html



erlc hello.erl

erl -eval "hello:world()." -s init stop

## Interactive

```
erl
1> c(hello).
2> hello:world().
3> q().
```