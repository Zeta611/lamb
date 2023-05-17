# 🐑

## Example
```rust
let e = parse("((\\x.x) (\\y.y)) z");
println!("e = {:?}", e);
println!("FV(e) = {:?}", free_var(&e));
println!("closed(e) = {:?}", closed(&e));
println!("eval(e) = {:?}", beta_red(&e, 0));
```

```
e = App(App(Lamb("x", Var("x")), Lamb("y", Var("y"))), Var("z"))
FV(e) = {"z"}
closed(e) = false
eval(e) = (Var("z"), 0)
```
