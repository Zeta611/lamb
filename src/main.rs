use lamb::*;

fn main() {
    let e = parse("(\\x.x) (\\y.y) (\\x.\\y.x) a b");
    println!("e = {:?}", e);
    println!("FV(e) = {:?}", free_var(&e));
    println!("closed(e) = {:?}", closed(&e));
    println!("eval(e) = {:?}", beta_red(e, 0));
}
