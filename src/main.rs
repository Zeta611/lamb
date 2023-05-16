use lamb::*;

fn main() {
    let x = Expr::Var(String::from("x"));
    let e1 = Expr::Lamb(String::from("x"), Box::new(x));
    // copy e1 into e2
    let e2 = e1.clone();
    let e = Expr::App(Box::new(e1), Box::new(e2));
    println!("e = {:?}", e);
    println!("FV(e) = {:?}", free_var(&e));
    println!("closed(e) = {:?}", closed(&e));
}
