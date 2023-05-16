use std::collections::HashSet;

#[derive(Debug, Clone)]
pub enum Expr {
    Var(String),
    Lamb(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
}

/// Finds free variables of a given expression.
pub fn free_var(e: &Expr) -> HashSet<&str> {
    match e {
        // FV(x) = {x}
        Expr::Var(x) => HashSet::from([x.as_str()]),

        // FV(\x.e) = FV(e) - {x}
        Expr::Lamb(x, e_body) => free_var(e_body)
            .difference(&HashSet::from([x.as_str()]))
            .map(|x| x.to_owned())
            .collect(),

        // FV(e0 e1) = FV(e0) \cup FV(e1)
        Expr::App(e0, e1) => free_var(e0)
            .union(&free_var(e1))
            .map(|x| x.to_owned())
            .collect(),
    }
}

/// Checks if free variables do not occur in a given expression.
pub fn closed(e: &Expr) -> bool {
    free_var(e).is_empty()
}

/// Safely substitutes x with e_x in e.
pub fn subst(e: &Expr, x: &str, e_x: &Expr, gen_cnt: u32) -> (Expr, u32) {
    match e {
        // x{e_x/x} = e_x
        Expr::Var(y) if y == x => (e_x.clone(), gen_cnt),
        // y{e_x/x} = e_x where y != x
        Expr::Var(y) => (Expr::Var(y.clone()), gen_cnt),

        // (\x.e0){e_x/x} = \x.e0
        Expr::Lamb(y, _) if y == x => (e.clone(), gen_cnt),
        // (\y.e0){e_x/x} = \y.(e0{e_x/x}) where y != x & y not in FV(e_x)
        Expr::Lamb(y, e0) if y != x && !free_var(e_x).contains(y.as_str()) => {
            let (e0_subst, gen_cnt) = subst(e0, x, e_x, gen_cnt);
            (Expr::Lamb(y.clone(), Box::new(e0_subst)), gen_cnt)
        }
        // (\y.e0){e_x/x} = \z.(e0{z/y}{e_x/x}) where y != x /\ y in FV(e_x)
        Expr::Lamb(y, e0) => {
            let z = format!("${}", gen_cnt);
            let gen_cnt = gen_cnt + 1;
            let (e0, gen_cnt) = subst(e0, y, &Expr::Var(z.clone()), gen_cnt);
            let (e0, gen_cnt) = subst(&e0, x, e_x, gen_cnt);
            (Expr::Lamb(z, Box::new(e0)), gen_cnt)
        }

        // (e0 e1){e_x/x} = (e0{e_x/x} e1{e_x/x})
        Expr::App(e0, e1) => {
            let (e0, gen_cnt) = subst(e0, x, e_x, gen_cnt);
            let (e1, gen_cnt) = subst(e1, x, e_x, gen_cnt);
            (Expr::App(Box::new(e0), Box::new(e1)), gen_cnt)
        }
    }
}

pub fn beta_red(e: &Expr, gen_cnt: u32) -> (Expr, u32) {
    match e {
        // (\x.e0) e1 -> e0{e1/x}
        Expr::App(e0, e1) => match e0.as_ref() {
            Expr::Lamb(x, e0) => subst(e0, x, e1, gen_cnt),
            _ => panic!("e0 is not a lambda expression"),
        },
        _ => (e.clone(), gen_cnt),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn free_var_x_is_x() {
        let x = Expr::Var(String::from("x"));
        assert!(free_var(&x).contains("x"));
    }

    #[test]
    fn free_var_abc_is_abc() {
        let abc = Expr::Var(String::from("abc"));
        assert!(free_var(&abc).contains("abc"));
    }

    #[test]
    fn free_var_lamb() {
        let lamb = Expr::Lamb(String::from("x"), Box::new(Expr::Var(String::from("y"))));
        assert!(free_var(&lamb).contains("y"));
    }

    #[test]
    fn free_var_app() {
        let e = Expr::App(
            Box::new(Expr::Lamb(
                String::from("x"),
                Box::new(Expr::Var(String::from("x"))),
            )),
            Box::new(Expr::Var(String::from("x"))),
        );
        assert!(free_var(&e).contains("x"));
    }

    #[test]
    fn closed_x_is_false() {
        let x = Expr::Var(String::from("x"));
        assert!(!closed(&x));
    }

    #[test]
    fn closed_lam_x_is_false() {
        let lam = Expr::Lamb(String::from("x"), Box::new(Expr::Var(String::from("x"))));
        assert!(closed(&lam));
    }

    #[test]
    fn subst_x_for_y_in_x_is_x() {
        let x = Expr::Var(String::from("x"));
        let y = Expr::Var(String::from("y"));
        let (e, _) = subst(&x, "x", &y, 0);
        assert!(matches!(e, Expr::Var(x) if x == "y"));
    }

    #[test]
    fn subst_x_for_y_in_app() {
        let x = Expr::Var(String::from("x"));
        let y = Expr::Var(String::from("y"));
        // e = (\x.x) x
        let e = Expr::App(
            Box::new(Expr::Lamb(String::from("x"), Box::new(x.clone()))),
            Box::new(x.clone()),
        );
        let (e, _) = subst(&e, "x", &y, 0);
        assert!(
            matches!(e, Expr::App(e0, e1) if matches!(&*e0, Expr::Lamb(x, e) if x == "x" && matches!(&**e, Expr::Var(x) if x == "x")) && matches!(&*e1, Expr::Var(x) if x == "y"))
        );
    }

    #[test]
    fn subst_x_for_y_in_lam() {
        // (\y.((z x) y)) {z/x} = \y.((z z) y)
        let x = Expr::Var(String::from("x"));
        let y = Expr::Var(String::from("y"));
        let z = Expr::Var(String::from("z"));
        let e = Expr::Lamb(
            String::from("y"),
            Box::new(Expr::App(
                Box::new(Expr::App(Box::new(z.clone()), Box::new(x.clone()))),
                Box::new(y.clone()),
            )),
        );
        let (e, _) = subst(&e, "x", &z, 0);
        assert!(
            matches!(e, Expr::Lamb(y, e) if y == "y" && matches!(&*e, Expr::App(e_zx, e_y) if matches!(&**e_zx, Expr::App(e_z, e_x) if matches!(&**e_z, Expr::Var(z) if z == "z") && matches!(&**e_x, Expr::Var(x) if x == "z")) && matches!(&**e_y, Expr::Var(y) if y == "y")))
        );
    }

    #[test]
    fn subst_x_for_y_in_lam2() {
        // (\y.((z x) y)) {y/x} = \z'.((z y) z')
        let x = Expr::Var(String::from("x"));
        let y = Expr::Var(String::from("y"));
        let z = Expr::Var(String::from("z"));
        let e = Expr::Lamb(
            String::from("y"),
            Box::new(Expr::App(
                Box::new(Expr::App(Box::new(z.clone()), Box::new(x.clone()))),
                Box::new(y.clone()),
            )),
        );
        let (e, _) = subst(&e, "x", &y, 0);
        assert!(
            matches!(e, Expr::Lamb(y, e) if y == "$0" && matches!(&*e, Expr::App(e_zx, e_y) if matches!(&**e_zx, Expr::App(e_z, e_x) if matches!(&**e_z, Expr::Var(z) if z == "z") && matches!(&**e_x, Expr::Var(x) if x == "y")) && matches!(&**e_y, Expr::Var(y) if y == "$0")))
        );
    }

    #[test]
    fn beta_red_is_correct() {
        let x = Expr::Var(String::from("x"));
        let y = Expr::Var(String::from("y"));
        let z = Expr::Var(String::from("z"));
        // e = \z.\x.\y.((z x) y)
        let e = Expr::Lamb(
            String::from("z"),
            Box::new(Expr::Lamb(
                String::from("x"),
                Box::new(Expr::Lamb(
                    String::from("y"),
                    Box::new(Expr::App(
                        Box::new(Expr::App(Box::new(z.clone()), Box::new(x.clone()))),
                        Box::new(y.clone()),
                    )),
                )),
            )),
        );
        let (e, _) = beta_red(&Expr::App(Box::new(e), Box::new(x)), 0);
        // (\z.\x.\y.((z x) y)) x -> \$0.\y.((x $0) y)))
        assert!(
            matches!(e, Expr::Lamb(x, e) if x == "$0" && matches!(&*e, Expr::Lamb(y, e) if y == "y" && matches!(&**e, Expr::App(e_zx, e_y) if matches!(&**e_zx, Expr::App(e_z, e_x) if matches!(&**e_z, Expr::Var(z) if z == "x") && matches!(&**e_x, Expr::Var(x) if x == "$0")) && matches!(&**e_y, Expr::Var(y) if y == "y"))))
        );
    }
}
