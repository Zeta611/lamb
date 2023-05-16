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
}
