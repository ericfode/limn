//! Constraint solver for Limn
//!
//! Implements bidirectional constraint propagation to solve Limn programs.
//! Given a program and key (input bindings), computes values for all variables.

use std::collections::{HashMap, HashSet};
use crate::ast::*;
use crate::error::{LimnError, Result};
use crate::value::Value;

/// Environment holding variable bindings
#[derive(Debug, Clone)]
pub struct Environment {
    bindings: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            bindings: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.bindings.get(name)
    }

    pub fn set(&mut self, name: impl Into<String>, value: Value) {
        self.bindings.insert(name.into(), value);
    }

    pub fn is_bound(&self, name: &str) -> bool {
        self.bindings.get(name).map(|v| v.is_bound()).unwrap_or(false)
    }

    pub fn bindings(&self) -> &HashMap<String, Value> {
        &self.bindings
    }

    pub fn into_bindings(self) -> HashMap<String, Value> {
        self.bindings
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

/// Solver for Limn constraint satisfaction
pub struct Solver {
    max_iterations: usize,
    verbose: bool,
}

impl Solver {
    pub fn new() -> Self {
        Solver {
            max_iterations: 1000,
            verbose: false,
        }
    }

    pub fn with_max_iterations(mut self, n: usize) -> Self {
        self.max_iterations = n;
        self
    }

    pub fn with_verbose(mut self, v: bool) -> Self {
        self.verbose = v;
        self
    }

    /// Solve a program with the given key bindings
    pub fn solve(&self, program: &Program, key: &Key) -> Result<HashMap<String, Value>> {
        let mut env = Environment::new();

        // Initialize all variables as unbound
        for var in &program.variables {
            env.set(&var.name, Value::Unbound);
        }

        // Apply key bindings
        for binding in &key.bindings {
            env.set(&binding.variable, binding.value.clone());
        }

        // Iterative constraint propagation
        let mut changed = true;
        let mut iterations = 0;

        while changed && iterations < self.max_iterations {
            changed = false;
            iterations += 1;

            if self.verbose {
                eprintln!("Iteration {}", iterations);
            }

            for constraint in &program.constraints {
                if self.solve_constraint(constraint, &mut env)? {
                    changed = true;
                }
            }
        }

        if iterations >= self.max_iterations {
            return Err(LimnError::solver("Max iterations reached - possible infinite loop"));
        }

        // Verify all constraints are satisfied
        self.verify_constraints(&program.constraints, &env)?;

        // Filter out unbound variables for output
        Ok(env.into_bindings()
            .into_iter()
            .filter(|(_, v)| v.is_bound())
            .collect())
    }

    /// Try to solve a single constraint, return true if any progress was made
    fn solve_constraint(&self, constraint: &Constraint, env: &mut Environment) -> Result<bool> {
        match &constraint.kind {
            ConstraintKind::Equals { left, right } => {
                self.solve_equals(left, right, constraint.negated, env)
            }
            ConstraintKind::GreaterThan { left, right } => {
                // Greater than constraints are checked, not solved
                Ok(false)
            }
            ConstraintKind::LessThan { left, right } => {
                // Less than constraints are checked, not solved
                Ok(false)
            }
            ConstraintKind::Conditional { condition, consequent, alternative } => {
                self.solve_conditional(condition, consequent, alternative.as_deref(), env)
            }
            ConstraintKind::FunctionCall { function, args, result } => {
                self.solve_function_call(function, args, result, env)
            }
            ConstraintKind::ElementOf { element, collection } => {
                // Element-of constraints are checked, not solved
                Ok(false)
            }
        }
    }

    /// Solve an equality constraint bidirectionally
    fn solve_equals(&self, left: &Expr, right: &Expr, negated: bool, env: &mut Environment) -> Result<bool> {
        let left_val = self.evaluate(left, env);
        let right_val = self.evaluate(right, env);

        // If negated, we can't propagate (only check)
        if negated {
            return Ok(false);
        }

        match (left_val, right_val) {
            // Both bound - nothing to do
            (Some(_), Some(_)) => Ok(false),

            // Left is bound, propagate to right
            (Some(val), None) => {
                self.assign(right, val, env)
            }

            // Right is bound, propagate to left
            (None, Some(val)) => {
                self.assign(left, val, env)
            }

            // Neither bound - try to solve bidirectionally
            (None, None) => {
                // Try to solve arithmetic expressions bidirectionally
                if let Some(changed) = self.solve_binary_bidirectional(left, right, env)? {
                    return Ok(changed);
                }
                if let Some(changed) = self.solve_binary_bidirectional(right, left, env)? {
                    return Ok(changed);
                }
                Ok(false)
            }
        }
    }

    /// Solve binary expressions bidirectionally
    ///
    /// For a constraint like `a joi b sa result`:
    /// - If a and b are known, compute result
    /// - If a and result are known, compute b
    /// - If b and result are known, compute a
    fn solve_binary_bidirectional(&self, expr: &Expr, target: &Expr, env: &mut Environment) -> Result<Option<bool>> {
        match expr {
            Expr::Binary { op, left, right } => {
                let left_val = self.evaluate(left, env);
                let right_val = self.evaluate(right, env);
                let target_val = self.evaluate(target, env);

                match (left_val, right_val, target_val) {
                    // Forward: both operands known, compute result
                    (Some(l), Some(r), None) => {
                        if let Some(result) = self.compute_binary(*op, &l, &r) {
                            self.assign(target, result, env)?;
                            return Ok(Some(true));
                        }
                    }

                    // Backward: left and result known, compute right
                    (Some(l), None, Some(t)) => {
                        if let Some(result) = self.compute_inverse_right(*op, &l, &t) {
                            self.assign(right, result, env)?;
                            return Ok(Some(true));
                        }
                    }

                    // Backward: right and result known, compute left
                    (None, Some(r), Some(t)) => {
                        if let Some(result) = self.compute_inverse_left(*op, &r, &t) {
                            self.assign(left, result, env)?;
                            return Ok(Some(true));
                        }
                    }

                    _ => {}
                }
                Ok(None)
            }
            _ => Ok(None),
        }
    }

    /// Compute forward binary operation
    fn compute_binary(&self, op: BinaryOp, left: &Value, right: &Value) -> Option<Value> {
        match op {
            BinaryOp::Add => left.add(right),
            BinaryOp::Sub => left.sub(right),
            BinaryOp::Mul => left.mul(right),
            BinaryOp::Div => left.div(right),
            BinaryOp::Pow => left.pow(right),
            BinaryOp::Root => left.root(right),
            BinaryOp::Concat => left.add(right), // String concat uses add
        }
    }

    /// Compute inverse to find right operand: left op ? = result
    fn compute_inverse_right(&self, op: BinaryOp, left: &Value, result: &Value) -> Option<Value> {
        match op {
            BinaryOp::Add => result.sub(left),      // a + ? = r  =>  ? = r - a
            BinaryOp::Sub => left.sub(result),      // a - ? = r  =>  ? = a - r
            BinaryOp::Mul => result.div(left),      // a * ? = r  =>  ? = r / a
            BinaryOp::Div => left.div(result),      // a / ? = r  =>  ? = a / r
            BinaryOp::Pow => {
                // a ^ ? = r  =>  ? = log_a(r) = ln(r) / ln(a)
                let a = left.as_f64()?;
                let r = result.as_f64()?;
                if a <= 0.0 || a == 1.0 || r <= 0.0 {
                    return None;
                }
                Some(Value::Float(r.ln() / a.ln()))
            }
            BinaryOp::Root => {
                // a root ? = r  =>  ? = a ^ (1/r) ... not standard, skip
                None
            }
            BinaryOp::Concat => None, // Can't invert string concat
        }
    }

    /// Compute inverse to find left operand: ? op right = result
    fn compute_inverse_left(&self, op: BinaryOp, right: &Value, result: &Value) -> Option<Value> {
        match op {
            BinaryOp::Add => result.sub(right),     // ? + b = r  =>  ? = r - b
            BinaryOp::Sub => result.add(right),     // ? - b = r  =>  ? = r + b
            BinaryOp::Mul => result.div(right),     // ? * b = r  =>  ? = r / b
            BinaryOp::Div => result.mul(right),     // ? / b = r  =>  ? = r * b
            BinaryOp::Pow => {
                // ? ^ b = r  =>  ? = r ^ (1/b)
                let b = right.as_f64()?;
                let r = result.as_f64()?;
                if b == 0.0 {
                    return None;
                }
                Some(Value::Float(r.powf(1.0 / b)))
            }
            BinaryOp::Root => {
                // ? root b = r  =>  ? = r ^ b
                result.pow(right)
            }
            BinaryOp::Concat => None,
        }
    }

    /// Solve a conditional constraint
    fn solve_conditional(
        &self,
        condition: &Expr,
        consequent: &[Constraint],
        alternative: Option<&[Constraint]>,
        env: &mut Environment,
    ) -> Result<bool> {
        if let Some(cond_val) = self.evaluate(condition, env) {
            if cond_val.is_truthy() {
                // Condition is true, solve consequent
                let mut changed = false;
                for c in consequent {
                    if self.solve_constraint(c, env)? {
                        changed = true;
                    }
                }
                return Ok(changed);
            } else if let Some(alt) = alternative {
                // Condition is false, solve alternative
                let mut changed = false;
                for c in alt {
                    if self.solve_constraint(c, env)? {
                        changed = true;
                    }
                }
                return Ok(changed);
            }
        }
        Ok(false)
    }

    /// Solve a function call constraint
    fn solve_function_call(
        &self,
        _function: &str,
        _args: &[Expr],
        _result: &str,
        _env: &mut Environment,
    ) -> Result<bool> {
        // Built-in functions would be handled here
        // For now, function calls are not solved by the core solver
        Ok(false)
    }

    /// Evaluate an expression if all variables are bound
    fn evaluate(&self, expr: &Expr, env: &Environment) -> Option<Value> {
        match expr {
            Expr::Variable(name) => {
                env.get(name).filter(|v| v.is_bound()).cloned()
            }

            Expr::Literal(value) => Some(value.clone()),

            Expr::Binary { op, left, right } => {
                let l = self.evaluate(left, env)?;
                let r = self.evaluate(right, env)?;
                self.compute_binary(*op, &l, &r)
            }

            Expr::Unary { op, operand } => {
                let val = self.evaluate(operand, env)?;
                match op {
                    UnaryOp::Negate => val.negate(),
                }
            }

            Expr::Group(elements) => {
                let vals: Option<Vec<Value>> = elements
                    .iter()
                    .map(|e| self.evaluate(e, env))
                    .collect();
                vals.map(Value::Group)
            }

            Expr::Index { index, collection } => {
                let idx = self.evaluate(index, env)?;
                let coll = self.evaluate(collection, env)?;
                if let (Value::Integer(i), Value::Group(items)) = (idx, coll) {
                    items.get(i as usize).cloned()
                } else {
                    None
                }
            }

            Expr::Size(collection) => {
                let coll = self.evaluate(collection, env)?;
                if let Value::Group(items) = coll {
                    Some(Value::Integer(items.len() as i64))
                } else {
                    None
                }
            }

            Expr::First(collection) => {
                let coll = self.evaluate(collection, env)?;
                if let Value::Group(items) = coll {
                    items.first().cloned()
                } else {
                    None
                }
            }

            Expr::Rest(collection) => {
                let coll = self.evaluate(collection, env)?;
                if let Value::Group(items) = coll {
                    Some(Value::Group(items.into_iter().skip(1).collect()))
                } else {
                    None
                }
            }

            Expr::Last(collection) => {
                let coll = self.evaluate(collection, env)?;
                if let Value::Group(items) = coll {
                    items.last().cloned()
                } else {
                    None
                }
            }

            Expr::Conditional { condition, then_branch, else_branch } => {
                let cond = self.evaluate(condition, env)?;
                if cond.is_truthy() {
                    self.evaluate(then_branch, env)
                } else if let Some(else_branch) = else_branch {
                    self.evaluate(else_branch, env)
                } else {
                    None
                }
            }

            Expr::Call { function: _, args: _ } => {
                // Function calls not evaluated in core solver
                None
            }
        }
    }

    /// Assign a value to an expression (for propagation)
    fn assign(&self, expr: &Expr, value: Value, env: &mut Environment) -> Result<bool> {
        match expr {
            Expr::Variable(name) => {
                if !env.is_bound(name) {
                    env.set(name, value);
                    Ok(true)
                } else {
                    // Already bound - check consistency
                    let existing = env.get(name).unwrap();
                    if existing.eq(&value) {
                        Ok(false)
                    } else {
                        Err(LimnError::ConstraintViolation(format!(
                            "Variable '{}' already bound to {} but trying to assign {}",
                            name, existing, value
                        )))
                    }
                }
            }
            _ => {
                // Can't assign to complex expressions
                Ok(false)
            }
        }
    }

    /// Verify all constraints are satisfied
    fn verify_constraints(&self, constraints: &[Constraint], env: &Environment) -> Result<()> {
        for constraint in constraints {
            self.verify_constraint(constraint, env)?;
        }
        Ok(())
    }

    fn verify_constraint(&self, constraint: &Constraint, env: &Environment) -> Result<()> {
        match &constraint.kind {
            ConstraintKind::Equals { left, right } => {
                if let (Some(l), Some(r)) = (self.evaluate(left, env), self.evaluate(right, env)) {
                    let equal = l.eq(&r);
                    let should_be_equal = !constraint.negated;
                    if equal != should_be_equal {
                        return Err(LimnError::ConstraintViolation(format!(
                            "Constraint at line {} violated: {} {} {}",
                            constraint.line,
                            l,
                            if constraint.negated { "nu sa" } else { "sa" },
                            r
                        )));
                    }
                }
            }

            ConstraintKind::GreaterThan { left, right } => {
                if let (Some(l), Some(r)) = (self.evaluate(left, env), self.evaluate(right, env)) {
                    let gt = l.gt(&r).unwrap_or(false);
                    let should_be_gt = !constraint.negated;
                    if gt != should_be_gt {
                        return Err(LimnError::ConstraintViolation(format!(
                            "Constraint at line {} violated: {} {} {}",
                            constraint.line,
                            l,
                            if constraint.negated { "nu ma" } else { "ma" },
                            r
                        )));
                    }
                }
            }

            ConstraintKind::LessThan { left, right } => {
                if let (Some(l), Some(r)) = (self.evaluate(left, env), self.evaluate(right, env)) {
                    let lt = l.lt(&r).unwrap_or(false);
                    let should_be_lt = !constraint.negated;
                    if lt != should_be_lt {
                        return Err(LimnError::ConstraintViolation(format!(
                            "Constraint at line {} violated: {} {} {}",
                            constraint.line,
                            l,
                            if constraint.negated { "nu mi" } else { "mi" },
                            r
                        )));
                    }
                }
            }

            ConstraintKind::Conditional { condition, consequent, alternative } => {
                if let Some(cond_val) = self.evaluate(condition, env) {
                    if cond_val.is_truthy() {
                        self.verify_constraints(consequent, env)?;
                    } else if let Some(alt) = alternative {
                        self.verify_constraints(alt, env)?;
                    }
                }
            }

            ConstraintKind::FunctionCall { .. } => {
                // Function calls verified separately
            }

            ConstraintKind::ElementOf { element, collection } => {
                if let (Some(elem), Some(coll)) = (self.evaluate(element, env), self.evaluate(collection, env)) {
                    if let Value::Group(items) = coll {
                        let contains = items.iter().any(|item| item.eq(&elem));
                        let should_contain = !constraint.negated;
                        if contains != should_contain {
                            return Err(LimnError::ConstraintViolation(format!(
                                "Constraint at line {} violated: {} {} collection",
                                constraint.line,
                                elem,
                                if constraint.negated { "nu amo" } else { "amo" }
                            )));
                        }
                    }
                }
            }
        }
        Ok(())
    }
}

impl Default for Solver {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_program(vars: &[&str], constraints: Vec<Constraint>) -> Program {
        Program {
            name: "test".to_string(),
            variables: vars.iter().map(|&v| Variable::new(v)).collect(),
            constraints,
            imports: Vec::new(),
        }
    }

    fn make_key(bindings: &[(&str, Value)]) -> Key {
        let mut key = Key::new();
        for (name, value) in bindings {
            key.bindings.push(KeyBinding {
                variable: name.to_string(),
                value: value.clone(),
            });
        }
        key
    }

    #[test]
    fn test_simple_addition() {
        // x joi y sa result
        let constraints = vec![
            Constraint {
                kind: ConstraintKind::Equals {
                    left: Expr::binary(BinaryOp::Add, Expr::var("x"), Expr::var("y")),
                    right: Expr::var("result"),
                },
                negated: false,
                line: 1,
            }
        ];

        let program = make_program(&["x", "y", "result"], constraints);
        let key = make_key(&[("x", Value::Integer(5)), ("y", Value::Integer(3))]);

        let solver = Solver::new();
        let result = solver.solve(&program, &key).unwrap();

        assert_eq!(result.get("result"), Some(&Value::Integer(8)));
    }

    #[test]
    fn test_backward_solving() {
        // x joi y sa result - solve for y given x and result
        let constraints = vec![
            Constraint {
                kind: ConstraintKind::Equals {
                    left: Expr::binary(BinaryOp::Add, Expr::var("x"), Expr::var("y")),
                    right: Expr::var("result"),
                },
                negated: false,
                line: 1,
            }
        ];

        let program = make_program(&["x", "y", "result"], constraints);
        let key = make_key(&[("x", Value::Integer(5)), ("result", Value::Integer(12))]);

        let solver = Solver::new();
        let result = solver.solve(&program, &key).unwrap();

        assert_eq!(result.get("y"), Some(&Value::Integer(7)));
    }

    #[test]
    fn test_chained_constraints() {
        // a joi b sa t1
        // t1 exp c sa result
        let constraints = vec![
            Constraint {
                kind: ConstraintKind::Equals {
                    left: Expr::binary(BinaryOp::Add, Expr::var("a"), Expr::var("b")),
                    right: Expr::var("t1"),
                },
                negated: false,
                line: 1,
            },
            Constraint {
                kind: ConstraintKind::Equals {
                    left: Expr::binary(BinaryOp::Mul, Expr::var("t1"), Expr::var("c")),
                    right: Expr::var("result"),
                },
                negated: false,
                line: 2,
            },
        ];

        let program = make_program(&["a", "b", "c", "t1", "result"], constraints);
        let key = make_key(&[
            ("a", Value::Integer(2)),
            ("b", Value::Integer(3)),
            ("c", Value::Integer(4)),
        ]);

        let solver = Solver::new();
        let result = solver.solve(&program, &key).unwrap();

        assert_eq!(result.get("t1"), Some(&Value::Integer(5)));
        assert_eq!(result.get("result"), Some(&Value::Integer(20)));
    }
}
