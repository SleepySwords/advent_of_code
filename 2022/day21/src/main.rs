use std::{collections::HashMap, error::Error};

use advent_of_code_lib::{self, Solver};

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2022", "21")
}

struct Day;

#[derive(Debug)]
enum Operation<'a> {
    Add(&'a str, &'a str),
    Subtract(&'a str, &'a str),
    Multiply(&'a str, &'a str),
    Divide(&'a str, &'a str),
    Yell(u64),
}

fn dfs(monkeys: &HashMap<&str, Operation>, operation: &Operation) -> u64 {
    match operation {
        Operation::Add(m1, m2) => {
            dfs(monkeys, monkeys.get(m1).unwrap()) + dfs(monkeys, monkeys.get(m2).unwrap())
        }
        Operation::Subtract(m1, m2) => {
            dfs(monkeys, monkeys.get(m1).unwrap()) - dfs(monkeys, monkeys.get(m2).unwrap())
        }
        Operation::Multiply(m1, m2) => {
            dfs(monkeys, monkeys.get(m1).unwrap()) * dfs(monkeys, monkeys.get(m2).unwrap())
        }
        Operation::Divide(m1, m2) => {
            dfs(monkeys, monkeys.get(m1).unwrap()) / dfs(monkeys, monkeys.get(m2).unwrap())
        }
        Operation::Yell(yell) => *yell,
    }
}

fn humn_path<'a>(
    monkeys: &HashMap<&'a str, Operation<'a>>,
    name: &'a str,
) -> Option<Vec<&'a str>> {
    if name == "humn" {
        return Some(vec!["humn"]);
    }
    match monkeys.get(name).unwrap() {
        Operation::Add(m1, m2) => {
            if let Some(mut v) = humn_path(monkeys, m1) {
                v.push(name);
                return Some(v);
            }
            if let Some(mut v) = humn_path(monkeys, m2) {
                v.push(name);
                return Some(v);
            }
        }
        Operation::Subtract(m1, m2) => {
            if let Some(mut v) = humn_path(monkeys, m1) {
                v.push(name);
                return Some(v);
            }
            if let Some(mut v) = humn_path(monkeys, m2) {
                v.push(name);
                return Some(v);
            }
        }
        Operation::Multiply(m1, m2) => {
            if let Some(mut v) = humn_path(monkeys, m1) {
                v.push(name);
                return Some(v);
            }
            if let Some(mut v) = humn_path(monkeys, m2) {
                v.push(name);
                return Some(v);
            }
        }
        Operation::Divide(m1, m2) => {
            if let Some(mut v) = humn_path(monkeys, m1) {
                v.push(name);
                return Some(v);
            }
            if let Some(mut v) = humn_path(monkeys, m2) {
                v.push(name);
                return Some(v);
            }
        }
        Operation::Yell(_) => {}
    };
    None
}

fn inverse(
    monkeys: &HashMap<&str, Operation>,
    humn_path: &Vec<&str>,
    operation: &Operation,
    prev_evalutated: i64,
) -> i64 {
    if let Operation::Yell(_) = operation {
        return prev_evalutated;
    } else {
        match operation {
            Operation::Add(m1, m2) => {
                let (evaluted, contains) = if humn_path.contains(m1) {
                    (m2, m1)
                } else {
                    (m1, m2)
                };

                // Could be cached, ie (create a tree)
                let evaluated = dfs(monkeys, monkeys.get(evaluted).unwrap()) as i64;
                // Prev = evaluted + contains => contains = prev - evaluated
                inverse(
                    monkeys,
                    humn_path,
                    monkeys.get(contains).unwrap(),
                    prev_evalutated - evaluated,
                )
            }
            Operation::Multiply(m1, m2) => {
                let (evaluted, contains) = if humn_path.contains(m1) {
                    (m2, m1)
                } else {
                    (m1, m2)
                };

                // Could be cached, ie (create a tree)
                let evaluated = dfs(monkeys, monkeys.get(evaluted).unwrap()) as i64;
                // Prev = evaluted * contains => contains = prev / evaluated
                inverse(
                    monkeys,
                    humn_path,
                    monkeys.get(contains).unwrap(),
                    prev_evalutated / evaluated,
                )
            }
            Operation::Subtract(m1, m2) => {
                if humn_path.contains(m1) {
                    // Prev = contains - evaluated => contains = prev + evaluated
                    inverse(
                        monkeys,
                        humn_path,
                        monkeys.get(m1).unwrap(),
                        prev_evalutated + dfs(monkeys, monkeys.get(m2).unwrap()) as i64,
                    )
                } else {
                    // Prev = evaluated - contains => contains = evaluated - prev
                    inverse(
                        monkeys,
                        humn_path,
                        monkeys.get(m2).unwrap(),
                        dfs(monkeys, monkeys.get(m1).unwrap()) as i64 - prev_evalutated,
                    )
                }
            }
            Operation::Divide(m1, m2) => {
                if humn_path.contains(m1) {
                    // Prev = contains / evaluated => contains = prev * evaluated
                    inverse(
                        monkeys,
                        humn_path,
                        monkeys.get(m1).unwrap(),
                        prev_evalutated * dfs(monkeys, monkeys.get(m2).unwrap()) as i64,
                    )
                } else {
                    // Prev = evaluated / contains => contains = evaluated / prev
                    inverse(
                        monkeys,
                        humn_path,
                        monkeys.get(m2).unwrap(),
                        dfs(monkeys, monkeys.get(m1).unwrap()) as i64 / prev_evalutated,
                    )
                }
            }
            Operation::Yell(_) => todo!(),
        }
    }
}

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        let op = input.lines().fold(HashMap::new(), |mut acc, f| {
            let id = &f[0..4];
            let operation = if f.chars().nth(6).unwrap().is_digit(10) {
                Operation::Yell(f[6..].parse::<u64>().unwrap())
            } else {
                let first = &f[6..10];
                let second = &f[13..17];
                match f.chars().nth(11).unwrap() {
                    '+' => Operation::Add(first, second),
                    '-' => Operation::Subtract(first, second),
                    '*' => Operation::Multiply(first, second),
                    '/' => Operation::Divide(first, second),
                    _ => unreachable!(),
                }
            };
            acc.insert(id, operation);
            acc
        });

        let root = op.get("root").unwrap();
        dfs(&op, root).to_string()
    }

    fn part2(&self, input: &str) -> String {
        let op = input.lines().fold(HashMap::new(), |mut acc, f| {
            let id = &f[0..4];
            let operation = if f.chars().nth(6).unwrap().is_digit(10) {
                Operation::Yell(f[6..].parse::<u64>().unwrap())
            } else {
                let first = &f[6..10];
                let second = &f[13..17];
                match f.chars().nth(11).unwrap() {
                    '+' => Operation::Add(first, second),
                    '-' => Operation::Subtract(first, second),
                    '*' => Operation::Multiply(first, second),
                    '/' => Operation::Divide(first, second),
                    _ => unreachable!(),
                }
            };
            acc.insert(id, operation);
            acc
        });

        let monkey = match op.get("root").unwrap() {
            Operation::Add(m1, m2) => (m1, m2),
            Operation::Subtract(m1, m2) => (m1, m2),
            Operation::Multiply(m1, m2) => (m1, m2),
            Operation::Divide(m1, m2) => (m1, m2),
            Operation::Yell(_) => unreachable!(),
        };
        let humn_path = humn_path(&op, "root").unwrap();
        let (eval, contains) = if humn_path.contains(monkey.0) {
            (monkey.1, monkey.0)
        } else {
            (monkey.0, monkey.1)
        };
        inverse(
            &op,
            &humn_path,
            op.get(contains).unwrap(),
            dfs(&op, op.get(eval).unwrap()) as i64,
        )
        .to_string()
    }
}
