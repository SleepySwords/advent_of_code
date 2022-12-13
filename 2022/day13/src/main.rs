use std::{cmp::Ordering, error::Error, iter};

use advent_of_code_lib::{self, Solver};
use itertools::Itertools;

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2022", "13")
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Type {
    List(Vec<Type>),
    Integer(usize),
}

impl Type {
    fn is_integer(&self) -> bool {
        if let Self::Integer(_) = self {
            true
        } else {
            false
        }
    }

    fn wrap_list(self) -> Type {
        Self::List(vec![self])
    }
}

#[derive(Debug, PartialEq, Eq)]
enum CompareResult {
    Wrong,
    Right,
    Continue,
}

fn compare(mut left: Type, mut right: Type) -> CompareResult {
    if left.is_integer() && right.is_integer() {
        let left = if let Type::Integer(x) = left {
            x
        } else {
            unreachable!()
        };
        let right = if let Type::Integer(x) = right {
            x
        } else {
            unreachable!()
        };
        return match left.cmp(&right) {
            std::cmp::Ordering::Less => CompareResult::Right,
            std::cmp::Ordering::Equal => CompareResult::Continue,
            std::cmp::Ordering::Greater => CompareResult::Wrong,
        };
    }
    if left.is_integer() {
        left = left.wrap_list();
    }
    if right.is_integer() {
        right = right.wrap_list();
    }

    let mut left = if let Type::List(x) = left {
        x.into_iter()
    } else {
        unreachable!()
    };
    let mut right = if let Type::List(x) = right {
        x.into_iter()
    } else {
        unreachable!()
    };

    for _ in 0..left.len().max(right.len()) {
        if let Some(left) = left.next() {
            if let Some(right) = right.next() {
                match compare(left, right) {
                    CompareResult::Wrong => return CompareResult::Wrong,
                    CompareResult::Right => return CompareResult::Right,
                    CompareResult::Continue => continue,
                }
            } else {
                return CompareResult::Wrong;
            }
        } else {
            return CompareResult::Right;
        }
    }

    CompareResult::Continue
}

struct Day;

fn parse_list<T: Iterator<Item = char>>(chars: &mut T) -> Type {
    let mut list = vec![];
    let mut integer_build = String::from("");
    loop {
        match chars.next().unwrap() {
            '[' => list.push(parse_list(chars)),
            ']' => {
                if !integer_build.is_empty() {
                    list.push(Type::Integer(integer_build.parse().unwrap()));
                }
                return Type::List(list);
            }
            ',' => {
                if !integer_build.is_empty() {
                    list.push(Type::Integer(integer_build.parse().unwrap()));
                    integer_build = String::new();
                }
            }
            a => {
                integer_build.push(a);
            }
        }
    }
}

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        input
            .split("\n\n")
            .map(|f| {
                f.lines()
                    .map(|x| parse_list(&mut x.chars().skip(1)))
                    .collect_tuple::<(_, _)>()
                    .unwrap()
            })
            .map(|(left, right)| compare(left, right))
            .enumerate()
            .filter(|f| f.1 == CompareResult::Right)
            .map(|f| f.0 + 1)
            .sum::<usize>()
            .to_string()
    }

    fn part2(&self, input: &str) -> String {
        input
            .split("\n\n")
            .flat_map(|f| f.lines().map(|x| parse_list(&mut x.chars().skip(1))))
            .chain(iter::once(Type::List(vec![Type::List(vec![
                Type::Integer(2),
            ])])))
            .chain(iter::once(Type::List(vec![Type::List(vec![
                Type::Integer(6),
            ])])))
            .sorted_by(|a, b| {
                if compare(a.clone(), b.clone()) == CompareResult::Right {
                    Ordering::Less
                } else {
                    Ordering::Greater
                }
            })
            .positions(|f| {
                f == (Type::List(vec![Type::List(vec![Type::Integer(2)])]))
                    || f == (Type::List(vec![Type::List(vec![Type::Integer(6)])]))
            })
            .map(|f| f + 1)
            .product::<usize>()
            .to_string()
    }
}
