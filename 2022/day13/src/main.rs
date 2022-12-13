use std::{cmp::Ordering, error::Error};

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
    fn list_vecs(self) -> Vec<Type> {
        match self {
            Type::List(l) => l,
            l => vec![l],
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum CompareResult {
    Wrong,
    Right,
    Continue,
}

fn compare(left: Type, right: Type) -> CompareResult {
    if let Type::Integer(left) = left {
        if let Type::Integer(right) = right {
            return match left.cmp(&right) {
                std::cmp::Ordering::Less => CompareResult::Right,
                std::cmp::Ordering::Equal => CompareResult::Continue,
                std::cmp::Ordering::Greater => CompareResult::Wrong,
            };
        }
    }

    let mut left = left.list_vecs().into_iter();
    let mut right = right.list_vecs().into_iter();

    for _ in 0..left.len().max(right.len()) {
        if let Some(left) = left.next() {
            if let Some(right) = right.next() {
                match compare(left, right) {
                    CompareResult::Continue => continue,
                    result => return result,
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
    let mut integer = String::from("");

    fn add_integer(integer: String, list: &mut Vec<Type>) {
        if !integer.is_empty() {
            list.push(Type::Integer(integer.parse().unwrap()));
        }
    }

    loop {
        match chars.next().unwrap() {
            '[' => list.push(parse_list(chars)),
            ']' => {
                add_integer(integer, &mut list);
                return Type::List(list);
            }
            ',' => {
                add_integer(integer, &mut list);
                integer = String::new();
            }
            a => {
                integer.push(a);
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
        let dividers = [
            Type::List(vec![Type::List(vec![Type::Integer(2)])]),
            Type::List(vec![Type::List(vec![Type::Integer(6)])]),
        ];
        input
            .split("\n\n")
            .flat_map(|f| f.lines().map(|x| parse_list(&mut x.chars().skip(1))))
            .chain(dividers.clone())
            .sorted_by(|a, b| {
                if compare(a.clone(), b.clone()) == CompareResult::Right {
                    Ordering::Less
                } else {
                    Ordering::Greater
                }
            })
            .positions(|f| dividers.contains(&f))
            .map(|f| f + 1)
            .product::<usize>()
            .to_string()
    }
}
