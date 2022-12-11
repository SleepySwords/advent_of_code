use std::{
    collections::VecDeque,
    error::Error,
    fmt::Debug,
};

use advent_of_code_lib::{self, Solver};
use itertools::Itertools;
use lazy_static::lazy_static;
use num::integer::lcm;
use regex::Regex;

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2022", "11")
}

lazy_static! {
    static ref OPERATION_REGEX: Regex = Regex::new(r"(.+) (.) (.+)").unwrap();
}

struct Day;

struct Monkey<'a> {
    inspect_counter: usize,
    items: VecDeque<u64>,
    operation: Box<dyn Fn(u64) -> u64 + 'a>,
    divisible: u64,
    monkey_if_true: u64,
    monkey_if_false: u64,
}

impl Debug for Monkey<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "Monkey ({}, {:?}, {}, {}, {})",
            self.inspect_counter,
            self.items,
            self.divisible,
            self.monkey_if_true,
            self.monkey_if_false
        ))
    }
}

fn lowest_common_multiple(monkeys: &Vec<Monkey>) -> u64 {
    monkeys
        .iter()
        .map(|f| f.divisible)
        .unique()
        .reduce(|a, b| lcm(a, b))
        .unwrap()
}

fn inspect_items(monkey_number: usize, monkeys: &mut Vec<Monkey>, lcm: u64, divide_by_3: bool) {
    while let Some(mut item) = monkeys[monkey_number].items.pop_front() {
        monkeys[monkey_number].inspect_counter += 1;

        item = (monkeys[monkey_number].operation)(item);
        if divide_by_3 {
            item = item / 3;
        } else {
            // Cannot do modulo when dividing by 3, as dividing by 3 screws the modulo cycles up.
            item = item % lcm;
        }

        let throw_to = if item % monkeys[monkey_number].divisible == 0 {
            monkeys[monkey_number].monkey_if_true
        } else {
            monkeys[monkey_number].monkey_if_false
        };
        monkeys[throw_to as usize].items.push_back(item);
    }
    monkeys[monkey_number].items = VecDeque::new();
}

fn round(monkeys: &mut Vec<Monkey>, lcm: u64, divide_by_3: bool) {
    for x in 0..monkeys.len() {
        inspect_items(x, monkeys, lcm, divide_by_3);
    }
}

fn parse<'a>(input: &'a str) -> Vec<Monkey<'a>> {
    input
        .split("\n\n")
        .map(|f| {
            let mut iter = f.lines().skip(1);

            let items = iter.next().unwrap()["  Starting items: ".len()..]
                .split(", ")
                .map(|a| a.parse::<_>().unwrap())
                .collect::<VecDeque<_>>();

            let operation = &iter.next().unwrap()["  Operation: new = ".len()..];
            let captures = OPERATION_REGEX.captures(operation).unwrap();
            let first_variable = captures.get(1).unwrap().as_str();
            let operation = captures.get(2).unwrap().as_str();
            let second_variable = captures.get(3).unwrap().as_str();
            let operation = Box::new(move |x: u64| {
                let first = if first_variable == "old" {
                    x
                } else {
                    first_variable.parse().unwrap()
                };
                let second = if second_variable == "old" {
                    x
                } else {
                    second_variable.parse().unwrap()
                };
                if operation == "*" {
                    first * second
                } else {
                    first + second
                }
            });

            let divisible = iter.next().unwrap()["  Test: divisible by ".len()..]
                .parse::<_>()
                .unwrap();
            let monkey_if_true = iter.next().unwrap()["    If true: throw to monkey ".len()..]
                .parse::<_>()
                .unwrap();
            let monkey_if_false = iter.next().unwrap()["    If false: throw to monkey ".len()..]
                .parse::<_>()
                .unwrap();

            Monkey {
                inspect_counter: 0,
                items,
                operation,
                divisible,
                monkey_if_true,
                monkey_if_false,
            }
        })
        .collect::<Vec<Monkey>>()
}

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        let mut monkeys = parse(input);
        let lcm = lowest_common_multiple(&monkeys);
        for _ in 0..20 {
            round(&mut monkeys, lcm, true);
        }
        monkeys
            .into_iter()
            .map(|f| f.inspect_counter)
            .sorted()
            .rev()
            .take(2)
            .reduce(|a, b| a * b)
            .unwrap()
            .to_string()
    }

    fn part2(&self, input: &str) -> String {
        let mut monkeys = parse(input);
        let lcm = lowest_common_multiple(&monkeys);
        for _ in 0..10000 {
            round(&mut monkeys, lcm, false);
        }
        monkeys
            .into_iter()
            .map(|f| f.inspect_counter)
            .sorted()
            .rev()
            .take(2)
            .reduce(|a, b| a * b)
            .unwrap()
            .to_string()
    }
}
