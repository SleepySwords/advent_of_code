use std::{collections::VecDeque, error::Error, str::Lines};

use advent_of_code_lib::{self, Solver};
use lazy_static::lazy_static;
use regex::Regex;

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day5, "2022", "5")?;
    Ok(())
}

lazy_static! {
    static ref INSTRUCTION_REGEX: Regex = Regex::new(r"move (\d+) from (\d+) to (\d+)").unwrap();
}

struct Day5;

impl Day5 {
    fn parse_crates(mut crate_part: Lines) -> Vec<VecDeque<&str>> {
        let crate_regex = Regex::new(r"(?:.(.). ?)").unwrap();
        let number_crates = crate_part
            .next_back()
            .unwrap()
            .trim()
            .chars()
            .last()
            .unwrap()
            .to_digit(10)
            .unwrap();

        crate_part.fold(
            vec![VecDeque::new(); number_crates as usize],
            |mut acc: Vec<VecDeque<&str>>, l| {
                for (i, x) in crate_regex.captures_iter(l).enumerate() {
                    let crate_box = x.get(1).unwrap().as_str();
                    if !crate_box.chars().all(|f| f.is_whitespace()) {
                        acc[i].push_front(crate_box);
                    }
                }
                acc
            },
        )
    }

    fn parse_instructions(instruction: &str) -> (usize, usize, usize) {
        let capture_group = INSTRUCTION_REGEX.captures(instruction).unwrap();
        let len = capture_group
            .get(1)
            .unwrap()
            .as_str()
            .parse::<usize>()
            .unwrap();
        let from = capture_group
            .get(2)
            .unwrap()
            .as_str()
            .parse::<usize>()
            .unwrap();
        let to = capture_group
            .get(3)
            .unwrap()
            .as_str()
            .parse::<usize>()
            .unwrap();
        (len, from, to)
    }
}

impl Solver for Day5 {
    fn part1(&self, input: &str) -> String {
        let inp = input.split_once("\n\n").unwrap();
        let mut crates = Self::parse_crates(inp.0.lines());

        for instruction in inp.1.lines() {
            let (len, from, to) = Self::parse_instructions(instruction);
            for _ in 0..len {
                let crate_move = crates[from - 1].pop_back().unwrap();
                crates[to - 1].push_back(crate_move);
            }
        }

        crates
            .into_iter()
            .map(|mut v| v.pop_back().unwrap())
            .collect::<String>()
    }

    fn part2(&self, input: &str) -> String {
        let inp = input.split_once("\n\n").unwrap();
        let mut crates = Self::parse_crates(inp.0.lines());

        for instruction in inp.1.lines() {
            let (len, from, to) = Self::parse_instructions(instruction);
            (0..len)
                .map(|_| crates[from - 1].pop_back().unwrap())
                .collect::<Vec<&str>>()
                .into_iter()
                .rev()
                .for_each(|crate_box| crates[to - 1].push_back(crate_box));
        }

        crates
            .into_iter()
            .map(|mut v| v.pop_back().unwrap())
            .collect::<String>()
    }
}

#[test]
fn test_file1() -> Result<(), Box<dyn Error>> {
    use advent_of_code_lib::Part;
    advent_of_code_lib::test_file(Day5, "5", "test1", Part::Part1)?;
    advent_of_code_lib::test_file(Day5, "5", "test1", Part::Part2)
}
