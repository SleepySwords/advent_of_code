use std::error::Error;

use advent_of_code_lib::{self, Solver};
use dotenv::dotenv;

fn main() -> Result<(), Box<dyn Error>> {
    dotenv().ok();
    advent_of_code_lib::run_and_print(Day, "2023", "1")
}

struct Day;

const NUMBERS: &[(&str, u32)] = &[
    ("0", 0),
    ("zero", 0),
    ("1", 1),
    ("one", 1),
    ("2", 2),
    ("two", 2),
    ("3", 3),
    ("three", 3),
    ("4", 4),
    ("four", 4),
    ("5", 5),
    ("five", 5),
    ("6", 6),
    ("six", 6),
    ("7", 7),
    ("seven", 7),
    ("8", 8),
    ("eight", 8),
    ("9", 9),
    ("nine", 9),
];

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        input
            .lines()
            .map(|x| {
                let min = x
                    .chars()
                    .filter(|char| char.is_digit(10))
                    .next()
                    .unwrap()
                    .to_digit(10)
                    .unwrap();

                let max = x
                    .chars()
                    .filter(|char| char.is_digit(10))
                    .last()
                    .unwrap()
                    .to_digit(10)
                    .unwrap();
                min * 10 + max
            })
            .sum::<u32>()
            .to_string()
    }

    fn part2(&self, input: &str) -> String {
        input
            .lines()
            .map(|x| {
                let min = NUMBERS
                    .iter()
                    .filter_map(|(st, value)| Some((x.find(st)?, value)))
                    .min_by_key(|x| x.0)
                    .unwrap();
                let max = NUMBERS
                    .iter()
                    .filter_map(|(st, value)| Some((x.rfind(st)?, value)))
                    .max_by_key(|x| x.0)
                    .unwrap();
                min.1 * 10 + max.1
            })
            .sum::<u32>()
            .to_string()
    }
}
