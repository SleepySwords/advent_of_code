use std::error::Error;

use advent_of_code_lib::{self, Solver};

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2022", "6")
}

struct Day;

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        (input
            .lines()
            .next()
            .unwrap()
            .chars()
            .collect::<Vec<char>>()
            .windows(4)
            .enumerate()
            .find(|(_, chars)| {
                for i in 1..chars.len() {
                    if chars[i..].contains(&chars[i - 1]) {
                        return false;
                    }
                }
                true
            })
            .unwrap()
            .0
            + 4)
        .to_string()
    }

    fn part2(&self, input: &str) -> String {
        (input
            .lines()
            .next()
            .unwrap()
            .chars()
            .collect::<Vec<char>>()
            .windows(14)
            .enumerate()
            .find(|(_, chars)| {
                for i in 1..chars.len() {
                    if chars[i..].contains(&chars[i - 1]) {
                        return false;
                    }
                }
                true
            })
            .unwrap()
            .0 + 14)
            .to_string()
    }
}

// #[test]
// fn test_file1() -> Result<(), Box<dyn Error>> {
//     advent_of_code_lib::test_file(Day, "test1", advent_of_code_lib::Part::Part1)
// }
