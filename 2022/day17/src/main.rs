use std::error::Error;

use advent_of_code_lib::{self, Solver};

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2022", "-")
}

struct Day;

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        0.to_string()
    }

    // fn part2(&self, input: &str) -> String {
    // }
}
