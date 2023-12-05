use std::error::Error;

use advent_of_code_lib::{self, Solver};

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2023", "5")
}

struct Day;

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        input
            .lines()
            .map(|f| f.parse().unwrap())
            .fold((0, i32::MAX), |(mut acc, prev), inp| {
                if inp > prev {
                    acc += 1
                }
                (acc, inp)
            })
            .0
            .to_string()
    }

    // fn part2(&self, input: &str) -> String {
    // }
}
