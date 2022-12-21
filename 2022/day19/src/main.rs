use std::error::Error;

use advent_of_code_lib::{self, Solver};

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2022", "-")
}

struct Day;

impl Solver for Day {
    // Reducing the decision tree
    // - Each minute (either buy or not buy)
    // The not buy can be added to a skipped list, as to reduce the decision (collectors should be
    // bought as early as possible)
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

// #[test]
// fn test_file1() -> Result<(), Box<dyn Error>> {
//     advent_of_code_lib::test_file(Day, "test1", advent_of_code_lib::Part::Part1)
// }
