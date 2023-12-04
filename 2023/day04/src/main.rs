use std::error::Error;

use advent_of_code_lib::{self, Solver};

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2023", "4")
}

struct Day;

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        input.lines().map(|f| {
            let mut num = f.split(": ");
            let mut card = num.next().unwrap().replace("Card", "").trim().parse::<u32>();
            let mut cards = num.next().unwrap().split(" | ");
            // let winning_cards = cards.next().unwrap().split_inclusive
        });

        "aet".to_string()
    }

    // fn part2(&self, input: &str) -> String {
    // }
}
