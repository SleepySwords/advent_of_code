use std::error::Error;

use advent_of_code_lib::{self, Solver};

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day1, "2022", "2")?;
    Ok(())
}

struct Day1;

impl Day1 {
    fn winner(elf_choice: Choice, player_choice: Choice) -> Winner {
        if elf_choice == player_choice {
            return Winner::Draw;
        }
        if elf_choice == Choice::Rock && player_choice == Choice::Scissors {
            return Winner::Loss;
        }
        if elf_choice == Choice::Paper && player_choice == Choice::Rock {
            return Winner::Loss;
        }
        if elf_choice == Choice::Scissors && player_choice == Choice::Paper {
            return Winner::Loss;
        }
        Winner::Winner
    }

    fn choice(elf_choice: Choice, winner: Winner) -> Choice {
        if winner == Winner::Draw {
            return elf_choice;
        }
        if winner == Winner::Winner {
            match elf_choice {
                Choice::Rock => Choice::Paper,
                Choice::Paper => Choice::Scissors,
                Choice::Scissors => Choice::Rock,
            }
        } else {
            match elf_choice {
                Choice::Rock => Choice::Scissors,
                Choice::Paper => Choice::Rock,
                Choice::Scissors => Choice::Paper,
            }
        }
    }
}

impl Solver for Day1 {
    fn part1(&self, input: &str) -> String {
        input
            .lines()
            .map(|f| {
                let mut split = f.split(" ");
                let comb = split.next().unwrap();
                let elf_choice = if comb == "A" {
                    Choice::Rock
                } else if comb == "B" {
                    Choice::Paper
                } else {
                    Choice::Scissors
                };
                let comb = split.next().unwrap();
                let player_choice = if comb == "X" {
                    Choice::Rock
                } else if comb == "Y" {
                    Choice::Paper
                } else {
                    Choice::Scissors
                };
                (elf_choice, player_choice)
            })
            .fold(0, |mut acc, (elf_choice, player_choice)| {
                acc += if player_choice == Choice::Rock {
                    1
                } else if player_choice == Choice::Paper {
                    2
                } else {
                    3
                };
                acc += match Self::winner(elf_choice, player_choice) {
                    Winner::Winner => 6,
                    Winner::Draw => 3,
                    Winner::Loss => 0,
                };
                acc
            })
            .to_string()
    }

    fn part2(&self, input: &str) -> String {
        input
            .lines()
            .map(|f| {
                let mut split = f.split(" ");
                let comb = split.next().unwrap();
                let elf_choice = if comb == "A" {
                    Choice::Rock
                } else if comb == "B" {
                    Choice::Paper
                } else {
                    Choice::Scissors
                };
                let comb = split.next().unwrap();
                let player_choice = if comb == "X" {
                    Winner::Loss
                } else if comb == "Y" {
                    Winner::Draw
                } else {
                    Winner::Winner
                };
                (elf_choice, player_choice)
            })
            .fold(0, |mut acc, (elf_choice, winner)| {
                acc += if winner == Winner::Loss {
                    0
                } else if winner == Winner::Draw {
                    3
                } else {
                    6
                };
                acc += match Self::choice(elf_choice, winner) {
                    Choice::Rock => 1,
                    Choice::Paper => 2,
                    Choice::Scissors => 3,
                };
                acc
            })
            .to_string()
    }
}

#[derive(PartialEq, Eq)]
enum Choice {
    Rock,
    Paper,
    Scissors,
}

#[derive(PartialEq, Eq)]
enum Winner {
    Winner,
    Draw,
    Loss,
}

// impl Choice {
//     fn win_to(self) -> Choice {

//     }
// }
