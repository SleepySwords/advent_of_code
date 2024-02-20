use std::error::Error;

use advent_of_code_lib::{self, Solver};
use itertools::Itertools;

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2023", "12")
}

struct Day;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Tile {
    Damaged,     // #
    Operational, // ?
    Unknown,     // ?
}

fn parse_input(input: &str) -> (Vec<Tile>, Vec<usize>) {
    let (field, condition) = input.split(" ").next_tuple().expect("Invalid input");
    let field = field
        .chars()
        .map(|ch| match ch {
            '#' => Tile::Damaged,
            '.' => Tile::Operational,
            '?' => Tile::Unknown,
            _ => panic!("Invalid input"),
        })
        .collect_vec();
    let condition = condition
        .split(",")
        .map(|sp| sp.trim().parse::<usize>().expect("Invalid input"))
        .collect_vec();
    return (field, condition);
}

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        return String::from("Do haskell");
    }

    fn part2(&self, input: &str) -> String {
        input
            .split("\n")
            .filter(|f| !f.is_empty())
            .map(|line| {
                let (mut field, condition) = parse_input(line);
                println!("{:?} {:?}", field, condition);
                let field_len = field.len();
                field.push(Tile::Unknown);
                let field = field
                    .into_iter()
                    .cycle()
                    .take((field_len + 1) * 5 - 1)
                    .collect_vec();
                let condition_len = condition.len();
                let condition = condition
                    .into_iter()
                    .cycle()
                    .take(condition_len * 5)
                    .collect_vec();
                let state = State {
                    field_index: 0,
                    condition_index: 0,
                    damaged: 0,
                    field: &field,
                    condition: &condition,
                };
                state.next_step()
            })
            .sum::<usize>()
            .to_string()
    }
}

#[derive(Clone, Copy)]
struct State<'a> {
    field_index: usize,
    condition_index: usize,
    damaged: usize,
    field: &'a Vec<Tile>,
    condition: &'a Vec<usize>,
}

impl State<'_> {
    pub fn current_tile(&self) -> Tile {
        return self.field[self.field_index];
    }

    pub fn need_damaged(&self) -> usize {
        return self.condition[self.condition_index];
    }

    // pub fn damaged(mut self, mut current: Vec<Tile>) -> usize {
    pub fn damaged(mut self) -> usize {
        // current.push(Tile::Damaged);
        if self.need_damaged() == self.damaged + 1 {
            // Ensure that the next is not damaged as well.
            if self.field_index + 1 == self.field.len()
                || self.field[self.field_index + 1] != Tile::Damaged
            {
                // current.push(Tile::Operational);
                self.field_index += 2;
                self.condition_index += 1;
                self.damaged = 0;
                // return self.next_step(current);
                return self.next_step();
            } else {
            // println!("Bad: {}", self.field_index);
                return 0;
            }
        } else {
            self.damaged += 1;
            self.field_index += 1;
            // return self.next_step(current);
            return self.next_step();
        }
    }

    // pub fn operating(mut self, mut current: Vec<Tile>) -> usize {
    pub fn operating(mut self) -> usize {
        // current.push(Tile::Operational);
        if self.damaged > 0 {
            // println!("Bad: {}", self.field_index);
            return 0;
        }
        self.field_index += 1;
        // return self.next_step(current);
        return self.next_step();
    }

    // pub fn next_step(self, current: Vec<Tile>) -> usize {
    pub fn next_step(self) -> usize {
        println!("{}", self.field_index);
        // println!(
        //     "Current: {} {} {:?} {} {}",
        //     self.field_index,
        //     self.field.len(),
        //     current
        //         .iter()
        //         .map(|f| match f {
        //             Tile::Damaged => '#',
        //             Tile::Operational => '.',
        //             Tile::Unknown => '?',
        //         })
        //         .collect::<String>(),
        //     self.condition_index,
        //     self.condition.len(),
        // );
        if self.field_index >= self.field.len() && self.condition_index >= self.condition.len() {
            // println!("good!");
            return 1;
        }
        if self.field_index >= self.field.len() && self.condition_index < self.condition.len() {
            // println!("Bad: {}", self.field_index);
            return 0;
        }
        if self.field_index < self.field.len() && self.condition_index >= self.condition.len() {
            return if (self.field_index..self.field.len())
                .map(|i| self.field[i])
                .all(|s| s == Tile::Operational || s == Tile::Unknown)
            {
                1
            } else {
                0
            };
        }
        if self.damaged >= self.need_damaged() {
            // println!("Bad: {}", self.field_index);
            return 0;
        }
        match self.current_tile() {
            Tile::Damaged => {
                // println!("Damaged: {}", self.field_index);
                // return self.damaged(current);
                return self.damaged();
            }
            Tile::Operational => {
                // println!("Operating: {}", self.field_index);
                // return self.operating(current);
                return self.operating();
            }
            Tile::Unknown => {
                // println!("Unknown damaged: {}", self.field_index);
                let damaged = self.damaged();
                // let damaged = self.damaged(current.clone());
                // println!(
                //     "Unknown operating: {}, found damaged {}",
                //     self.field_index, damaged
                // );
                // return damaged + self.operating(current.clone());
                return damaged + self.operating();
            }
        }
    }
}
