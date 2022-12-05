use std::{collections::HashMap, error::Error};

use advent_of_code_lib::{self, Solver};
use itertools::Itertools;

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day3, "2022", "3")?;
    Ok(())
}

struct Day3;

impl Solver for Day3 {
    fn part1(&self, input: &str) -> String {
        input
            .lines()
            .fold(0, |mut acc, f| {
                let (compartment_1, compartment_2) = f.split_at(f.len() / 2);
                let c = compartment_1
                    .chars()
                    .reduce(|acc, c| if compartment_2.contains(c) { c } else { acc })
                    .unwrap();
                if c.is_lowercase() {
                    acc += u32::from(c) - 96;
                } else {
                    acc += u32::from(c) - 65 + 27;
                }
                acc
            })
            .to_string()
    }

    fn part2(&self, input: &str) -> String {
        input
            .lines()
            .chunks(3)
            .into_iter()
            .fold(0, |mut acc, group| {
                let map = group.fold(HashMap::new(), |mut acc, elf| {
                    println!("{}", elf);
                    for ele in 'a'..='z' {
                        if elf.contains(ele) {
                            acc.insert(ele, acc.get(&ele).unwrap_or(&0) + 1);
                        }
                    }
                    for ele in 'A'..='Z' {
                        if elf.contains(ele) {
                            acc.insert(ele, acc.get(&ele).unwrap_or(&0) + 1);
                        }
                    }
                    acc
                });
                let mut a = 0;
                for (c, v) in map.clone() {
                    if v == 3 {
                        if c.is_lowercase() {
                            a = u32::from(c) - 96;
                        } else {
                            a = u32::from(c) - 65 + 27;
                        }
                    }
                }
                acc+=a;
                acc
            })
            .to_string()
    }
}
