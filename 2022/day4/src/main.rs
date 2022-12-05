use std::{error::Error, ops::RangeInclusive};

use advent_of_code_lib::{self, Solver};

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day4, "2022", "4")?;
    Ok(())
}

struct Day4;
impl Day4 {
    fn range_in_another(a: &RangeInclusive<u32>, b: &RangeInclusive<u32>) -> bool {
        (a.start() >= b.start()) && (a.end() <= b.end())
    }
    fn range_a_overlaps_b(a: &RangeInclusive<u32>, b: &RangeInclusive<u32>) -> bool {
        // ((a.start() <= b.start()) && (a.end() >= b.start()))
        //     || ((a.start() <= b.end()) && (a.end() >= b.end()))
        (a.start() <= b.end()) && (a.end() >= b.start())
    }
}
impl Solver for Day4 {
    fn part1(&self, input: &str) -> String {
        input
            .lines()
            .map(|f| {
                let mut split = f.split(",");
                let mut elf1 = split
                    .next()
                    .unwrap()
                    .split("-")
                    .map(|f| f.parse::<u32>().unwrap());
                let mut elf2 = split.next().unwrap().split("-").map(|f| f.parse().unwrap());
                (
                    elf1.next().unwrap()..=elf1.next().unwrap(),
                    elf2.next().unwrap()..=elf2.next().unwrap(),
                )
            })
            .fold(0, |mut acc, (elf1, elf2)| {
                if Self::range_in_another(&elf1, &elf2) || Self::range_in_another(&elf2, &elf1) {
                    acc += 1;
                }
                acc
            })
            .to_string()
    }

    fn part2(&self, input: &str) -> String {
        input
            .lines()
            .map(|f| {
                let mut split = f.split(",");
                let mut elf1 = split
                    .next()
                    .unwrap()
                    .split("-")
                    .map(|f| f.parse::<u32>().unwrap());
                let mut elf2 = split.next().unwrap().split("-").map(|f| f.parse().unwrap());
                (
                    elf1.next().unwrap()..=elf1.next().unwrap(),
                    elf2.next().unwrap()..=elf2.next().unwrap(),
                )
            })
            .fold(0, |mut acc, (elf1, elf2)| {
                if Self::range_a_overlaps_b(&elf1, &elf2) || Self::range_a_overlaps_b(&elf2, &elf1)
                {
                    acc += 1;
                }
                acc
            })
            .to_string()
    }
}
