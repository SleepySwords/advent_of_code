use std::error::Error;

use advent_of_code_lib::{self, Solver};
use itertools::Itertools;

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2022", "20")
}

struct Day;

fn cycle(pos: usize, offset: i64, max: usize) -> usize {
    let new = (pos as i64 + offset) % (max - 1) as i64;
    if new < 0 {
        (max as i64 - 1 + new) as usize
    } else {
        new as usize
    }
}

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        let count = input.lines().count();
        let mut numbers = input
            .lines()
            .map(|f| f.parse::<i64>().unwrap())
            .enumerate()
            .collect_vec();
        for x in 0..count {
            let pos = numbers.iter().position(|f| f.0 == x).unwrap();
            let mix_number = numbers[pos].1;
            let new_pos = cycle(pos, mix_number, count);
            numbers.remove(pos);
            numbers.insert(new_pos, (x, mix_number));
        }
        let x = numbers.iter().position(|f| f.1 == 0).unwrap();
        let calc = numbers[(x + 1000) % count].1
            + numbers[(x + 2000) % count].1
            + numbers[(x + 3000) % count].1;
        // println!("{:?}", numbers);
        calc.to_string()
    }

    fn part2(&self, input: &str) -> String {
        let count = input.lines().count();
        let mut numbers = input
            .lines()
            .map(|f| f.parse::<i64>().unwrap() * 811589153)
            .enumerate()
            .collect_vec();
        for _ in 0..10 {
            for x in 0..count {
                let pos = numbers.iter().position(|f| f.0 == x).unwrap();
                let mix_number = numbers[pos].1;
                let new_pos = cycle(pos, mix_number, count);
                numbers.remove(pos);
                numbers.insert(new_pos, (x, mix_number));
            }
        }
        let x = numbers.iter().position(|f| f.1 == 0).unwrap();
        let calc = numbers[(x + 1000) % count].1
            + numbers[(x + 2000) % count].1
            + numbers[(x + 3000) % count].1;
        // println!("{:?}", numbers);
        calc.to_string()
    }
}
