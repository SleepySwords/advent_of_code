use std::error::Error;

use advent_of_code_lib::{self, Solver};
use itertools::Itertools;

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day1, "2022", "1")?;
    Ok(())
}

struct Day1;

impl Solver for Day1 {
    fn part1(&self, input: &str) -> String {
        input
            .split_terminator("\n\n")
            .map(|f| f.lines().map(|x| x.parse::<i32>().unwrap()).sum::<i32>())
            .max()
            .unwrap()
            .to_string()
    }

    fn part2(&self, input: &str) -> String {
        input
            .split_terminator("\n\n")
            .map(|f| f.lines().map(|x| x.parse::<i32>().unwrap()).sum::<i32>())
            .sorted()
            .rev()
            .take(3)
            .sum::<i32>()
            .to_string()
    }
}

#[cfg(test)]
mod tests {
    use std::error::Error;

    use crate::Day1;

    #[test]
    fn test_file_part1() -> Result<(), Box<dyn Error>> {
        advent_of_code_lib::test_file(Day1, "1", "test1", advent_of_code_lib::Part::Part1)?;
        Ok(())
    }

    #[test]
    fn test_file_part2() -> Result<(), Box<dyn Error>> {
        advent_of_code_lib::test_file(Day1, "1", "test1", advent_of_code_lib::Part::Part2)?;
        Ok(())
    }
}
