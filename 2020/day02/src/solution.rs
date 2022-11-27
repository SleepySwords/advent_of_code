
use utils;

pub struct Day;

impl utils::Solution for Day {
    fn part1(&self, input: &str) -> String {
        let mut depth = 0;
        let mut x = 0;
        for command in input.split('\n') {
            let command: Vec<_> = command.split(" ").collect();
            let value = command[1].parse::<u32>().unwrap();
            match command[0] {
                "forward" => {
                    x += value;
                }
                "down" => {
                    depth += value;
                }
                "up" => {
                    depth -= value;
                }
                _ => {}
            }
        }
        (depth * x).to_string()
    }

    fn part2(&self, input: &str) -> String {
        let mut depth: i32 = 0;
        let mut x: i32 = 0;
        let mut aim: i32 = 0;
        for command in input.split('\n') {
            let command: Vec<_> = command.split(" ").collect();
            let value = command[1].parse::<i32>().unwrap();
            match command[0] {
                "forward" => {
                    x += value;
                    depth += aim * value;
                }
                "down" => {
                    aim += value;
                }
                "up" => {
                    aim -= value;
                }
                _ => {}
            }
        }
        (depth * x).to_string()
    }
}

