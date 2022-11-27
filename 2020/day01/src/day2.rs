use crate::Solution;
pub struct Day2;

impl Solution for Day2 {
    fn part1(input: &String) -> String {
        let mut depth = 0;
        let mut x = 0;
        for command in input.split('\n') {
            if command.len() != 2 { continue };
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

    fn part2(input: &String) -> String {
        let mut depth: i32 = 0;
        let mut x: i32 = 0;
        let mut aim: i32 = 0;
        for command in input.split('\n') {
            if command.len() != 2 { continue };
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
