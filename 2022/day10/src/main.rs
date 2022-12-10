use std::error::Error;

use advent_of_code_lib::{self, Solver};

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2022", "10")
}

struct Day;

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        input
            .lines()
            .flat_map(|f| {
                if f == "noop" {
                    vec![0]
                } else {
                    vec![0, f[5..].parse::<i32>().unwrap()]
                }
            })
            .enumerate()
            .fold(
                (0, 1),
                |(mut acc, mut x_value), (mut cycle, x_instruction)| {
                    cycle += 1;
                    if cycle == 20
                        || cycle == 60
                        || cycle == 100
                        || cycle == 140
                        || cycle == 180
                        || cycle == 220
                    {
                        acc += cycle as i32 * x_value;
                    }
                    x_value += x_instruction;
                    (acc, x_value)
                },
            )
            .0
            .to_string()
    }

    fn part2(&self, input: &str) -> String {
        let output = input
            .lines()
            .flat_map(|f| {
                if f == "noop" {
                    vec![0]
                } else {
                    vec![0, f[5..].parse::<i32>().unwrap()]
                }
            })
            .enumerate()
            .fold(
                (vec![], 1i32),
                |(mut acc, mut x_value), (cycle, x_instruction)| {
                    if ((x_value - 1)..=(x_value + 1)).contains(&((cycle as i32) % 40)) {
                        acc.push(true);
                    } else {
                        acc.push(false);
                    }
                    // if cycle < 41 {
                    //     println!("Executing cycle: {cycle}");
                    //     println!("Sprite at position: {x_value}");
                    //     println!("{}", acc.iter().map(|f| if *f { "#" } else { "." }).collect::<String>());
                    //     println!()
                    // }
                    x_value += x_instruction;
                    (acc, x_value)
                },
            )
            .0;
        for y in 0..6 {
            for x in 0..40 {
                print!("{}", if output[y * 40 + x] { "#" } else { "." });
            }
            println!()
        }
        "The letters above ^^".to_string()
    }
}

// #[test]
// fn test_file1() -> Result<(), Box<dyn Error>> {
//     advent_of_code_lib::test_file(Day, "test1", advent_of_code_lib::Part::Part1)
// }
