mod day1;
mod day2;
mod default_sol;

use reqwest::blocking::Client;
use reqwest::blocking::Response;
use reqwest::header::COOKIE;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let arg = args[1].clone();
    
    println!("Solution for day {}", arg);
    let buffer = fs::read_to_string(get_path(&arg));
    if let Err(_err) = buffer {
        println!("Writing to file...");

        let response = get_input(&arg);

        if response.status().is_success() {
            let input = response.text().unwrap();
            fs::write(get_path(&arg), &input).unwrap();
            run_solutions(&arg, &input)
        } else {
            println!("Error while fetching: {}", response.status())
        }
    } else if let Ok(input) = buffer {
        run_solutions(&arg, &input)
    }
}

fn get_input(day: &String) -> Response {
    let client = Client::new();
    // Too lazy to do the token stuff here
    let response = client.get(format!("https://adventofcode.com/2021/day/{}/input", day))
        .send();

    response.unwrap()
}

fn get_path(day: &String) -> String {
    format!("inputs/{}.input", day)
}

fn run_solutions(day: &String, input: &String) {
    match day.as_str() {
        "1" => {
            println!("Part 1: {}", day1::Day1::part1(input));
            println!("Part 2: {}", day1::Day1::part2(input));
        },
        "2" => {
            println!("Part 1: {}", day2::Day2::part1(input));
            println!("Part 2: {}", day2::Day2::part2(input));
        },
        _ => {
            println!("Part 2: {}", default_sol::DefaultSolution::part2(input));
            println!("Part 2: {}", default_sol::DefaultSolution::part2(input));
        },
    };
}

trait Solution {
    fn part1(input: &String) -> String;
    fn part2(input: &String) -> String;
}

struct DefaultSolution;

impl Solution for DefaultSolution {
    fn part1(_input: &String) -> String {
        String::from("Not implemented yet")
    }

    fn part2(_input: &String) -> String {
        String::from("Not implemented yet")
    }
}
