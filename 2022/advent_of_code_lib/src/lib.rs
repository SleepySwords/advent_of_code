use reqwest::blocking::ClientBuilder;
use reqwest::blocking::Response;
use std::env;
use std::error::Error;
use std::fs;

pub fn run_and_print<T: Solver>(solver: T, year: &str, day: &str) -> Result<(), Box<dyn Error>> {
    let (part1, part2) = download_input_and_run(solver, year, day)?;
    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
    Ok(())
}

fn download_input_and_run<T: Solver>(
    solver: T,
    year: &str,
    day: &str,
) -> Result<(String, String), Box<dyn Error>> {
    let session = env::var("AOC_SESSION").expect("Need to set AOC_SESSION");

    println!("Solution for day {}", day);
    let buffer = fs::read_to_string(path());
    match buffer {
        Ok(input) => Ok((solver.part1(&input), solver.part2(&input))),
        Err(_) => {
            println!("Writing to file...");

            let response = input(session, year, day)?;

            if response.status().is_success() {
                let input = response.text()?;
                fs::write(path(), &input)?;
                println!("{}", solver.part1(&input));
                println!("{}", solver.part2(&input));
                Ok((solver.part1(&input), solver.part2(&input)))
            } else {
                println!("Error while fetching: {}", response.status());
                Ok((String::from("Error"), String::from("Error")))
            }
        }
    }
}

fn input(session: String, year: &str, day: &str) -> Result<Response, Box<dyn Error>> {
    let client = ClientBuilder::new().build()?;
    // Too lazy to do the token stuff here
    let response = client
        .get(format!(
            "https://adventofcode.com/{}/day/{}/input",
            year, day
        ))
        .header("cookie", format!("session={}", session))
        .send();

    Ok(response?)
}

fn path() -> String {
    format!("inputs/main.input")
}

pub fn test<T: Solver>(
    solver: T,
    input: &str,
    solution: &str,
    part: Part,
) -> Result<(), Box<dyn Error>> {
    match part {
        Part::Part1 => assert_eq!(solver.part1(input), solution),
        Part::Part2 => assert_eq!(solver.part2(input), solution),
    }
    Ok(())
}

pub fn test_file<T: Solver>(solver: T, file: &str, part: Part) -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string(format!("inputs/{}.input", file))?;
    let answer = fs::read_to_string(format!(
        "inputs/{}.{}",
        file,
        match part {
            Part::Part1 => "part1",
            Part::Part2 => "part2",
        }
    ))?;
    test(solver, input.as_str(), answer.trim(), part)
}

pub trait Solver {
    fn part1(&self, _: &str) -> String {
        String::from("Not implemented yet")
    }

    fn part2(&self, _: &str) -> String {
        String::from("Not implemented yet")
    }
}

#[derive(PartialEq, Eq)]
pub enum Part {
    Part1,
    Part2,
}
