use reqwest::blocking::Client;
use reqwest::blocking::Response;
use reqwest::header::COOKIE;
use std::fs;

pub trait Solution {
    fn part1(&self, input: &str) -> String;
    fn part2(&self, input: &str) -> String;
}

pub fn setup<T>(day: u32, solution: T) 
    where T: Solution
{
    println!("Solution for day {}", day);

    let buffer = fs::read_to_string(get_path());
    if let Err(_err) = buffer {
        println!("Writing to file...");

        let response = get_input(&day);

        if response.status().is_success() {
            let mut input = response.text().unwrap();
            fs::write(get_path(), &input).unwrap();
            input.pop();
            run(solution, &input)
        } else {
            println!("Error while fetching: {}", response.status())
        }
    } else if let Ok(mut input) = buffer {
        input.pop();
        run(solution, &input)
    }
}

fn run<T>(solution: T, input: &String)
    where T: Solution
{
    println!("Part 1: {}", solution.part1(&input));
    println!("Part 2: {}", solution.part2(&input));
}

fn get_input(day: &u32) -> Response {
    let session = env!("AOC_SESSION");
    let client = Client::new();
    let response = client.get(format!("https://adventofcode.com/2021/day/{}/input", day))
        .header(COOKIE, format!("session={}", session))
        .send();

    response.unwrap()
}

fn get_path() -> String {
    String::from("./inputs/input.txt")
}

pub fn test<T>(part: String, day: &T) 
    where T: Solution
{
    let paths = fs::read_dir(format!("./inputs/{}", part)).unwrap();
    for path in paths {
        let path = path.unwrap();
        if path.file_name().to_string_lossy().starts_with("test") {
            let mut input = fs::read_to_string(path.path()).unwrap();
            let mut solution = fs::read_to_string(
                format!("./inputs/{}/solution_{}", part, path.file_name().to_string_lossy())
            ).unwrap();

            input.pop();
            solution.pop();

            if part == "part1" {
                assert_eq!(day.part1(&input), solution);
            } else {
                assert_eq!(day.part2(&input), solution);
            }
        }
    }
    println!("Completed all tests for {}", part)
}

