use std::{collections::HashMap, error::Error};

use advent_of_code_lib::{self, Solver};
use colored::Colorize;
use itertools::Itertools;

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2023", "3")
}

const CHECK_FOR: [(isize, isize); 8] = [
    (1, 1),
    (1, 0),
    (1, -1),
    (0, 1),
    (0, -1),
    (-1, 1),
    (-1, 0),
    (-1, -1),
];

struct Day;

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        let height = input.lines().count();
        let width = input.lines().next().unwrap().len();
        let grid = input
            .lines()
            .map(|ch| ch.chars().collect_vec())
            .collect_vec();

        println!("Printing the grid!");

        let mut current_number = String::new();

        let mut sum: usize = 0;
        let mut add_to_sum = false;

        for y in 0..height {
            for x in 0..width {
                if grid[y][x].is_digit(10) {
                    current_number.push(grid[y][x]);
                } else {
                    print!("{}", grid[y][x])
                }
                if !current_number.is_empty() && (!grid[y][x].is_digit(10) || x == width - 1) {
                    if add_to_sum {
                        sum += current_number.parse::<usize>().unwrap();
                        add_to_sum = false;
                        print!("{}", current_number.green());
                    } else {
                        print!("{}", current_number.red());
                    }
                    current_number = String::new();
                }

                // Ensures we do not look for other symbols, if are not on a number
                if current_number.is_empty() {
                    continue;
                }

                // Checks for found characters
                for (x_off, y_off) in CHECK_FOR {
                    // Checks bounds
                    let check_x = x as isize + x_off;
                    if check_x < 0 || check_x >= width as isize {
                        continue;
                    }

                    let check_y = y as isize + y_off;
                    if check_y < 0 || check_y >= height as isize {
                        continue;
                    }

                    let found_char = grid[check_y as usize][check_x as usize];
                    if !found_char.is_digit(10) && found_char != '.' {
                        add_to_sum = true;
                    }
                }
            }
            current_number = String::new();
            println!()
        }

        println!();
        sum.to_string()
    }

    fn part2(&self, input: &str) -> String {
        let height = input.lines().count();
        let width = input.lines().next().unwrap().len();
        let grid = input
            .lines()
            .map(|ch| ch.chars().collect_vec())
            .collect_vec();

        println!("Printing the grid!");

        let mut current_number = String::new();

        let mut sum: usize = 0;
        let mut find_gear_ratio = false;
        let mut multiply_loc = (0, 0);

        let mut next_stars: HashMap<(usize, usize), usize> = HashMap::new();

        for y in 0..height {
            for x in 0..width {
                // Add to current number
                if grid[y][x].is_digit(10) {
                    current_number.push(grid[y][x]);
                } else {
                    print!("{}", grid[y][x])
                }

                // If the character is not numeric or we are at the end of the screen
                if !current_number.is_empty() && (!grid[y][x].is_digit(10) || x == width - 1) {
                    if find_gear_ratio {
                        let found_number = current_number.parse::<usize>().unwrap();

                        if next_stars.contains_key(&multiply_loc) {
                            sum += found_number * next_stars[&multiply_loc];
                            print!("{}", current_number.green());
                        } else {
                            next_stars.insert(multiply_loc, found_number);
                            print!("{}", current_number.yellow());
                        }
                        find_gear_ratio = false;
                    } else {
                        print!("{}", current_number.red());
                    }
                    current_number = String::new();
                }

                // Ensures we do not look for other symbols, if are not on a number
                if current_number.is_empty() {
                    continue;
                }

                for (x_off, y_off) in CHECK_FOR {
                    let check_x = x as isize + x_off;
                    if check_x < 0 || check_x >= width as isize {
                        continue;
                    }
                    let check_y = y as isize + y_off;
                    if check_y < 0 || check_y >= height as isize {
                        continue;
                    }

                    let found_char = grid[check_y as usize][check_x as usize];
                    if found_char == '*' {
                        find_gear_ratio = true;
                        multiply_loc = (check_x as usize, check_y as usize)
                    }
                }
            }
            current_number = String::new();
            println!()
        }

        println!();
        sum.to_string()
    }
}
