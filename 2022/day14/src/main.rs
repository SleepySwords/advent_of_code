use std::{collections::HashSet, error::Error};

use advent_of_code_lib::{self, Solver};
use itertools::Itertools;

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2022", "14")
}

struct Day;

fn simulate_sand<T: Fn((isize, isize)) -> bool>(
    rocks: &HashSet<(isize, isize)>,
    sand: &mut HashSet<(isize, isize)>,
    sand_particle: (isize, isize),
    condition: T,
    floor: Option<isize>,
) -> bool {
    // Condition must be executed here if there is no floor as without it, it would lead to a stack
    // overflow.
    if condition(sand_particle) && floor.is_none() {
        return false;
    }

    let is_not_floor = floor.map(|x| sand_particle.1 + 1 != x).unwrap_or(true);
    for (x, y) in [(0, 1), (-1, 1), (1, 1)] {
        if !rocks.contains(&(sand_particle.0 + x, sand_particle.1 + y))
            && !sand.contains(&(sand_particle.0 + x, sand_particle.1 + y))
            && is_not_floor
        {
            return simulate_sand(
                rocks,
                sand,
                (sand_particle.0 + x, sand_particle.1 + y),
                condition,
                floor,
            );
        }
    }
    sand.insert(sand_particle);

    // Condition as it check against the source, which would always be true as it initialises as
    // the source.
    if condition(sand_particle) && floor.is_some() {
        return false;
    }

    true
}

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        let rock = parse(input);
        let mut sand = HashSet::new();
        let source = (500, 0);
        let lowest_point = rock.iter().map(|f| f.1).max().unwrap();

        let mut x = 0;
        while simulate_sand(&rock, &mut sand, source, |sand| lowest_point < sand.1, None) {
            x += 1;
        }

        x.to_string()
    }

    fn part2(&self, input: &str) -> String {
        let rock = parse(input);
        let mut sand = HashSet::new();
        let source = (500, 0);
        let lowest_point = rock.iter().map(|f| f.1).max().unwrap();

        let mut x = 0;
        while simulate_sand(
            &rock,
            &mut sand,
            source,
            |sand| sand == source,
            Some(lowest_point + 2),
        ) {
            x += 1;
        }

        for y in 0..=11 {
            for x in 490..=507 {
                print!(
                    "{}",
                    if rock.contains(&(x, y)) {
                        "#"
                    } else if y == lowest_point + 2 {
                        "#"
                    } else if (x, y) == source {
                        "+"
                    } else if sand.contains(&(x, y)) {
                        "o"
                    } else {
                        "."
                    }
                )
            }
            println!()
        }

        (x + 1).to_string()
    }
}

fn parse(input: &str) -> HashSet<(isize, isize)> {
    input.lines().fold(HashSet::new(), |mut acc, f| {
        for (a, b) in f.split(" -> ").tuple_windows() {
            let (a_x, a_y) = a.split_once(",").unwrap();
            let (a_x, a_y): (isize, isize) = (a_x.parse().unwrap(), a_y.parse().unwrap());

            let (b_x, b_y) = b.split_once(",").unwrap();
            let (b_x, b_y): (isize, isize) = (b_x.parse().unwrap(), b_y.parse().unwrap());

            let min_x = a_x.min(b_x);
            let max_x = a_x.max(b_x);

            let min_y = a_y.min(b_y);
            let max_y = a_y.max(b_y);

            if min_x == max_x {
                for y in min_y..=max_y {
                    acc.insert((min_x, y));
                }
            }

            if min_y == max_y {
                for x in min_x..=max_x {
                    acc.insert((x, min_y));
                }
            }
        }
        acc
    })
}
