use std::{error::Error, collections::HashMap};

use advent_of_code_lib::{self, Solver};
use lazy_static::lazy_static;
use regex::Regex;
use rayon::prelude::*;

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2022", "19")
}

struct Day;

enum Ore {
    Ore,
    Clay,
    Obsidian,
    Geode,
}

struct Blueprint {
    ore_cost: [usize; 3],
    clay_cost: [usize; 3],
    obsidian_cost: [usize; 3],
    geode_cost: [usize; 3],
}

impl Blueprint {
    fn cost(&self, id: usize) -> [usize; 3] {
        if id == 0 {
            return self.ore_cost;
        } else if id == 1 {
            return self.clay_cost;
        } else if id == 2 {
            return self.obsidian_cost;
        } else if id == 3 {
            return self.geode_cost;
        }
        unreachable!()
    }
}

fn can_purchase(cost: [usize; 3], resources: [usize; 3]) -> bool {
    resources[0] >= cost[0] && resources[1] >= cost[1] && resources[2] >= cost[2]
}

fn purchase(cost: [usize; 3], resources: [usize; 3]) -> [usize; 3] {
    [
        resources[0] - cost[0],
        resources[1] - cost[1],
        resources[2] - cost[2],
    ]
}

fn turn(
    blueprint: &Blueprint,
    robots: [usize; 4],
    resources: [usize; 3],
    skip_build: [bool; 4],
    minutes: usize,
) -> usize {
    // println!("{} {:?} {:?} {}", minutes, resources, robots, robots[3]);
    if minutes == 0 {
        return 0;
    }

    // adding the robots
    let resources_after_mined = [
        resources[0] + robots[0],
        resources[1] + robots[1],
        resources[2] + robots[2],
    ];

    let mut to_skip = (0..4).map(|ore| skip_build[ore] || can_purchase(blueprint.cost(ore), resources));
    let to_skip = [
        to_skip.next().unwrap(),
        to_skip.next().unwrap(),
        to_skip.next().unwrap(),
        to_skip.next().unwrap(),
    ];

    // If you did not purchase, add to skip.
    let max = [0usize, 1usize, 2usize, 3usize].par_iter()
        .filter(|&&ore| !skip_build[ore])
        .filter(|&&ore| can_purchase(blueprint.cost(ore), resources))
        .map(|&ore| {
            let resources = purchase(blueprint.cost(ore), resources_after_mined);
            let mut new_robots = robots;
            new_robots[ore] += 1;
            turn(blueprint, new_robots, resources, [false; 4], minutes - 1) + robots[3]
        })
        .max()
        .unwrap_or(0);

    // No purchase
    max.max(
        turn(
            blueprint,
            robots,
            resources_after_mined,
            to_skip,
            minutes - 1,
        ) + robots[3],
    )
}

lazy_static! {
    static ref BLUEPRINT_REGEX: Regex = Regex::new(r"Blueprint .+: Each ore robot costs (.+) ore. Each clay robot costs (.+) ore. Each obsidian robot costs (.+) ore and (.+) clay. Each geode robot costs (.+) ore and (.+) obsidian.").unwrap();
}

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        let sum = input
            .lines()
            .enumerate()
            .map(|(i, f)| {
                let captures = BLUEPRINT_REGEX.captures(f).unwrap();
                let ore_cost = [
                    captures.get(1).unwrap().as_str().parse::<usize>().unwrap(),
                    0,
                    0,
                ];
                let clay_cost = [
                    captures.get(2).unwrap().as_str().parse::<usize>().unwrap(),
                    0,
                    0,
                ];
                let obsidian_cost = [
                    captures.get(3).unwrap().as_str().parse::<usize>().unwrap(),
                    captures.get(4).unwrap().as_str().parse::<usize>().unwrap(),
                    0,
                ];
                let geode_cost = [
                    captures.get(5).unwrap().as_str().parse::<usize>().unwrap(),
                    0,
                    captures.get(6).unwrap().as_str().parse::<usize>().unwrap(),
                ];
                let blueprint = Blueprint {
                    ore_cost,
                    clay_cost,
                    obsidian_cost,
                    geode_cost,
                };
                let x = turn(&blueprint, [1, 0, 0, 0], [0; 3], [false; 4], 24);
                println!("Completed: {} {}", i, x);
                (i, x)
            })
            .map(|(i, a)| (i + 1) * a)
            .sum::<usize>();

        println!("{:?}", sum);

        sum.to_string()
    }

    fn part2(&self, input: &str) -> String {
        input
            .lines()
            .take(3)
            .map(|f| {
                let captures = BLUEPRINT_REGEX.captures(f).unwrap();
                let ore_cost = [
                    captures.get(1).unwrap().as_str().parse::<usize>().unwrap(),
                    0,
                    0,
                ];
                let clay_cost = [
                    captures.get(2).unwrap().as_str().parse::<usize>().unwrap(),
                    0,
                    0,
                ];
                let obsidian_cost = [
                    captures.get(3).unwrap().as_str().parse::<usize>().unwrap(),
                    captures.get(4).unwrap().as_str().parse::<usize>().unwrap(),
                    0,
                ];
                let geode_cost = [
                    captures.get(5).unwrap().as_str().parse::<usize>().unwrap(),
                    0,
                    captures.get(6).unwrap().as_str().parse::<usize>().unwrap(),
                ];
                let blueprint = Blueprint {
                    ore_cost,
                    clay_cost,
                    obsidian_cost,
                    geode_cost,
                };
                let x = turn(&blueprint, [1, 0, 0, 0], [0; 3], [false; 4], 32);
                println!("Completed {}", x);
                x
            })
            .product::<usize>()
            .to_string()
    }
}

// #[test]
// fn test_file1() -> Result<(), Box<dyn Error>> {
//     advent_of_code_lib::test_file(Day, "test1", advent_of_code_lib::Part::Part1)
// }
