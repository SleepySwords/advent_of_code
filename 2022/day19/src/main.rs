use std::{collections::HashMap, error::Error};

use advent_of_code_lib::{self, Solver};
use lazy_static::lazy_static;
use rayon::prelude::*;
use regex::Regex;

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
    ore_cost: [u16; 3],
    clay_cost: [u16; 3],
    obsidian_cost: [u16; 3],
    geode_cost: [u16; 3],
}

impl Blueprint {
    fn cost(&self, id: u16) -> [u16; 3] {
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

fn can_purchase(cost: [u16; 3], resources: [u16; 3]) -> bool {
    resources[0] >= cost[0] && resources[1] >= cost[1] && resources[2] >= cost[2]
}

fn purchase(cost: [u16; 3], resources: [u16; 3]) -> [u16; 3] {
    [
        resources[0] - cost[0],
        resources[1] - cost[1],
        resources[2] - cost[2],
    ]
}

fn turn(
    blueprint: &Blueprint,
    current_max: &mut u16,
    robots: [u16; 4],
    resources: [u16; 3],
    skip_build: u8,
    minutes: u8,
) -> u16 {
    // println!("{} {:} {:?} {}", minutes, resources, robots, robots[3]);
    let max: usize = (1..(minutes as usize + robots[3] as usize)).product();
    if max < *current_max as usize {
        // ie: don't consider this pathway.
        return 0;
    }
    if minutes == 0 {
        return 0;
    }

    // adding the robots
    let resources_after_mined = [
        resources[0] + robots[0],
        resources[1] + robots[1],
        resources[2] + robots[2],
    ];

    let to_skip = (0..4)
        .map(|ore| {
            if skip_build >> ore & 1 == 1 || can_purchase(blueprint.cost(ore as u16), resources) {
                1 << ore
            } else {
                0
            }
        })
        .sum::<u8>();

    // If you did not purchase, add to skip.
    //
    // When is it useless to buy a robot?
    // - When there is enough of that robot.
    // - Could cut down by having a 'global max' that has been calc and if it can't reach it, it
    // breaks of that pathway. (minutes * max geode ore can get) is less than max.
    let mut c_max = 0;
    let max = [0, 1, 2, 3]
        .iter()
        .filter(|&&ore| skip_build >> ore & 1 == 0)
        .filter(|&&ore| can_purchase(blueprint.cost(ore as u16), resources))
        .map(|&ore| {
            let resources = purchase(blueprint.cost(ore as u16), resources_after_mined);
            let mut new_robots = robots;
            new_robots[ore] += 1;
            turn(blueprint, &mut c_max, new_robots, resources, 0, minutes - 1)
        })
        .max()
        .unwrap_or(0);

    // No purchase
    let mut m =max.max(
        turn(
            blueprint,
            &mut c_max,
            robots,
            resources_after_mined,
            to_skip,
            minutes - 1,
        ),
    );
    if current_max < &mut m {
        *current_max = m;
    }
    m + robots[3]
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
                    captures.get(1).unwrap().as_str().parse::<u16>().unwrap(),
                    0,
                    0,
                ];
                let clay_cost = [
                    captures.get(2).unwrap().as_str().parse::<u16>().unwrap(),
                    0,
                    0,
                ];
                let obsidian_cost = [
                    captures.get(3).unwrap().as_str().parse::<u16>().unwrap(),
                    captures.get(4).unwrap().as_str().parse::<u16>().unwrap(),
                    0,
                ];
                let geode_cost = [
                    captures.get(5).unwrap().as_str().parse::<u16>().unwrap(),
                    0,
                    captures.get(6).unwrap().as_str().parse::<u16>().unwrap(),
                ];
                let blueprint = Blueprint {
                    ore_cost,
                    clay_cost,
                    obsidian_cost,
                    geode_cost,
                };
                let mut c = 0;
                let x = turn(&blueprint, &mut c, [1, 0, 0, 0], [0; 3], 0, 24);
                println!("Completed: {} {}", i, x);
                (i, x)
            })
            .map(|(i, a)| (i as u16 + 1) * a)
            .sum::<u16>();

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
                    captures.get(1).unwrap().as_str().parse::<u16>().unwrap(),
                    0,
                    0,
                ];
                let clay_cost = [
                    captures.get(2).unwrap().as_str().parse::<u16>().unwrap(),
                    0,
                    0,
                ];
                let obsidian_cost = [
                    captures.get(3).unwrap().as_str().parse::<u16>().unwrap(),
                    captures.get(4).unwrap().as_str().parse::<u16>().unwrap(),
                    0,
                ];
                let geode_cost = [
                    captures.get(5).unwrap().as_str().parse::<u16>().unwrap(),
                    0,
                    captures.get(6).unwrap().as_str().parse::<u16>().unwrap(),
                ];
                let blueprint = Blueprint {
                    ore_cost,
                    clay_cost,
                    obsidian_cost,
                    geode_cost,
                };
                let mut c = 0;
                let x = turn(&blueprint, &mut c, [1, 0, 0, 0], [0; 3], 0, 32);
                println!("Completed {}", x);
                x
            })
            .product::<u16>()
            .to_string()
    }
}

// #[test]
// fn test_file1() -> Result<(), Box<dyn Error>> {
//     advent_of_code_lib::test_file(Day, "test1", advent_of_code_lib::Part::Part1)
// }
