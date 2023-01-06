use std::{
    collections::{HashSet, VecDeque},
    error::Error,
};

use advent_of_code_lib::{self, Solver};
use lazy_static::lazy_static;
use regex::Regex;

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2022", "19")
}

struct Day;

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
struct State {
    robots: [u16; 4],
    resources: [u16; 3],
    geode: u16,
    skip_build: u8,
    minutes: u8,
}

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
struct CacheState {
    robots: [u16; 4],
    resources: [u16; 3],
    geode: u16,
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

    fn max_robots(&self, id: usize) -> u16 {
        if id == 3 { return u16::MAX }
        self.ore_cost[id].max(self.clay_cost[id]).max(self.obsidian_cost[id]).max(self.geode_cost[id])
    }
}

fn can_purchase(cost: &[u16; 3], resources: &[u16; 3]) -> bool {
    resources[0] >= cost[0] && resources[1] >= cost[1] && resources[2] >= cost[2]
}

fn purchase(cost: &[u16; 3], resources: &[u16; 3]) -> [u16; 3] {
    [
        resources[0] - cost[0],
        resources[1] - cost[1],
        resources[2] - cost[2],
    ]
}

fn turn(
    blueprint: &Blueprint,
    cache: &mut HashSet<CacheState>,
    State {
        robots,
        geode,
        resources,
        skip_build,
        minutes,
    }: State,
) -> u16 {
    let mut queue = VecDeque::new();

    queue.push_back(State {
        robots,
        geode,
        resources,
        skip_build,
        minutes,
    });

    let mut max = 0;

    while let Some(State {
        robots,
        mut geode,
        resources,
        skip_build,
        mut minutes,
    }) = queue.pop_front()
    {
        if cache.contains(&CacheState {
            robots,
            resources,
            geode,
        }) {
            // println!("repeated {:?} {:?} {geode} {minutes} {skip_build}", robots, resources);
            continue;
        } else {
            cache.insert(CacheState {
                robots,
                resources,
                geode,
            });
        }

        if minutes == 0 || skip_build == 1 + 2 + 4 + 8 || (geode as u128 + robots[3] as u128 * minutes as u128 + (0..(minutes - 1) as u128).product::<u128>()) < max as u128 {
            max = geode.max(max);
            continue;
        }

        // adding the robots
        let resources_after_mined = [
            resources[0] + robots[0],
            resources[1] + robots[1],
            resources[2] + robots[2],
        ];

        minutes -= 1;
        geode += robots[3];

        let to_skip = (0..4)
            .map(|ore| {
                if skip_build >> ore & 1 == 1 || can_purchase(&blueprint.cost(ore as u16), &resources)
                {
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
        for s in (0..4)
            .filter(|&ore| skip_build >> ore & 1 == 0 && can_purchase(&blueprint.cost(ore as u16), &resources))
            .filter(|&ore| blueprint.max_robots(ore) > robots[ore]) // Credit to https://nickymeuleman.netlify.app/garden/aoc2022-day19/ for this optimisation (speeds up a ton)
            .map(|ore| {
                let resources = purchase(&blueprint.cost(ore as u16), &resources_after_mined);
                let mut new_robots = robots;
                new_robots[ore] += 1;
                State {
                    robots: new_robots,
                    geode,
                    resources,
                    skip_build: 0,
                    minutes,
                }
            })
        {
            queue.push_back(s);
        }

        // No purchase
        queue.push_back(State {
            robots,
            geode,
            resources: resources_after_mined,
            skip_build: to_skip,
            minutes,
        });
        // println!("{}", max);
    }

    // cache.insert(
    //     State {
    //         robots,
    //         resources,
    //         skip_build,
    //         minutes,
    //     },
    //     r,
    // );
    // r
    max
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
                let x = turn(
                    &blueprint,
                    &mut HashSet::new(),
                    State {
                        robots: [1, 0, 0, 0],
                        geode: 0,
                        resources: [0; 3],
                        skip_build: 0,
                        minutes: 24,
                    },
                );
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
                let x = turn(
                    &blueprint,
                    &mut HashSet::new(),
                    State {
                        robots: [1, 0, 0, 0],
                        geode: 0,
                        resources: [0; 3],
                        skip_build: 0,
                        minutes: 32,
                    },
                );
                println!("Completed {}", x);
                x
            })
            .product::<u16>()
            .to_string()
    }
}
