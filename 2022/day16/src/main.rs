// Credit to the person on the AOC subreddit who mentioned DP bitset. Making me realise I probably
// should not be cloning the path EVERY SINGLE TIME I GO TO EXPLORE NEXT NODE.

use std::{
    collections::{HashMap, VecDeque},
    error::Error,
};

use advent_of_code_lib::{self, Solver};
use itertools::Itertools;
use lazy_static::lazy_static;
use regex::Regex;

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2022", "16")
}

lazy_static! {
    static ref VALVE_REGEX: Regex =
        Regex::new(r"Valve (.+) has flow rate=(.+); tunnels? leads? to valves? (.+)").unwrap();
}

struct Day;

struct Valve<'a> {
    valve_name: &'a str,
    connected_valves: Vec<&'a str>,
    flow_value: u32,
}

fn bfs_sp<'a>(grid: &Vec<Valve<'a>>, start_position: &'a str, goal: &str) -> Vec<&'a str> {
    let mut queue = VecDeque::new();
    queue.push_front(vec![start_position]);
    if start_position == goal {
        return vec![start_position];
    }
    while let Some(path) = queue.pop_front() {
        let node = path.last().unwrap();
        for f in &grid
            .iter()
            .find(|f| f.valve_name == *node)
            .unwrap()
            .connected_valves
        {
            let f = grid.iter().find(|a| a.valve_name == *f).unwrap();
            if !path.contains(&f.valve_name) {
                let mut new_path = path.clone();
                new_path.push(f.valve_name);
                if f.valve_name == goal {
                    return new_path;
                }
                queue.push_back(new_path);
            }
        }
    }
    unreachable!()
}

fn dfs(
    valves: &HashMap<u32, (u32, Vec<u32>)>,
    distance: &HashMap<(u32, u32), u32>,
    position: u32,
    mut opened: u16,
    minutes: i32,
) -> u32 {
    if minutes <= 0 {
        return 0;
    }
    opened = opened | 1 << position;
    let (flow_rate, neighbours) = &valves[&position];
    let total_release = flow_rate * minutes as u32;
    neighbours
        .iter()
        .filter(|neighbour| opened >> *neighbour & 1 == 0)
        .map(|neighbour| {
            dfs(
                valves,
                distance,
                *neighbour,
                opened,
                minutes - distance[&(position, *neighbour)] as i32 - 1, // distance for travel - 1 for
                                                                          // openning it.
            ) + total_release
        })
        .max()
        .unwrap_or(total_release)
}

// Assumtions, the elephant and the human must move to the next place immediately after opening a
// valve.

fn select_next(
    valves: &HashMap<u32, (u32, Vec<u32>)>,
    // cache: &mut HashMap<(u32, i32, u32, i32, u16, i32), u32>,
    distance: &HashMap<(u32, u32), u32>,
    h_position: u32,
    h_progress: i32,
    e_position: u32,
    e_progress: i32,
    opened: u16,
    minutes: i32,
) -> u32 {
    // if let Some(r) = cache.get(&(
    //     h_position, h_progress, e_position, e_progress, opened, minutes,
    // )) {
    //     return *r;
    // }
    let r = if e_progress == h_progress {
        let release = both_open(
            valves,
            // cache,
            distance,
            h_position,
            e_position,
            opened,
            minutes - h_progress,
        );
        release
    } else if e_progress > h_progress {
        let release = human_open(
            valves,
            // cache,
            distance,
            h_position,
            e_position,
            e_progress - h_progress,
            opened,
            minutes - h_progress,
        );
        release
    } else {
        let release = elephant_open(
            valves,
            // cache,
            distance,
            h_position,
            h_progress - e_progress,
            e_position,
            opened,
            minutes - e_progress,
        );
        release
    };
    // println!("{}", cache.len());
    // cache.insert(
    //     (
    //         h_position, h_progress, e_position, e_progress, opened, minutes,
    //     ),
    //     r,
    // );
    r
}

fn human_open(
    valves: &HashMap<u32, (u32, Vec<u32>)>,
    // cache: &mut HashMap<(u32, i32, u32, i32, u16, i32), u32>,
    distance: &HashMap<(u32, u32), u32>,
    h_position: u32,
    e_position: u32,
    e_progress: i32,
    mut opened: u16,
    minutes: i32,
) -> u32 {
    if minutes <= 0 || (0..valves.len()).map(|f| 1 << f).sum::<u16>() == opened {
        return 0;
    }
    let total_release = valves[&h_position].0 * (minutes - 1) as u32;
    opened = opened | 1 << h_position;
    valves
        .keys()
        .filter(|neighbour| opened >> *neighbour & 1 == 0 && **neighbour != e_position)
        .map(|neighbour| {
            let h_progress = distance[&(h_position, *neighbour)] as i32;
            select_next(
                valves,
                // cache,
                distance,
                *neighbour,
                h_progress,
                e_position,
                e_progress - 1, // Compensate for opening a valve
                opened,
                minutes - 1, // subtract one to open a valve
            )
        })
        .max()
        .unwrap_or(0)
        + total_release
}

fn both_open(
    valves: &HashMap<u32, (u32, Vec<u32>)>,
    // cache: &mut HashMap<(u32, i32, u32, i32, u16, i32), u32>,
    distance: &HashMap<(u32, u32), u32>,
    h_position: u32,
    e_position: u32,
    mut opened: u16,
    minutes: i32,
) -> u32 {
    if minutes <= 0 || (0..valves.len()).map(|f| 1 << f).sum::<u16>() == opened {
        return 0;
    }
    let total_release = valves[&h_position].0 * (minutes - 1) as u32
        + valves[&e_position].0 * (minutes - 1) as u32;
    opened = opened | 1 << h_position;
    opened = opened | 1 << e_position;
    valves
        .keys()
        .filter(|neighbour| opened >> *neighbour & 1 == 0)
        .permutations(2)
        .map(|neighbours| {
            let h_neighbour = neighbours[0];
            let e_neighbour = neighbours[1];
            let h_progress = distance[&(h_position, *h_neighbour)] as i32;
            let e_progress = distance[&(e_position, *e_neighbour)] as i32;
            select_next(
                valves,
                // cache,
                distance,
                *h_neighbour,
                h_progress,
                *e_neighbour,
                e_progress,
                opened,
                minutes - 1, // subtract one to open a valve
            )
        })
        .max()
        .unwrap_or(0)
        + total_release
}

fn elephant_open(
    valves: &HashMap<u32, (u32, Vec<u32>)>,
    // cache: &mut HashMap<(u32, i32, u32, i32, u16, i32), u32>,
    distance: &HashMap<(u32, u32), u32>,
    h_position: u32,
    h_progress: i32,
    e_position: u32,
    mut opened: u16,
    minutes: i32,
) -> u32 {
    if minutes <= 0 || (0..valves.len()).map(|f| 1 << f).sum::<u16>() == opened {
        return 0;
    }
    let total_release = valves[&e_position].0 * (minutes - 1) as u32;
    opened = opened | 1 << e_position;
    valves
        .keys()
        .filter(|neighbour| opened >> *neighbour & 1 == 0 && **neighbour != h_position)
        .map(|neighbour| {
            let e_progress = distance[&(e_position, *neighbour)] as i32;
            select_next(
                valves,
                // cache,
                distance,
                h_position,
                h_progress - 1, // Compensate for opening a valve (and taking a turn)
                *neighbour,
                e_progress,
                opened,
                minutes - 1, // Subtract one for opening the valve
            )
        })
        .max()
        .unwrap_or(0)
        + total_release
}

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        let grid = input
            .lines()
            .map(|f| {
                println!("{}", f);
                let captures = VALVE_REGEX.captures(f).unwrap();
                let valve_name = captures.get(1).unwrap().as_str();
                let flow_value = captures.get(2).unwrap().as_str().parse::<u32>().unwrap();
                let connected_valves = captures.get(3).unwrap().as_str().split(", ").collect_vec();
                Valve {
                    flow_value,
                    valve_name,
                    connected_valves,
                }
            })
            .collect_vec();
        let mut valves = HashMap::new();
        let mut distances = HashMap::new();

        let mut valid_valves = grid
            .iter()
            .filter(|f| f.flow_value > 0)
            .map(|f| f.valve_name)
            .collect_vec();
        valid_valves.insert(0, "AA");

        for x in 0..valid_valves.len() {
            for y in 0..valid_valves.len() {
                distances.insert(
                    (x as u32, y as u32),
                    (bfs_sp(&grid, valid_valves[x], valid_valves[y]).len() - 1) as u32,
                );
            }
            let valve = grid
                .iter()
                .find(|f| f.valve_name == valid_valves[x])
                .unwrap();
            valves.insert(
                x as u32,
                (
                    valve.flow_value,
                    (0..valid_valves.len()).filter(|f| *f != x).map(|f| f as u32).collect_vec(),
                ),
            );
        }

        dfs(&valves, &distances, 0, 0, 30).to_string()
    }

    fn part2(&self, input: &str) -> String {
        let grid = input
            .lines()
            .map(|f| {
                let captures = VALVE_REGEX.captures(f).unwrap();
                let valve_name = captures.get(1).unwrap().as_str();
                let flow_value = captures.get(2).unwrap().as_str().parse().unwrap();
                let connected_valves = captures.get(3).unwrap().as_str().split(", ").collect_vec();
                Valve {
                    flow_value,
                    valve_name,
                    connected_valves,
                }
            })
            .collect_vec();
        let mut valves = HashMap::new();
        let mut distance = HashMap::new();

        let mut valid_valves = grid
            .iter()
            .filter(|f| f.flow_value > 0)
            .map(|f| f.valve_name)
            .collect_vec();
        valid_valves.insert(0, "AA");

        for x in 0..valid_valves.len() {
            for y in 0..valid_valves.len() {
                distance.insert(
                    (x as u32, y as u32),
                    (bfs_sp(&grid, valid_valves[x], valid_valves[y]).len() - 1) as u32,
                );
            }
            let valve = grid
                .iter()
                .find(|f| f.valve_name == valid_valves[x])
                .unwrap();
            valves.insert(
                x as u32,
                (
                    valve.flow_value,
                    (0..valid_valves.len()).filter(|f| *f != x).map(|f| f as u32).collect_vec(),
                ),
            );
        }

        // let mut cache = HashMap::new();

        let release = valves
            .keys()
            .filter(|&&f| f != 0)
            .permutations(2)
            .map(|neighbours| {
                let h_neighbour = neighbours[0];
                let e_neighbour = neighbours[1];
                let h_progress = distance[&(0, *h_neighbour)] as i32;
                let e_progress = distance[&(0, *e_neighbour)] as i32;
                // if *h_neighbour == valid_valves.iter().position(|&f| f == "JJ").unwrap()
                //     && *e_neighbour == valid_valves.iter().position(|&f| f == "DD").unwrap()
                // {
                //     println!("{:?}", valid_valves.iter().enumerate().collect_vec());
                //     println!(
                //         "ok {}",
                let r = select_next(
                    &valves,
                    // &mut cache,
                    &distance,
                    *h_neighbour,
                    h_progress,
                    *e_neighbour,
                    e_progress,
                    0,
                    26,
                );
                println!("{}", r);
                r
                // );
                // }
                // 0
            })
            .max()
            .unwrap();
        release.to_string()
    }
}
