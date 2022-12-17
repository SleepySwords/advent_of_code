// Credit to the person on the AOC subreddit who mentioned DP bitset. Making me realise I probably
// should not be cloning the path EVERY SINGLE TIME I GO TO THE NEXT NODE.


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
    flow_value: usize,
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
    valves: &HashMap<usize, (usize, Vec<usize>)>,
    distance: &HashMap<(usize, usize), usize>,
    position: usize,
    mut opened: u16,
    minutes: isize,
) -> usize {
    if minutes <= 0 {
        return 0;
    }
    opened = opened | 1 << position;
    let (flow_rate, neighbours) = &valves[&position];
    let total_release = flow_rate * minutes as usize;
    neighbours
        .iter()
        .filter(|neighbour| opened >> *neighbour & 1 == 0)
        .map(|neighbour| {
            dfs(
                valves,
                distance,
                *neighbour,
                opened,
                minutes - distance[&(position, *neighbour)] as isize - 1,
            ) + total_release
        })
        .max()
        .unwrap_or(total_release)
}

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
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
        let mut distances = HashMap::new();

        let mut valid_valves = grid.iter().filter(|f| f.flow_value > 0).map(|f| f.valve_name).collect_vec();
        valid_valves.insert(0, "AA");

        for x in 0..valid_valves.len() {
            for y in 0..valid_valves.len() {
                distances.insert((x, y), bfs_sp(&grid, valid_valves[x], valid_valves[y]).len() - 1);
            }
            let valve = grid.iter().find(|f| f.valve_name == valid_valves[x]).unwrap();
            valves.insert(x, (valve.flow_value, (0..valid_valves.len()).filter(|f| *f != x).collect_vec()));
        }

        dfs(&valves, &distances, 0, 0, 30).to_string()
    }

    // fn part2(&self, input: &str) -> String {
    // }
}
