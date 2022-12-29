// Credit to the person on the AOC subreddit who mentioned DP bitset. Making me realise I probably
// should not be cloning the path EVERY SINGLE TIME I GO TO EXPLORE NEXT NODE.

use std::{
    collections::{hash_map::ValuesMut, HashMap, VecDeque},
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
                minutes - distance[&(position, *neighbour)] as isize - 1, // distance for travel - 1 for
                                                                          // openning it.
            ) + total_release
        })
        .max()
        .unwrap_or(total_release)
}

// Assumtions, the elephant and the human must move to the next place immediately after opening a
// valve.

fn select_next(
    valves: &HashMap<usize, (usize, Vec<usize>)>,
    distance: &HashMap<(usize, usize), usize>,
    h_position: usize,
    h_progress: isize,
    e_position: usize,
    e_progress: isize,
    mut opened: u16,
    minutes: isize,
) -> (usize, Vec<usize>, Vec<usize>) {
    if e_progress == h_progress {
        let (release, mut h, mut e) = dfs_both(
            valves,
            distance,
            *neighbour,
            e_position,
            opened,
            minutes - 1 - h_progress,
        );
        h.push(h_position);
        return (release + total_release, h, e);
    } else if e_progress > h_progress {
        let (release, mut h, mut e) = dfs_human(
            valves,
            distance,
            *neighbour,
            e_position,
            e_progress - h_progress,
            opened,
            minutes - 1 - h_progress,
        );
        h.push(h_position);
        return (release + total_release, h, e);
    } else {
        let (release, mut h, mut e) = dfs_elephant(
            valves,
            distance,
            *neighbour,
            h_progress - e_progress,
            e_position,
            opened,
            minutes - 1 - e_progress,
        );
        h.push(h_position);
        return (release + total_release, h, e);
    }
}

fn dfs_human(
    valves: &HashMap<usize, (usize, Vec<usize>)>,
    distance: &HashMap<(usize, usize), usize>,
    h_position: usize,
    e_position: usize,
    e_progress: isize,
    mut opened: u16,
    minutes: isize,
) -> (usize, Vec<usize>, Vec<usize>) {
    if minutes <= 0 {
        return (0, vec![h_position], vec![e_position]);
    }
    let total_release = valves[&h_position].0 * minutes as usize;
    opened = opened | 1 << h_position;
    valves
        .keys()
        .filter(|neighbour| opened >> *neighbour & 1 == 0 && **neighbour != e_position)
        .map(|neighbour| {
            let h_progress = distance[&(h_position, *neighbour)] as isize;
            select_next()
        })
        .max_by(|a, b| a.0.cmp(&b.0))
        .unwrap_or((total_release, vec![h_position], vec![e_position]))
}

fn dfs_both(
    valves: &HashMap<usize, (usize, Vec<usize>)>,
    distance: &HashMap<(usize, usize), usize>,
    h_position: usize,
    e_position: usize,
    mut opened: u16,
    minutes: isize,
) -> (usize, Vec<usize>, Vec<usize>) {
    if minutes <= 0 {
        return (0, vec![h_position], vec![e_position]);
    }
    let total_release =
        valves[&h_position].0 * minutes as usize + valves[&e_position].0 * minutes as usize;
    opened = opened | 1 << h_position;
    opened = opened | 1 << e_position;
    valves
        .keys()
        .filter(|neighbour| opened >> *neighbour & 1 == 0)
        .permutations(2)
        .map(|neighbours| {
            let h_neighbour = neighbours[0];
            let e_neighbour = neighbours[1];
            let h_progress = distance[&(h_position, *h_neighbour)] as isize;
            let e_progress = distance[&(e_position, *e_neighbour)] as isize;
            if e_progress == h_progress {
                let (release, mut h, mut e) = dfs_both(
                    valves,
                    distance,
                    *h_neighbour,
                    *e_neighbour,
                    opened,
                    minutes - 1 - h_progress,
                );
                h.push(h_position);
                e.push(e_position);
                return (release + total_release, h, e);
            } else if e_progress > h_progress {
                let (release, mut h, mut e) = dfs_human(
                    valves,
                    distance,
                    *h_neighbour,
                    *e_neighbour,
                    e_progress - h_progress,
                    opened,
                    minutes - 1 - h_progress,
                );
                h.push(h_position);
                e.push(e_position);
                return (release + total_release, h, e);
            } else {
                let (release, mut h, mut e) = dfs_elephant(
                    valves,
                    distance,
                    *h_neighbour,
                    h_progress - e_progress,
                    *e_neighbour,
                    opened,
                    minutes - 1 - e_progress,
                );
                h.push(h_position);
                e.push(e_position);
                return (release + total_release, h, e);
            }
        })
        .max_by(|a, b| a.0.cmp(&b.0))
        .unwrap_or((total_release, vec![h_position], vec![e_position]))
}

fn dfs_elephant(
    valves: &HashMap<usize, (usize, Vec<usize>)>,
    distance: &HashMap<(usize, usize), usize>,
    h_position: usize,
    h_progress: isize,
    e_position: usize,
    mut opened: u16,
    minutes: isize,
) -> (usize, Vec<usize>, Vec<usize>) {
    if minutes <= 0 {
        return (0, vec![h_position], vec![e_position]);
    }
    let total_release = valves[&h_position].0 * minutes as usize;
    opened = opened | 1 << h_position;
    valves
        .keys()
        .filter(|neighbour| opened >> *neighbour & 1 == 0 && **neighbour != e_position)
        .map(|neighbour| {
            let e_progress = distance[&(h_position, *neighbour)] as isize;

            if e_progress == h_progress {
                let (release, mut h, mut e) = dfs_both(
                    valves,
                    distance,
                    *neighbour,
                    e_position,
                    opened,
                    minutes - 1 - h_progress,
                );
                e.push(e_position);
                return (release + total_release, h, e);
            } else if e_progress > h_progress {
                let (release, mut h, mut e) = dfs_human(
                    valves,
                    distance,
                    *neighbour,
                    e_position,
                    e_progress - h_progress,
                    opened,
                    minutes - 1 - h_progress,
                );
                e.push(e_position);
                return (release + total_release, h, e);
            } else {
                let (release, mut h, mut e) = dfs_elephant(
                    valves,
                    distance,
                    *neighbour,
                    h_progress - e_progress,
                    e_position,
                    opened,
                    minutes - 1 - e_progress,
                );
                e.push(e_position);
                return (release + total_release, h, e);
            }
        })
        .max_by(|a, b| a.0.cmp(&b.0))
        .unwrap_or((total_release, vec![h_position], vec![e_position]))
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

        let mut valid_valves = grid
            .iter()
            .filter(|f| f.flow_value > 0)
            .map(|f| f.valve_name)
            .collect_vec();
        valid_valves.insert(0, "AA");

        for x in 0..valid_valves.len() {
            for y in 0..valid_valves.len() {
                distances.insert(
                    (x, y),
                    bfs_sp(&grid, valid_valves[x], valid_valves[y]).len() - 1,
                );
            }
            let valve = grid
                .iter()
                .find(|f| f.valve_name == valid_valves[x])
                .unwrap();
            valves.insert(
                x,
                (
                    valve.flow_value,
                    (0..valid_valves.len()).filter(|f| *f != x).collect_vec(),
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
                    (x, y),
                    bfs_sp(&grid, valid_valves[x], valid_valves[y]).len() - 1,
                );
            }
            let valve = grid
                .iter()
                .find(|f| f.valve_name == valid_valves[x])
                .unwrap();
            valves.insert(
                x,
                (
                    valve.flow_value,
                    (0..valid_valves.len()).filter(|f| *f != x).collect_vec(),
                ),
            );
        }

        let (release, h, e) = valves
            .keys()
            .filter(|&&f| f != 0)
            .permutations(2)
            .map(|neighbours| {
                let h_neighbour = neighbours[0];
                let e_neighbour = neighbours[1];
                let h_progress = distance[&(0, *h_neighbour)] as isize;
                let e_progress = distance[&(0, *e_neighbour)] as isize;
                if e_progress == h_progress {
                    let (release, mut h, mut e) = dfs_both(
                        &valves,
                        &distance,
                        *h_neighbour,
                        *e_neighbour,
                        0,
                        26 - h_progress,
                    );
                    return (release, h, e);
                } else if e_progress > h_progress {
                    let (release, mut h, mut e) = dfs_human(
                        &valves,
                        &distance,
                        *h_neighbour,
                        *e_neighbour,
                        e_progress - h_progress,
                        0,
                        26 - h_progress,
                    );
                    return (release, h, e);
                } else {
                    let (release, mut h, mut e) = dfs_elephant(
                        &valves,
                        &distance,
                        *h_neighbour,
                        h_progress - e_progress,
                        *e_neighbour,
                        0,
                        26 - e_progress,
                    );
                    return (release, h, e);
                }
            })
            .max_by(|a, b| a.0.cmp(&b.0))
            .unwrap();
        println!("{:?}", h.iter().map(|f| valid_valves[*f]).collect_vec());
        println!("{:?}", e.iter().map(|f| valid_valves[*f]).collect_vec());
        println!(
            "{:?}",
            distance[&(0, valid_valves.iter().position(|&f| f == "JJ").unwrap())]
        );
        release.to_string()
    }
}
