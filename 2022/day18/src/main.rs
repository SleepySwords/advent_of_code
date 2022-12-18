use std::{
    collections::{HashSet, VecDeque},
    error::Error,
};

use advent_of_code_lib::{self, Solver};
use itertools::Itertools;
use lazy_static::lazy_static;

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2022", "18")
}

struct Day;

type Position = (isize, isize, isize);

fn add_position(a: Position, b: Position) -> Position {
    (a.0 + b.0, a.1 + b.1, a.2 + b.2)
}

fn is_adjacent(a: Position, b: Position) -> bool {
    POS.iter().map(|f| add_position(*f, a)).any(|f| f == b)
}

lazy_static! {
    static ref POS: Vec<Position> = vec![
        (1, 0, 0),
        (-1, 0, 0),
        (0, 1, 0),
        (0, -1, 0),
        (0, 0, 1),
        (0, 0, -1),
    ];
}

fn is_outside(cubes: &HashSet<Position>, position: Position) -> (bool, HashSet<Position>) {
    let mut path = HashSet::new();
    let mut queue: VecDeque<Position> = VecDeque::new();
    queue.push_front(position);
    path.insert(position);
    while let Some(node) = queue.pop_front() {
        if node.0 == 0 || node.1 == 0 || node.2 == 0 {
            return (true, path);
        }
        if node.0 == 100 || node.1 == 100 || node.2 == 100 {
            return (true, path);
        }
        for pos in POS
            .iter()
            .map(|f| add_position(*f, node))
            .filter(|f| !path.contains(f) && !cubes.contains(f))
            .collect_vec()
        {
            queue.push_back(pos);
            path.insert(pos);
        }
    }
    (false, path)
}

fn find_all_connected(cubes: &HashSet<Position>, position: Position) -> HashSet<Position> {
    let mut path = HashSet::new();
    let mut queue: VecDeque<Position> = VecDeque::new();
    queue.push_front(position);
    path.insert(position);
    while let Some(node) = queue.pop_front() {
        if node.0 < 0 || node.1 < 0 || node.2 < 0 {
            continue;
        }
        if node.0 > 100 || node.1 > 100 || node.2 > 100 {
            continue;
        }
        for pos in POS
            .iter()
            .map(|f| add_position(*f, node))
            .filter(|f| !path.contains(f) && !cubes.contains(f))
            .collect_vec()
        {
            queue.push_back(pos);
            path.insert(pos);
        }
    }
    path
}

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        let mut cubes_sides = input.lines().count() * 6;
        let mut cubes = HashSet::new();
        for line in input.lines() {
            let mut i = line.split(",");
            let x = i.next().unwrap().parse().unwrap();
            let y = i.next().unwrap().parse().unwrap();
            let z = i.next().unwrap().parse().unwrap();
            for c in &cubes {
                if is_adjacent(*c, (x, y, z)) {
                    cubes_sides -= 2;
                }
            }
            cubes.insert((x, y, z));
        }

        cubes_sides.to_string()
    }

    fn part2(&self, input: &str) -> String {
        let mut cubes = HashSet::new();
        for line in input.lines() {
            let mut i = line.split(",");
            let x = i.next().unwrap().parse().unwrap();
            let y = i.next().unwrap().parse().unwrap();
            let z = i.next().unwrap().parse().unwrap();
            cubes.insert((x, y, z));
        }

        let mut outside = HashSet::new();
        for x in 0..100 {
            for y in 0..100 {
                for z in 0..100 {
                    let position = (x, y, z);
                    if !cubes.contains(&position) && !outside.contains(&position) {
                        let (is_outside, path) = is_outside(&cubes, position);
                        if is_outside {
                            for a in find_all_connected(&cubes, position){
                                outside.insert(a);
                            }
                            for a in path {
                                outside.insert(a);
                            }
                        } else {
                            for a in path {
                                cubes.insert(a);
                            }
                        }
                    }
                }
            }
        }

        let mut cubes_sides = cubes.len() * 6;
        let mut checked = HashSet::new();
        for c in cubes {
            for a in &checked {
                if is_adjacent(*a, c) {
                    cubes_sides -= 2;
                }
            }
            checked.insert(c);
        }

        cubes_sides.to_string()
    }
}
