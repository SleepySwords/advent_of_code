// Credit due to: https://www.reddit.com/r/adventofcode/comments/zmfwg1/2022_day_15_part_2_seekin_for_the_beacon/
// Which I happen to have spotted while browsing the AOC subreddit.

use std::{collections::HashSet, error::Error};

use advent_of_code_lib::{self, Solver};
use itertools::Itertools;
use lazy_static::lazy_static;
use regex::Regex;

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2022", "15")
}

struct Day;

type Position = (isize, isize);

lazy_static! {
    static ref REGEX: Regex =
        Regex::new(r"Sensor at x=(.+), y=(.+): closest beacon is at x=(.+), y=(.+)").unwrap();
}

fn manhatten_distance(source: Position, destination: Position) -> usize {
    (source.0).abs_diff(destination.0) + (source.1).abs_diff(destination.1)
}

fn distance_from_y(pos: &mut HashSet<isize>, y_level: isize, sensor: Position, beacon: Position) {
    let strength = manhatten_distance(sensor, beacon);
    if sensor.1.abs_diff(y_level) > strength {
        return;
    }
    // Could probably use ranges
    pos.insert(sensor.0);
    for x in 1..=sensor.1.abs_diff(y_level).abs_diff(strength) {
        pos.insert(sensor.0 - x as isize);
        pos.insert(sensor.0 + x as isize);
    }
}

fn intersects(test_for: Position, sensor: Position, strength: usize) -> bool {
    manhatten_distance(test_for, sensor) <= strength
}

fn ball_iterator(x: isize, y: isize, r: isize) -> impl Iterator<Item = Position> {
    // ..g..
    // .x.g.
    // x.o.g
    // .x.x.
    // ..x..
    let bottom_right = (x..=(x + r)).zip((y..=(y + r)).rev());

    // ..x..
    // .x.x.
    // x.o.g
    // .x.g.
    // ..g..
    let top_right = (x..=(x + r)).zip((y - r)..=y);

    // ..x..
    // .x.x.
    // g.o.x
    // .g.x.
    // ..g..
    let bottom_left = ((x - r)..=x).zip(((y - r)..=y).rev());

    // ..x..
    // .x.x.
    // g.o.x
    // .g.x.
    // ..g..
    let top_left = ((x - r)..=x).zip(y..=(y + r));

    top_right
        .chain(top_left)
        .chain(bottom_right)
        .chain(bottom_left)
}

// Treat them as bounding boxes?

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        let mut x = input
            .lines()
            .map(|f| {
                let captures = REGEX.captures(f).unwrap();
                let sensor = (
                    captures.get(1).unwrap().as_str().parse::<isize>().unwrap(),
                    captures.get(2).unwrap().as_str().parse::<isize>().unwrap(),
                );
                let beacon = (
                    captures.get(3).unwrap().as_str().parse::<isize>().unwrap(),
                    captures.get(4).unwrap().as_str().parse::<isize>().unwrap(),
                );
                (sensor, beacon)
            })
            .fold((HashSet::new(), Vec::new()), |mut acc, (sensor, beacon)| {
                distance_from_y(&mut acc.0, 2000000, sensor, beacon);
                acc.1.push(beacon);
                acc
            });
        x.1.iter().filter(|x| x.1 == 2000000).for_each(|f| {
            x.0.remove(&f.0);
        });
        x.0.len().to_string()
    }

    fn part2(&self, input: &str) -> String {
        let beacons = input
            .lines()
            .map(|f| {
                let captures = REGEX.captures(f).unwrap();
                let sensor = (
                    captures.get(1).unwrap().as_str().parse::<isize>().unwrap(),
                    captures.get(2).unwrap().as_str().parse::<isize>().unwrap(),
                );
                let beacon = (
                    captures.get(3).unwrap().as_str().parse::<isize>().unwrap(),
                    captures.get(4).unwrap().as_str().parse::<isize>().unwrap(),
                );
                (sensor, manhatten_distance(sensor, beacon))
            })
            .collect_vec();
        for (sensor, distance) in &beacons {
            for (x, y) in ball_iterator(sensor.0, sensor.1, *distance as isize + 1)
                .filter(|(x, y)| (0..=4_000_000).contains(x) && (0..=4_000_000).contains(y))
            {
                if beacons
                    .iter()
                    .all(|(sensor, distance)| !intersects((x, y), *sensor, *distance))
                {
                    return (x * 4_000_000 + y).to_string()
                }
            }
        }
        "Not_found".to_string()
    }
}
