use core::panic;
use std::{collections::HashSet, error::Error};

use advent_of_code_lib::{self, Solver};
use itertools::Itertools;

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2023", "16")
}

struct Day;

#[derive(Clone, Copy)]
enum Tile {
    Empty,          // .
    VertSplit,      // |
    HorizSplit,     // -
    ForwardMirror,  // /
    BackwardMirror, // \
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    pub fn new_beams(&self, tile: &Tile) -> Vec<Direction> {
        match tile {
            Tile::Empty => vec![*self],
            Tile::VertSplit => match self {
                Direction::Up | Direction::Down => vec![*self],
                _ => vec![Direction::Up, Direction::Down],
            },
            Tile::HorizSplit => match self {
                Direction::Left | Direction::Right => vec![*self],
                _ => vec![Direction::Left, Direction::Right],
            },
            Tile::ForwardMirror => match self {
                Direction::Right => vec![Direction::Up],
                Direction::Left => vec![Direction::Down],
                Direction::Up => vec![Direction::Right],
                Direction::Down => vec![Direction::Left],
            },
            Tile::BackwardMirror => match self {
                Direction::Right => vec![Direction::Down],
                Direction::Left => vec![Direction::Up],
                Direction::Up => vec![Direction::Left],
                Direction::Down => vec![Direction::Right],
            },
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct Beam {
    x: isize,
    y: isize,
    direction: Direction,
}

impl Beam {
    fn move_pos(self) -> Self {
        let (x, y) = (self.x, self.y);
        let (new_x, new_y) = match self.direction {
            Direction::Up => (x, y - 1),
            Direction::Down => (x, y + 1),
            Direction::Left => (x - 1, y),
            Direction::Right => (x + 1, y),
        };
        return Beam {
            x: new_x,
            y: new_y,
            direction: self.direction,
        };
    }
}

fn simulate(
    map: &Vec<Vec<Tile>>,
    beam: Beam,
    seen_beams: &mut HashSet<Beam>,
    seen: &mut HashSet<(isize, isize)>,
) {
    let mut new_beam = beam.move_pos();
    // println!("{:?}", beam);
    if new_beam.x < 0 || new_beam.x as usize >= map[0].len() {
        return;
    }
    if new_beam.y < 0 || new_beam.y as usize >= map.len() {
        return;
    }
    seen.insert((new_beam.x, new_beam.y));

    for new_dir in beam
        .direction
        .new_beams(&map[new_beam.y as usize][new_beam.x as usize])
    {
        new_beam.direction = new_dir;
        if seen_beams.contains(&new_beam) {
            continue;
        }

        seen_beams.insert(new_beam);
        simulate(map, new_beam, seen_beams, seen);
    }
}

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        let map = input
            .lines()
            .map(|line| {
                line.chars()
                    .map(|c| match c {
                        '.' => Tile::Empty,
                        '|' => Tile::VertSplit,
                        '-' => Tile::HorizSplit,
                        '/' => Tile::ForwardMirror,
                        '\\' => Tile::BackwardMirror,
                        _ => panic!("Yoo?"),
                    })
                    .collect_vec()
            })
            .collect_vec();
        let start_beam = Beam {
            x: -1,
            y: 0,
            direction: Direction::Right,
        };
        let mut seen = HashSet::new();
        simulate(&map, start_beam, &mut HashSet::new(), &mut seen);
        seen.len().to_string()
    }

    fn part2(&self, input: &str) -> String {
        let map = input
            .lines()
            .map(|line| {
                line.chars()
                    .map(|c| match c {
                        '.' => Tile::Empty,
                        '|' => Tile::VertSplit,
                        '-' => Tile::HorizSplit,
                        '/' => Tile::ForwardMirror,
                        '\\' => Tile::BackwardMirror,
                        _ => panic!("Yoo?"),
                    })
                    .collect_vec()
            })
            .collect_vec();
        // DP + Merge could potentially be faster.
        // Calculate all the paths to the end from a tile to the end.
        // Use this as a cache to find all the seen tiles.
        //
        // Potentailly fun to implement later.
        let mut max = 0;
        for y in 0..map.len() {
            let start_beam = Beam {
                x: -1,
                y: y as isize,
                direction: Direction::Right,
            };
            let mut seen = HashSet::new();
            simulate(&map, start_beam, &mut HashSet::new(), &mut seen);
            if seen.len() > max {
                max = seen.len()
            }

            let start_beam = Beam {
                x: map[0].len() as isize,
                y: y as isize,
                direction: Direction::Left,
            };
            let mut seen = HashSet::new();
            simulate(&map, start_beam, &mut HashSet::new(), &mut seen);
            if seen.len() > max {
                max = seen.len()
            }
        }
        for x in 0..map.len() {
            let start_beam = Beam {
                x: x as isize,
                y: -1,
                direction: Direction::Down,
            };
            let mut seen = HashSet::new();
            simulate(&map, start_beam, &mut HashSet::new(), &mut seen);
            if seen.len() > max {
                max = seen.len()
            }

            let start_beam = Beam {
                y: map.len() as isize,
                x: x as isize,
                direction: Direction::Up,
            };
            let mut seen = HashSet::new();
            simulate(&map, start_beam, &mut HashSet::new(), &mut seen);
            if seen.len() > max {
                max = seen.len()
            }
        }

        max.to_string()
    }
}
