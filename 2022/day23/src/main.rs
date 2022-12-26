use std::{
    collections::{HashMap, HashSet, VecDeque},
    error::Error,
    ops::Add,
};

use advent_of_code_lib::{self, Solver};
use itertools::Itertools;

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2022", "23")
}

struct Day;

#[derive(Clone, Copy)]
enum Direction {
    North,
    South,
    East,
    West,
    Stationary,
}

impl Direction {
    fn position_offset(&self) -> Position {
        match self {
            Direction::North => Position { x: 0, y: -1 },
            Direction::South => Position { x: 0, y: 1 },
            Direction::East => Position { x: 1, y: 0 },
            Direction::West => Position { x: -1, y: 0 },
            Direction::Stationary => Position { x: 0, y: 0 },
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Position {
    x: isize,
    y: isize,
}

impl Add<Position> for Position {
    type Output = Position;

    fn add(self, rhs: Position) -> Self::Output {
        Position {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

fn propose_direction(
    map: &HashSet<Position>,
    position: Position,
    order: &VecDeque<Direction>,
) -> Position {
    let a = [
        Position { y: -1, x: -1 },
        Position { y: -1, x: 0 },
        Position { y: -1, x: 1 },
        Position { y: 1, x: -1 },
        Position { y: 1, x: 0 },
        Position { y: 1, x: 1 },
        Position { x: 1, y: -1 },
        Position { x: 1, y: 0 },
        Position { x: 1, y: 1 },
        Position { x: -1, y: -1 },
        Position { x: -1, y: 0 },
        Position { x: -1, y: 1 },
    ];
    if !a.iter().any(|&f| map.contains(&(f + position))) {
        return position;
    }
    for i in 0..4 {
        let dir = order[i];
        let offsets = match dir {
            Direction::North => [
                Position { y: -1, x: -1 },
                Position { y: -1, x: 0 },
                Position { y: -1, x: 1 },
            ],
            Direction::South => [
                Position { y: 1, x: -1 },
                Position { y: 1, x: 0 },
                Position { y: 1, x: 1 },
            ],
            Direction::East => [
                Position { x: 1, y: -1 },
                Position { x: 1, y: 0 },
                Position { x: 1, y: 1 },
            ],
            Direction::West => [
                Position { x: -1, y: -1 },
                Position { x: -1, y: 0 },
                Position { x: -1, y: 1 },
            ],
            Direction::Stationary => unreachable!(),
        };
        if !offsets.iter().any(|&f| map.contains(&(f + position))) {
            return dir.position_offset() + position;
        }
    }
    position
}

fn first_half(
    map: &HashSet<Position>,
    order: &mut VecDeque<Direction>,
) -> HashMap<Position, Position> {
    let mut positions = HashMap::new();
    for pos in map {
        positions.insert(*pos, propose_direction(map, *pos, order));
    }
    let dir = order.pop_front().unwrap();
    order.push_back(dir);
    return positions;
}

fn print(map: &HashSet<Position>) {
    let min_y = map.iter().map(|f| f.y).min().unwrap();
    let max_y = map.iter().map(|f| f.y).max().unwrap();
    let min_x = map.iter().map(|f| f.x).min().unwrap();
    let max_x = map.iter().map(|f| f.x).max().unwrap();

    println!("{} {} {} {}", min_x, max_x, min_y, max_y);

    for y in -10..=10 {
        for x in -10..=10 {
            if map.contains(&Position { x, y }) {
                print!("#")
            } else {
                print!(".")
            }
        }
        println!()
    }
}

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        let default_order = vec![
            Direction::North,
            Direction::South,
            Direction::West,
            Direction::East,
        ];
        let mut positions = input
            .lines()
            .enumerate()
            .flat_map(|(y, f)| {
                f.chars().enumerate().map(move |(x, c)| {
                    (
                        Position {
                            x: x as isize,
                            y: y as isize,
                        },
                        c,
                    )
                })
            })
            .filter(|(_, c)| *c == '#')
            .map(|(pos, _)| pos)
            .collect::<HashSet<Position>>();
        // let mut order = HashMap::new();
        // for p in &positions {
        //     order.insert(*p, default_order.clone());
        // }
        print(&positions);
        let mut order = default_order.into_iter().collect::<VecDeque<Direction>>();
        for _ in 0..10 {
            let n = first_half(&positions, &mut order);
            positions = n
                .iter()
                .map(|f| {
                    if n.values().filter(|&a| a == f.1).count() == 1 {
                        // let o = order.remove(f.0).unwrap();
                        // order.insert(*f.1, o);
                        *f.1
                    } else {
                        *f.0
                    }
                })
                .collect::<HashSet<Position>>();
            print(&positions);
        }

        let min_y = positions.iter().map(|f| f.y).min().unwrap();
        let max_y = positions.iter().map(|f| f.y).max().unwrap();
        let min_x = positions.iter().map(|f| f.x).min().unwrap();
        let max_x = positions.iter().map(|f| f.x).max().unwrap();
        ((((max_x - min_x) + 1) * ((max_y - min_y) + 1)) as usize - positions.len() ).to_string()
    }

    // fn part2(&self, input: &str) -> String {
    // }
}

// #[test]
// fn test_file1() -> Result<(), Box<dyn Error>> {
//     advent_of_code_lib::test_file(Day, "test1", advent_of_code_lib::Part::Part1)
// }
