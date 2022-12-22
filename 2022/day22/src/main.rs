// This is a non-generic solution, it only works on my input.
use std::{collections::HashMap, error::Error};

use advent_of_code_lib::{self, Solver};

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2022", "22")
}

struct Day;

#[derive(PartialEq, Eq)]
enum Tile {
    Open,
    Wall,
}

#[derive(Debug)]
enum Direction {
    Right = 0,
    Down = 1,
    Left = 2,
    Up = 3,
}

impl Direction {
    fn clockwise(&self) -> Direction {
        match self {
            Direction::Right => Direction::Down,
            Direction::Down => Direction::Left,
            Direction::Left => Direction::Up,
            Direction::Up => Direction::Right,
        }
    }

    fn counter_clockwise(&self) -> Direction {
        match self {
            Direction::Right => Direction::Up,
            Direction::Down => Direction::Right,
            Direction::Left => Direction::Down,
            Direction::Up => Direction::Left,
        }
    }

    fn offset(&self, (x, y): (isize, isize)) -> (isize, isize) {
        match self {
            Direction::Right => (x + 1, y),
            Direction::Down => (x, y + 1),
            Direction::Left => (x - 1, y),
            Direction::Up => (x, y - 1),
        }
    }

    fn warp(
        &self,
        (x, y): (isize, isize),
        map: &HashMap<(isize, isize), (Tile, usize)>,
    ) -> (isize, isize) {
        match self {
            Direction::Right => *map
                .keys()
                .filter(|(_, y_t)| *y_t == y)
                .min_by(|a, b| a.0.cmp(&b.0))
                .unwrap(),
            Direction::Down => *map
                .keys()
                .filter(|(x_t, _)| *x_t == x)
                .min_by(|a, b| a.1.cmp(&b.1))
                .unwrap(),
            Direction::Left => *map
                .keys()
                .filter(|(_, y_t)| *y_t == y)
                .max_by(|a, b| a.0.cmp(&b.0))
                .unwrap(),
            Direction::Up => *map
                .keys()
                .filter(|(x_t, _)| *x_t == x)
                .max_by(|a, b| a.1.cmp(&b.1))
                .unwrap(),
        }
    }

    fn warp_cube(
        &self,
        (x, y): (isize, isize),
        map: &HashMap<(isize, isize), (Tile, usize)>,
    ) -> ((isize, isize), Direction) {
        let region = map[&(x, y)].1;
        if region == 7 {
            if let Direction::Down = self {
                let min_9_y = map
                    .iter()
                    .filter(|f| f.1 .1 == 9)
                    .map(|f| f.0 .1)
                    .min()
                    .unwrap();
                let max_9_x = map
                    .iter()
                    .filter(|f| f.1 .1 == 9)
                    .map(|f| f.0 .0)
                    .max()
                    .unwrap();
                return ((max_9_x, min_9_y + (x % 50)), Direction::Left);
            }

            if let Direction::Right = self {
                let max_2_y = map
                    .iter()
                    .filter(|f| f.1 .1 == 2)
                    .map(|f| f.0 .1)
                    .max()
                    .unwrap();
                let max_2_x = map
                    .iter()
                    .filter(|f| f.1 .1 == 2)
                    .map(|f| f.0 .0)
                    .max()
                    .unwrap();
                return ((max_2_x, max_2_y - (y % 50)), Direction::Left);
            }
        }

        if region == 9 {
            if let Direction::Right = self {
                let max_7_y = map
                    .iter()
                    .filter(|f| f.1 .1 == 7)
                    .map(|f| f.0 .1)
                    .max()
                    .unwrap();
                let min_7_x = map
                    .iter()
                    .filter(|f| f.1 .1 == 7)
                    .map(|f| f.0 .0)
                    .min()
                    .unwrap();
                return ((min_7_x + (y % 50), max_7_y), Direction::Up);
            }

            if let Direction::Left = self {
                let min_1_y = map
                    .iter()
                    .filter(|f| f.1 .1 == 1)
                    .map(|f| f.0 .1)
                    .min()
                    .unwrap();
                let min_1_x = map
                    .iter()
                    .filter(|f| f.1 .1 == 1)
                    .map(|f| f.0 .0)
                    .min()
                    .unwrap();
                return ((min_1_x + (y % 50), min_1_y), Direction::Down);
            }

            if let Direction::Down = self {
                let min_2_y = map
                    .iter()
                    .filter(|f| f.1 .1 == 2)
                    .map(|f| f.0 .1)
                    .min()
                    .unwrap();
                let min_2_x = map
                    .iter()
                    .filter(|f| f.1 .1 == 2)
                    .map(|f| f.0 .0)
                    .min()
                    .unwrap();
                return ((min_2_x + (x % 50), min_2_y), Direction::Down);
            }
        }

        if region == 2 {
            if let Direction::Down = self {
                let min_4_y = map
                    .iter()
                    .filter(|f| f.1 .1 == 4)
                    .map(|f| f.0 .1)
                    .min()
                    .unwrap();
                let max_4_x = map
                    .iter()
                    .filter(|f| f.1 .1 == 4)
                    .map(|f| f.0 .0)
                    .max()
                    .unwrap();
                return ((max_4_x, min_4_y + (x % 50)), Direction::Left);
            }

            if let Direction::Up = self {
                let max_9_y = map
                    .iter()
                    .filter(|f| f.1 .1 == 9)
                    .map(|f| f.0 .1)
                    .max()
                    .unwrap();
                let min_9_x = map
                    .iter()
                    .filter(|f| f.1 .1 == 9)
                    .map(|f| f.0 .0)
                    .min()
                    .unwrap();
                return ((min_9_x + (x % 50), max_9_y), Direction::Up);
            }

            if let Direction::Right = self {
                let max_7_y = map
                    .iter()
                    .filter(|f| f.1 .1 == 7)
                    .map(|f| f.0 .1)
                    .max()
                    .unwrap();
                let max_7_x = map
                    .iter()
                    .filter(|f| f.1 .1 == 7)
                    .map(|f| f.0 .0)
                    .max()
                    .unwrap();
                return ((max_7_x, max_7_y - (y % 50)), Direction::Left);
            }
        }

        if region == 4 {
            if let Direction::Right = self {
                let max_2_y = map
                    .iter()
                    .filter(|f| f.1 .1 == 2)
                    .map(|f| f.0 .1)
                    .max()
                    .unwrap();
                let min_2_x = map
                    .iter()
                    .filter(|f| f.1 .1 == 2)
                    .map(|f| f.0 .0)
                    .min()
                    .unwrap();
                return ((min_2_x + (y % 50), max_2_y), Direction::Up);
            }

            if let Direction::Left = self {
                let min_6_y = map
                    .iter()
                    .filter(|f| f.1 .1 == 6)
                    .map(|f| f.0 .1)
                    .min()
                    .unwrap();
                let min_6_x = map
                    .iter()
                    .filter(|f| f.1 .1 == 6)
                    .map(|f| f.0 .0)
                    .min()
                    .unwrap();
                return ((min_6_x + (y % 50), min_6_y), Direction::Down);
            }
        }

        if region == 1 {
            if let Direction::Left = self {
                let max_6_y = map
                    .iter()
                    .filter(|f| f.1 .1 == 6)
                    .map(|f| f.0 .1)
                    .max()
                    .unwrap();
                let min_6_x = map
                    .iter()
                    .filter(|f| f.1 .1 == 6)
                    .map(|f| f.0 .0)
                    .min()
                    .unwrap();
                return ((min_6_x, max_6_y - (y % 50)), Direction::Right);
            }

            if let Direction::Up = self {
                let min_9_y = map
                    .iter()
                    .filter(|f| f.1 .1 == 9)
                    .map(|f| f.0 .1)
                    .min()
                    .unwrap();
                let min_9_x = map
                    .iter()
                    .filter(|f| f.1 .1 == 9)
                    .map(|f| f.0 .0)
                    .min()
                    .unwrap();
                return ((min_9_x, min_9_y + (x % 50)), Direction::Right);
            }
        }
        if region == 6 {
            if let Direction::Left = self {
                let max_1_y = map
                    .iter()
                    .filter(|f| f.1 .1 == 1)
                    .map(|f| f.0 .1)
                    .max()
                    .unwrap();
                let min_1_x = map
                    .iter()
                    .filter(|f| f.1 .1 == 1)
                    .map(|f| f.0 .0)
                    .min()
                    .unwrap();
                return ((min_1_x, max_1_y - (y % 50)), Direction::Right);
            }

            if let Direction::Up = self {
                let min_4_y = map
                    .iter()
                    .filter(|f| f.1 .1 == 4)
                    .map(|f| f.0 .1)
                    .min()
                    .unwrap();
                let min_4_x = map
                    .iter()
                    .filter(|f| f.1 .1 == 4)
                    .map(|f| f.0 .0)
                    .min()
                    .unwrap();
                return ((min_4_x, min_4_y + (x % 50)), Direction::Right);
            }
        }
        unreachable!()
    }
}

#[derive(Debug)]
struct Cursor {
    position: (isize, isize),
    facing: Direction,
}

fn turn(map: &HashMap<(isize, isize), (Tile, usize)>, cursor: &mut Cursor, steps: u32) {
    for _ in 0..steps {
        let new_pos = cursor.facing.offset(cursor.position);
        if let Some((tile, _)) = map.get(&new_pos) {
            if *tile == Tile::Open {
                cursor.position = new_pos;
            } else {
                break;
            }
        } else {
            let new_pos = cursor.facing.warp(cursor.position, map);
            if map.get(&new_pos).unwrap().0 == Tile::Open {
                cursor.position = new_pos;
            } else {
                break;
            }
        }
    }
}

fn turn_cube(map: &HashMap<(isize, isize), (Tile, usize)>, cursor: &mut Cursor, steps: u32) {
    for _ in 0..steps {
        let new_pos = cursor.facing.offset(cursor.position);
        if let Some((tile, _)) = map.get(&new_pos) {
            if *tile == Tile::Open {
                cursor.position = new_pos;
            } else {
                break;
            }
        } else {
            let new_pos = cursor.facing.warp_cube(cursor.position, map);
            if map.get(&new_pos.0).unwrap().0 == Tile::Open {
                cursor.position = new_pos.0;
                cursor.facing = new_pos.1;
            } else {
                break;
            }
        }
    }
}

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        let (map, instruction) = input.split_once("\n\n").unwrap();
        let map = map
            .lines()
            .enumerate()
            .fold(HashMap::new(), |mut acc, (y_level, ch)| {
                for (index, ch) in ch.chars().enumerate() {
                    if ch.is_whitespace() {
                        continue;
                    }
                    if ch == '.' {
                        acc.insert(
                            ((index) as isize, (y_level) as isize),
                            (Tile::Open, (index / 50) + (y_level / 50) * 3),
                        );
                    }
                    if ch == '#' {
                        acc.insert(
                            ((index) as isize, (y_level) as isize),
                            (Tile::Wall, (index / 50) + (y_level / 50) * 3),
                        );
                    }
                }
                acc
            });
        let y_level = map.keys().min_by(|a, b| a.1.cmp(&b.1)).unwrap().1;
        let x_level = map
            .keys()
            .filter(|f| f.1 == y_level)
            .min_by(|a, b| a.0.cmp(&b.0))
            .unwrap()
            .0;
        let mut cursor = Cursor {
            position: (x_level, y_level),
            facing: Direction::Right,
        };
        let max_x = map.iter().max_by(|a, b| a.0 .0.cmp(&b.0 .0)).unwrap().0 .0;
        let max_y = map.iter().max_by(|a, b| a.0 .1.cmp(&b.0 .1)).unwrap().0 .1;
        let mut number_builder = String::new();
        for a in instruction.lines().next().unwrap().chars() {
            if a == '\n' {
                continue;
            }
            if a.is_ascii_digit() {
                number_builder += &a.to_string();
            } else {
                turn(&map, &mut cursor, number_builder.parse().unwrap());
                if a == 'L' {
                    cursor.facing = cursor.facing.counter_clockwise();
                } else {
                    cursor.facing = cursor.facing.clockwise();
                }
                number_builder = String::new();
            }
        }
        if !number_builder.is_empty() {
            turn(&map, &mut cursor, number_builder.parse().unwrap());
        }
        ((cursor.position.1 + 1) * 1000 + (cursor.position.0 + 1) * 4 + cursor.facing as isize)
            .to_string()
    }

    fn part2(&self, input: &str) -> String {
        let (map, instruction) = input.split_once("\n\n").unwrap();
        let map = map
            .lines()
            .enumerate()
            .fold(HashMap::new(), |mut acc, (y_level, ch)| {
                for (index, ch) in ch.chars().enumerate() {
                    if ch.is_whitespace() {
                        continue;
                    }
                    if ch == '.' {
                        acc.insert(
                            ((index) as isize, (y_level) as isize),
                            (Tile::Open, (index / 50) + (y_level / 50) * 3),
                        );
                    }
                    if ch == '#' {
                        acc.insert(
                            ((index) as isize, (y_level) as isize),
                            (Tile::Wall, (index / 50) + (y_level / 50) * 3),
                        );
                    }
                }
                acc
            });
        let y_level = map.keys().min_by(|a, b| a.1.cmp(&b.1)).unwrap().1;
        let x_level = map
            .keys()
            .filter(|f| f.1 == y_level)
            .min_by(|a, b| a.0.cmp(&b.0))
            .unwrap()
            .0;
        let mut cursor = Cursor {
            position: (x_level, y_level),
            facing: Direction::Right,
        };
        let mut number_builder = String::new();
        for a in instruction.lines().next().unwrap().chars() {
            if a == '\n' {
                continue;
            }
            if a.is_ascii_digit() {
                number_builder += &a.to_string();
            } else {
                turn_cube(&map, &mut cursor, number_builder.parse().unwrap());
                if a == 'L' {
                    cursor.facing = cursor.facing.counter_clockwise();
                } else {
                    cursor.facing = cursor.facing.clockwise();
                }
                number_builder = String::new();
            }
        }
        if !number_builder.is_empty() {
            turn_cube(&map, &mut cursor, number_builder.parse().unwrap());
        }
        ((cursor.position.1 + 1) * 1000 + (cursor.position.0 + 1) * 4 + cursor.facing as isize)
            .to_string()
    }
}
