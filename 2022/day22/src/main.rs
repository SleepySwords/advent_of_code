use std::{collections::HashMap, error::Error, num};

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

    fn warp(&self, (x, y): (isize, isize), map: &HashMap<(isize, isize), Tile>) -> (isize, isize) {
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
}

#[derive(Debug)]
struct Cursor {
    position: (isize, isize),
    facing: Direction,
}

fn turn(map: &HashMap<(isize, isize), Tile>, cursor: &mut Cursor, steps: u32) {
    for _ in 0..steps {
        let new_pos = cursor.facing.offset(cursor.position);
        if let Some(tile) = map.get(&new_pos) {
            if *tile == Tile::Open {
                cursor.position = new_pos;
            } else {
                break;
            }
        } else {
            let new_pos = cursor.facing.warp(cursor.position, map);
            if *map.get(&new_pos).unwrap() == Tile::Open {
            cursor.position = new_pos;
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
                        acc.insert(((index + 1) as isize, (y_level + 1) as isize), Tile::Open);
                    }
                    if ch == '#' {
                        acc.insert(((index + 1) as isize, (y_level + 1) as isize), Tile::Wall);
                    }
                }
                acc
            });
        let y_level = map.keys().min_by(|a, b| a.1.cmp(&b.1)).unwrap().1;
        println!("{:?}", y_level);
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
        let max_x = map.iter().max_by(|a, b| a.0 .0.cmp(&b.0 .0)).unwrap().0.0;
        let max_y = map.iter().max_by(|a, b| a.0 .1.cmp(&b.0 .1)).unwrap().0.1;
        for y in 0..=max_y {
            for x in 0..=max_x {
                if cursor.position == (x, y) {
                    print!("x");
                    continue;
                }
                if let Some(x) = map.get(&(x, y)) {
                    match x {
                        Tile::Open => print!("."),
                        Tile::Wall => print!("#"),
                    }
                } else {
                    print!(" ");
                }
            }
            println!();
        }
        let mut number_builder = String::new();
        for a in instruction.chars() {
            if a == '\n' {
                continue;
            }
            if a.is_ascii_digit() {
                number_builder += &a.to_string();
            } else {
                println!("{}", number_builder);
                turn(&map, &mut cursor, number_builder.parse().unwrap());
                if a == 'L' {
                    cursor.facing = cursor.facing.counter_clockwise();
                } else {
                    cursor.facing = cursor.facing.clockwise();
                }
                number_builder = String::new();
                for y in 0..=max_y {
                    for x in 0..=max_x {
                        if cursor.position == (x, y) {
                            match cursor.facing {
                                Direction::Right => print!(">"),
                                Direction::Down => print!("v"),
                                Direction::Left => print!("<"),
                                Direction::Up => print!("^"),
                            }
                            continue;
                        }
                        if let Some(x) = map.get(&(x, y)) {
                            match x {
                                Tile::Open => print!("."),
                                Tile::Wall => print!("#"),
                            }
                        } else {
                            print!(" ");
                        }
                    }
                    println!();
                }
            }
        }
        if !number_builder.is_empty() {
            println!("{}", number_builder);
            turn(&map, &mut cursor, number_builder.parse().unwrap());
        }
        for y in 0..=max_y {
            for x in 0..=max_x {
                if cursor.position == (x, y) {
                    match cursor.facing {
                        Direction::Right => print!(">"),
                        Direction::Down => print!("v"),
                        Direction::Left => print!("<"),
                        Direction::Up => print!("^"),
                    }
                    continue;
                }
                if let Some(x) = map.get(&(x, y)) {
                    match x {
                        Tile::Open => print!("."),
                        Tile::Wall => print!("#"),
                    }
                } else {
                    print!(" ");
                }
            }
            println!();
        }
        println!("{:?}", cursor);
        // 23465 (Too low)
        //23314
        (cursor.position.1 * 1000 + cursor.position.0 * 4 + cursor.facing as isize).to_string()
    }

    // fn part2(&self, input: &str) -> String {
    // }
}

// #[test]
// fn test_file1() -> Result<(), Box<dyn Error>> {
//     advent_of_code_lib::test_file(Day, "test1", advent_of_code_lib::Part::Part1)
// }
