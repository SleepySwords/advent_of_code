use std::{collections::HashSet, error::Error};

use advent_of_code_lib::{self, Solver};

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2022", "9")
}

struct Day;

fn close_enough(head: &(isize, isize), tail: &(isize, isize)) -> bool {
    (0..=1).contains(&(head.0 - tail.0).abs()) && (0..=1).contains(&(head.1 - tail.1).abs())
}

// T.H.T
// 12345
//
// ..H..
// .....
// .T..

fn update_tail(head: (isize, isize), mut tail: (isize, isize)) -> (isize, isize) {
    if !close_enough(&head, &tail) {
        tail.0 += (head.0 - tail.0).signum();
        tail.1 += (head.1 - tail.1).signum();
    }
    tail
}

fn make_move(direction: &Direction, head: &mut (isize, isize), tail: &mut (isize, isize)) {
    let new_head = (head.0 + direction.x_offset(), head.1 + direction.y_offset());
    if !close_enough(&new_head, &tail) {
        *tail = *head;
    }
    *head = new_head;
}

#[derive(PartialEq, Eq, Debug)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn x_offset(&self) -> isize {
        match self {
            Direction::Up => 0,
            Direction::Down => 0,
            Direction::Left => -1,
            Direction::Right => 1,
        }
    }

    fn y_offset(&self) -> isize {
        match self {
            Direction::Up => 1,
            Direction::Down => -1,
            Direction::Left => 0,
            Direction::Right => 0,
        }
    }
}

fn print_grid(values: &Vec<(isize, isize)>) {
    for y in (0..10).rev() {
        for x in 0..10 {
            print!(
                "{}",
                values
                    .iter()
                    .position(|n| *n == (x, y))
                    .map(|f| f.to_string())
                    .unwrap_or(".".to_string())
            );
        }
        println!("");
    }
    println!("");
}

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        let mut pos = input
            .lines()
            .map(|f| {
                let direction = match f.chars().nth(0).unwrap() {
                    'R' => Direction::Right,
                    'U' => Direction::Up,
                    'D' => Direction::Down,
                    'L' => Direction::Left,
                    _ => unreachable!(),
                };
                (direction, f[2..].parse::<usize>().unwrap())
            })
            .fold((HashSet::new(), ((0, 0), (0, 0))), |mut acc, inst| {
                for _ in 0..inst.1 {
                    make_move(&inst.0, &mut acc.1 .0, &mut acc.1 .1);
                    acc.0.insert(acc.1 .1);
                }
                acc
            })
            .0;
        pos.insert((0, 0));
        pos.len().to_string()
    }

    fn part2(&self, input: &str) -> String {
        let mut pos = input
            .lines()
            .map(|f| {
                let direction = match f.chars().nth(0).unwrap() {
                    'R' => Direction::Right,
                    'U' => Direction::Up,
                    'D' => Direction::Down,
                    'L' => Direction::Left,
                    _ => unreachable!(),
                };
                (direction, f[2..].parse::<usize>().unwrap())
            })
            .fold((HashSet::new(), vec![(0, 0); 10]), |mut acc, inst| {
                let direction = inst.0;
                let mut positions = acc.1;
                for _ in 0..inst.1 {
                    let mut new_pos = vec![];
                    let mut head = (
                        positions[0].0 + direction.x_offset(),
                        positions[0].1 + direction.y_offset(),
                    );
                    new_pos.push(head);
                    for tail in positions.into_iter().skip(1) {
                        if !close_enough(&head, &tail) {
                            new_pos.push(update_tail(head, tail));
                            head = update_tail(head, tail);
                        } else {
                            new_pos.push(tail);
                            head = tail;
                        }
                    }
                    positions = new_pos;
                    if let Some(x) = positions.last() {
                        acc.0.insert(x.clone());
                    }
                }
                (acc.0, positions)
            })
            .0;
        pos.insert((0, 0));
        pos.len().to_string()
    }
}
