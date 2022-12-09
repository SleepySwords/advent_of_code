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

fn update_tail(head: (isize, isize), tail: &mut (isize, isize)) {
    if !close_enough(&head, &tail) {
        tail.0 += (head.0 - tail.0).signum();
        tail.1 += (head.1 - tail.1).signum();
    }
}

fn update_head(direction: Direction, head: &mut (isize, isize)) {
    head.0 += direction.x_offset();
    head.1 += direction.y_offset();
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
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

fn parse_input(input: &str) -> impl Iterator<Item = Direction> + '_ {
    input
        .lines()
        .map(|f| {
            let direction = match f.chars().nth(0).unwrap() {
                'R' => Direction::Right,
                'U' => Direction::Up,
                'D' => Direction::Down,
                'L' => Direction::Left,
                _ => unreachable!(),
            };

            std::iter::repeat(direction).take(f[2..].parse::<usize>().unwrap())
        })
        .flatten()
}

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        let mut pos = parse_input(input)
            .fold((HashSet::new(), ((0, 0), (0, 0))), |mut acc, direction| {
                let (head, tail) = &mut acc.1;
                update_head(direction, head);
                update_tail(*head, tail);
                acc.0.insert(*tail);
                acc
            })
            .0;
        pos.insert((0, 0));
        pos.len().to_string()
    }

    fn part2(&self, input: &str) -> String {
        let mut pos = parse_input(input)
            .fold((HashSet::new(), vec![(0, 0); 10]), |mut acc, direction| {
                let mut positions = acc.1;
                // Update head
                update_head(direction, &mut positions[0]);

                // Update the tails
                let mut head = 0;
                for tail in 1..positions.len() {
                    update_tail(positions[head], &mut positions[tail]);
                    head = tail;
                }

                // Insert last tail into the HashSet
                if let Some(x) = positions.last() {
                    acc.0.insert(*x);
                }
                (acc.0, positions)
            })
            .0;
        pos.insert((0, 0));
        pos.len().to_string()
    }
}
