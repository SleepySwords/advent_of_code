use std::{collections::HashMap, error::Error, usize};

use advent_of_code_lib::{self, Solver};
use itertools::Itertools;

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2023", "17")
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct Beam {
    x: isize,
    y: isize,
    prev_direction: Direction,
    travel: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct State {
    beam: Beam,
    heat_loss: usize,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn reverse(self) -> Self {
        match self {
            Direction::Up => Direction::Down,
            Direction::Down => Direction::Up,
            Direction::Left => Direction::Right,
            Direction::Right => Direction::Left,
        }
    }
}

impl Beam {
    fn move_pos(self) -> Self {
        let (x, y) = (self.x, self.y);
        let (new_x, new_y) = match self.prev_direction {
            Direction::Up => (x, y - 1),
            Direction::Down => (x, y + 1),
            Direction::Left => (x - 1, y),
            Direction::Right => (x + 1, y),
        };
        return Beam {
            x: new_x,
            y: new_y,
            travel: self.travel,
            prev_direction: self.prev_direction,
        };
    }
}

// Use djikista
fn simulate(map: &Vec<Vec<u8>>) {
    let mut states = Vec::new();
    let mut states_encounterd = HashMap::new();
    states.push(State {
        beam: Beam {
            x: 0,
            y: 0,
            prev_direction: Direction::Right,
            travel: 0,
        },
        heat_loss: 0,
    });
    let mut max = usize::MAX;
    while let Some(State { beam, heat_loss }) = states.pop() {
        for new_dir in [
            Direction::Up,
            Direction::Down,
            Direction::Left,
            Direction::Right,
        ] {
            if beam.prev_direction.reverse() == new_dir {
                continue;
            }
            let mut new_beam = beam;
            if new_beam.prev_direction == new_dir {
                if new_beam.travel == 3 {
                    continue;
                }
                new_beam.travel += 1;
            } else {
                new_beam.travel = 1;
            }
            new_beam.prev_direction = new_dir;
            let new_beam = new_beam.move_pos();

            if new_beam.x < 0 || new_beam.x as usize >= map[0].len() {
                continue;
            }
            if new_beam.y < 0 || new_beam.y as usize >= map.len() {
                continue;
            }

            let new_heat_loss = heat_loss + map[new_beam.y as usize][new_beam.x as usize] as usize;

            if new_beam.x as usize == map[0].len() - 1 && new_beam.y as usize == map[0].len() - 1 {
                println!("{}", new_heat_loss);
                if max > new_heat_loss {
                    println!("{}", new_heat_loss);
                    max = new_heat_loss;
                }
            }
            if states_encounterd.contains_key(&new_beam) && states_encounterd[&new_beam] < new_heat_loss {
                continue;
            }
            states_encounterd.insert(new_beam, new_heat_loss);
            states.push(State {
                beam: new_beam,
                heat_loss: new_heat_loss,
            });
        }
    }
    println!("{}", max);
}

struct Day;

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        let map = input
            .lines()
            .map(|f| {
                f.chars()
                    .map(|ch| ch.to_string().parse::<u8>().expect("ok"))
                    .collect_vec()
            })
            .collect_vec();

        simulate(&map);

        0.to_string()
    }

    // fn part2(&self, input: &str) -> String {
    // }
}
