use std::{collections::VecDeque, error::Error};

use advent_of_code_lib::{self, Solver};

type Blizzard = ((isize, isize), Direction);

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2022", "24")
}

struct Day;

#[derive(Clone, Copy, Debug)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn position_offset(&self) -> (isize, isize) {
        match self {
            Direction::Up => (0, -1),
            Direction::Down => (0, 1),
            Direction::Left => (-1, 0),
            Direction::Right => (1, 0),
        }
    }

    fn warp(&self, max_x: usize, max_y: usize, (x, y): (isize, isize)) -> (isize, isize) {
        match self {
            Direction::Up => (x, max_y as isize - 1 - 1),
            Direction::Down => (x, 1),
            Direction::Left => (max_x as isize - 1 - 1, y),
            Direction::Right => (1, y),
        }
    }
}

// Could just store each blizzard as a bitset, ie: `010010` and then we could just rotate it/ find
// it based on an offset.

fn simulate(blizzards: &mut Vec<Blizzard>, max_x: usize, max_y: usize) {
    for (pos, direction) in blizzards {
        let offset = direction.position_offset();
        pos.0 += offset.0;
        pos.1 += offset.1;
        if pos.0 == 0 || pos.0 == (max_x - 1) as isize {
            *pos = direction.warp(max_x, max_y, *pos);
        }
        // println!("{} {}", pos.0, max_x);
        if pos.1 == 0 || pos.1 == (max_y - 1) as isize {
            *pos = direction.warp(max_x, max_y, *pos);
        }
    }
}

fn print(
    blizzards: &Vec<Blizzard>,
    head: (isize, isize),
    goal: (isize, isize),
    max_x: usize,
    max_y: usize,
) {
    for y in 0..max_y {
        for x in 0..max_x {
            if (x as isize, y as isize) == head {
                print!("E");
                continue;
            } else if (x as isize, y as isize) == goal {
                print!("G");
                continue;
            }
            if let Some(d) = blizzards.iter().find(|&f| f.0 == (x as isize, y as isize)) {
                print!(
                    "{}",
                    match d.1 {
                        Direction::Up => '^',
                        Direction::Down => 'v',
                        Direction::Right => '>',
                        Direction::Left => '<',
                    }
                )
            } else {
                print!(".")
            }
        }
        println!()
    }
}

// State includes
// - Time taken,
// - Position
fn turn_bfs(
    blizzards: &mut Vec<Blizzard>,
    max_x: usize,
    max_y: usize,
    head: (isize, isize),
    goal: (isize, isize),
) -> u32 {
    let mut current_time = 0;
    let mut max_time = u32::MAX;
    let mut queue = VecDeque::new();
    queue.push_back((0, head, vec![]));

    while let Some((time, pos, path)) = queue.pop_front() {
        if time > max_time {
            continue;
        }
        if pos == goal {
            if time < max_time {
                max_time = time;
            }
            // println!("Path found: in {}", current_time);
            for x in path.iter().enumerate() {
                // println!("{}: {:?}", x.0, x.1);
            }
        }

        if time != current_time {
            current_time += 1;
            simulate(blizzards, max_x, max_y);
        }
        for x in [(0, 1), (0, -1), (1, 0), (-1, 0), (0, 0)]
            .iter()
            .filter_map(|&(x, y)| {
                // if time == 10 {
                //     println!("{:?} {}", (pos.0 + x, pos.1 + y), blizzards.iter().any(|f| f.0 == (pos.0 + x, pos.1 + y)));
                //     print(blizzards, head, goal, max_x, max_y);
                //     println!("{:?}", blizzards);
                // }
                if !blizzards.iter().any(|f| f.0 == (pos.0 + x, pos.1 + y))
                    && (((pos.0 + x) > 0
                        && (pos.0 + x) < max_x as isize - 1
                        && (pos.1 + y) > 0
                        && (pos.1 + y) < max_y as isize - 1)
                        || (pos.0 + x, pos.1 + y) == head
                        || (pos.0 + x, pos.1 + y) == goal)
                {
                    Some((pos.0 + x, pos.1 + y))
                } else {
                    None
                }
            })
        {
            if !queue.iter().any(|&(t, z, _)| t == time + 1 && z == x) {
                let mut path = path.clone();
                path.push(x);
                queue.push_back((time + 1, x, path));
            }
        }
    }

    max_time
}

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        let mut blizzards = vec![];
        let max_y = input.lines().count();
        let max_x = input.lines().nth(0).unwrap().chars().count();
        for (y, line) in input.lines().enumerate() {
            for (x, char) in line.chars().enumerate() {
                let dir = match char {
                    '#' => continue,
                    '>' => Direction::Right,
                    '<' => Direction::Left,
                    '^' => Direction::Up,
                    'v' => Direction::Down,
                    _ => continue,
                };
                blizzards.push(((x as isize, y as isize), dir));
            }
        }

        for y in 0..=max_y {
            for x in 0..=max_x {
                if let Some(d) = blizzards.iter().find(|&f| f.0 == (x as isize, y as isize)) {
                    print!(
                        "{}",
                        match d.1 {
                            Direction::Up => '^',
                            Direction::Down => 'v',
                            Direction::Right => '>',
                            Direction::Left => '<',
                        }
                    )
                } else {
                    print!(".")
                }
            }
            println!()
        }
        let head = (1isize, 0isize);
        let goal = ((max_x - 2) as isize, (max_y - 1) as isize);

        let mut b_2 = blizzards.clone();

        println!("{:?}", goal);
        for x in 0..=19 {
            println!();
            println!("Turn {}", x);
            print(&b_2, head, goal, max_x, max_y);
            simulate(&mut b_2, max_x, max_y);
        }
        print(&blizzards, head, goal, max_x, max_y);
        (turn_bfs(&mut blizzards, max_x, max_y, head, goal) - 1).to_string()
    }

    fn part2(&self, input: &str) -> String {
        let mut blizzards = vec![];
        let max_y = input.lines().count();
        let max_x = input.lines().nth(0).unwrap().chars().count();
        for (y, line) in input.lines().enumerate() {
            for (x, char) in line.chars().enumerate() {
                let dir = match char {
                    '#' => continue,
                    '>' => Direction::Right,
                    '<' => Direction::Left,
                    '^' => Direction::Up,
                    'v' => Direction::Down,
                    _ => continue,
                };
                blizzards.push(((x as isize, y as isize), dir));
            }
        }

        for y in 0..=max_y {
            for x in 0..=max_x {
                if let Some(d) = blizzards.iter().find(|&f| f.0 == (x as isize, y as isize)) {
                    print!(
                        "{}",
                        match d.1 {
                            Direction::Up => '^',
                            Direction::Down => 'v',
                            Direction::Right => '>',
                            Direction::Left => '<',
                        }
                    )
                } else {
                    print!(".")
                }
            }
            println!()
        }
        let head = (1isize, 0isize);
        let goal = ((max_x - 2) as isize, (max_y - 1) as isize);

        let mut b_2 = blizzards.clone();

        println!("{:?}", goal);
        for x in 0..=19 {
            println!();
            println!("Turn {}", x);
            print(&b_2, head, goal, max_x, max_y);
            simulate(&mut b_2, max_x, max_y);
        }
        print(&blizzards, head, goal, max_x, max_y);
        println!("{}", (turn_bfs(&mut blizzards, max_x, max_y, head, goal) - 1).to_string());
        println!("{}", (turn_bfs(&mut blizzards, max_x, max_y, goal, head)).to_string());
        (turn_bfs(&mut blizzards, max_x, max_y, head, goal)).to_string()
    }
}

// #[test]
// fn test_file1() -> Result<(), Box<dyn Error>> {
//     advent_of_code_lib::test_file(Day, "test1", advent_of_code_lib::Part::Part1)
// }
