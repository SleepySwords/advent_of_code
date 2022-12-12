use std::{error::Error, mem::swap, vec};

use advent_of_code_lib::{self, Solver};
use itertools::Itertools;

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2022", "12")
}

struct Day;

type Position = (usize, usize);

#[derive(Clone, Copy, PartialEq, Eq)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

const DIRECTIONS: [Direction; 4] = [
    Direction::Up,
    Direction::Down,
    Direction::Left,
    Direction::Right,
];

impl Direction {
    fn apply_direction(&self, position: Position) -> Position {
        match self {
            Direction::Up => (position.0, position.1 + 1),
            Direction::Down => (position.0, position.1 - 1),
            Direction::Left => (position.0 - 1, position.1),
            Direction::Right => (position.0 + 1, position.1),
        }
    }

    fn can_apply_direction(&self, width: usize, height: usize, position: Position) -> bool {
        match self {
            Direction::Up => position.1 < height - 1,
            Direction::Down => position.1 > 0,
            Direction::Left => position.0 > 0,
            Direction::Right => position.0 < width - 1,
        }
    }
}

fn character_at(grid: &Vec<Vec<char>>, position: Position) -> char {
    grid[position.1][position.0]
}

fn can_visit(mut a: char, mut b: char, reverse: bool) -> bool {
    if a == 'S' {
        a = 'a';
    }
    if a == 'E' {
        a = 'z';
    }
    if b == 'S' {
        b = 'a';
    }
    if b == 'E' {
        b = 'z';
    }
    if reverse {
        swap(&mut a, &mut b);
    }
    ('a'..='z').position(|x| x == a).unwrap() + 1 >= ('a'..='z').position(|x| x == b).unwrap()
}

fn is_neighbour(
    grid: &Vec<Vec<char>>,
    position: Position,
    direction: Direction,
    width: usize,
    height: usize,
    reverse: bool,
) -> bool {
    direction.can_apply_direction(width, height, position)
        && can_visit(
            character_at(grid, position),
            character_at(grid, direction.apply_direction(position)),
            reverse,
        )
}

fn neighbours(
    grid: &Vec<Vec<char>>,
    position: Position,
    reverse: bool,
) -> impl Iterator<Item = Position> + '_ {
    let height = grid.len();
    let width = grid[0].len();
    DIRECTIONS
        .iter()
        .filter(move |f| is_neighbour(grid, position, **f, width, height, reverse))
        .map(move |f| f.apply_direction(position))
}

fn min_tenetive(visited: &Vec<Vec<bool>>, tentative_distance: &Vec<Vec<u32>>) -> Option<Position> {
    let height = visited.len();
    let width = visited[0].len();
    let mut min_tenetive = u32::MAX;
    let mut position = None;
    for y in 0..height {
        for x in 0..width {
            if !visited[y][x] && tentative_distance[y][x] < min_tenetive {
                min_tenetive = tentative_distance[y][x];
                position = Some((x, y));
            }
        }
    }
    position
}

fn djiksta<'a>(grid: &Vec<Vec<char>>, start_position: Position, reverse: bool, condition: Box<dyn Fn(&Vec<Vec<bool>>) -> bool + 'a>) -> Vec<Vec<u32>> {
    let height = grid.len();
    let width = grid[0].len();
    let mut visited = vec![vec![false; width]; height];
    let mut tentative_distance = vec![vec![u32::MAX; width]; height];

    tentative_distance[start_position.1][start_position.0] = 0;
    let mut current_node = start_position;
    while condition(&visited) {
        for (neighbour_x, neighbour_y) in
            neighbours(grid, current_node, reverse).filter(|&(x, y)| !visited[y][x])
        {
            if tentative_distance[current_node.1][current_node.0] + 1
                < tentative_distance[neighbour_y][neighbour_x]
            {
                tentative_distance[neighbour_y][neighbour_x] =
                    tentative_distance[current_node.1][current_node.0] + 1;
            }
        }
        visited[current_node.1][current_node.0] = true;
        current_node = if let Some(x) = min_tenetive(&visited, &tentative_distance) {
            x
        } else {
            break;
        }
    }
    tentative_distance
}

fn part1(grid: &Vec<Vec<char>>, start_position: Position, end_position: Position) -> u32 {
    let end_x = end_position.0;
    let end_y = end_position.1;
    djiksta(grid, start_position, false, Box::new(|vvisited| !vvisited[end_y][end_x]))[end_position.1][end_position.0]
}

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        let grid = input.lines().map(|f| f.chars().collect_vec()).collect_vec();

        let start_y = grid.iter().position(|f| f.contains(&'S')).unwrap();
        let start_x = grid[start_y].iter().position(|f| f == &'S').unwrap();
        let end_y = grid.iter().position(|f| f.contains(&'E')).unwrap();
        let end_x = grid[end_y].iter().position(|f| f == &'E').unwrap();

        part1(&grid, (start_x, start_y), (end_x, end_y)).to_string()
    }

    fn part2(&self, input: &str) -> String {
        let grid = input.lines().map(|f| f.chars().collect_vec()).collect_vec();

        let end_y = grid.iter().position(|f| f.contains(&'E')).unwrap();
        let end_x = grid[end_y].iter().position(|f| f == &'E').unwrap();

        let distances = djiksta(&grid, (end_x, end_y), true, Box::new(|_| true));
        let mut min = u32::MAX;
        for y in 0..grid.len() {
            for x in 0..grid[y].len() {
                if grid[y][x] == 'a' || grid[y][x] == 'S' {
                    if distances[y][x] < min {
                        min = distances[y][x];
                    }
                }
            }
        }
        min.to_string()
    }
}
