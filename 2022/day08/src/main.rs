use std::error::Error;

use advent_of_code_lib::{self, Solver};
use itertools::Itertools;

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2022", "8")
}

struct Day;

fn validate_tree(trees: &Vec<Vec<u32>>, x: usize, y: usize) -> bool {
    if x == 0 || y == 0 || x == trees[y].len() - 1 || y == trees.len() - 1 {
        return true;
    }
    let height = trees[y][x];
    let left = (0..x).all(|f| trees[y][f] < height);
    let top = (0..y).all(|f| trees[f][x] < height);
    let right = ((x + 1)..trees[y].len()).all(|f| trees[y][f] < height);
    let bottom = ((y + 1)..trees.len()).all(|f| trees[f][x] < height);

    return left || top || right || bottom;
}

fn count_tree_x<T: Iterator<Item = usize>>(trees: &Vec<Vec<u32>>, range: T, x: usize, y: usize) -> usize {
    let mut counter = 0;
    for f in range {
        if trees[y][f] >= trees[y][x] {
            counter += 1;
            break;
        }
        counter += 1;
    }
    counter
}

fn count_tree_y<T: Iterator<Item = usize>>(trees: &Vec<Vec<u32>>, range: T, x: usize, y: usize) -> usize {
    let mut counter = 0;
    for f in range {
        if trees[f][x] >= trees[y][x] {
            counter += 1;
            break;
        }
        counter += 1;
    }
    counter
}

fn scenic_score(trees: &Vec<Vec<u32>>, x: usize, y: usize) -> usize {
    if x == 0 || y == 0 || x == trees[y].len() - 1 || y == trees.len() - 1 {
        return 0;
    }
    let left = count_tree_x(trees, (0..x).rev(), x, y);
    let top = count_tree_y(trees, (0..y).rev(), x, y);
    let right = count_tree_x(trees, x + 1..trees[y].len(), x, y);
    let bottom = count_tree_y(trees, y + 1..trees.len(), x, y);
    left * bottom * top * right
}

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        let trees = input
            .lines()
            .map(|f| {
                f.chars()
                    .into_iter()
                    .map(|d| d.to_digit(10).unwrap())
                    .collect_vec()
            })
            .collect_vec();
        let mut count = 0;
        for y in 0..trees.len() {
            for x in 0..trees[y].len() {
                if validate_tree(&trees, x, y) {
                    count += 1;
                }
            }
        }
        count.to_string()
    }

    fn part2(&self, input: &str) -> String {
        let trees = input
            .lines()
            .map(|f| {
                f.chars()
                    .into_iter()
                    .map(|d| d.to_digit(10).unwrap())
                    .collect_vec()
            })
            .collect_vec();
        // scenic_score(&trees, 2, 1);
        let mut count = 0;
        for y in 0..trees.len() {
            for x in 0..trees[y].len() {
                if count < scenic_score(&trees, x, y) {
                    count = scenic_score(&trees, x, y);
                }
            }
        }

        count.to_string()
    }
}

// #[test]
// fn test_file1() -> Result<(), Box<dyn Error>> {
//     advent_of_code_lib::test_file(Day, "test1", advent_of_code_lib::Part::Part1)
// }
