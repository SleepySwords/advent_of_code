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

fn scenic_score(trees: &Vec<Vec<u32>>, x: usize, y: usize) -> usize {
    if x == 0 || y == 0 || x == trees[y].len() - 1 || y == trees.len() - 1 {
        return 0;
    }
    let left = (0..x).rev().position(|f| trees[y][f] >= trees[y][x]).map_or(x, |f| f + 1);
    let top = (0..y).rev().position(|f| trees[f][x] >= trees[y][x]).map_or(y, |f| f + 1);
    let right = (x + 1..trees[y].len()).position(|f| trees[y][f] >= trees[y][x]).map_or(trees[y].len() - x - 1, |f| f + 1);
    let bottom = (y + 1..trees.len()).position(|f| trees[f][x] >= trees[y][x]).map_or(trees.len() - y - 1, |f| f + 1);
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
