use std::cmp::{min, max};
use utils;

pub struct Day;

impl utils::Solution for Day {
    fn part1(&self, input: &str) -> String {
        let input = input.split("\n");
        let mut field = vec![vec![0; 1000]; 1000];
        for line in input {
            let coords: Vec<Vec<u32>> = line
                .split(" -> ")
                .map(|s| s.split(","))
                .map(|s| s.map(|x| x.parse::<u32>().unwrap()).collect())
                .collect();
            let coords1: &Vec<u32> = &coords[0];
            let coords2: &Vec<u32> = &coords[1];
            if coords1[0] == coords2[0] {
                for y in min(coords1[1], coords2[1])..=max(coords1[1], coords2[1]) {
                    field[y as usize][coords1[0] as usize] += 1;
                }
            } else if coords1[1] == coords2[1] {
                for x in min(coords1[0], coords2[0])..=max(coords1[0], coords2[0]) {
                    field[coords1[1] as usize][x as usize] += 1;
                }
            }
        }
        field.into_iter().flatten().filter(|&x| x >= 2).count().to_string()
    }

    fn part2(&self, input: &str) -> String {
        let input = input.split("\n");
        let mut field = vec![vec![0; 1000]; 1000];
        for line in input {
            let coords: Vec<Vec<i32>> = line
                .split(" -> ")
                .map(|s| s.split(","))
                .map(|s| s.map(|x| x.parse::<i32>().unwrap()).collect())
                .collect();
            let coords1: &Vec<i32> = &coords[0];
            let coords2: &Vec<i32> = &coords[1];
            let x1 = coords1[0];
            let x2 = coords2[0];
            let y1 = coords1[1];
            let y2 = coords2[1];
            
            if x1 == x2 {
                for y in min(y1, y2)..=max(y1, y2) {
                    field[y as usize][coords1[0] as usize] += 1;
                }
            } else if y1 == y2 {
                for x in min(x1, x2)..=max(x1, x2) {
                    field[coords1[1] as usize][x as usize] += 1;
                }
            } else {
                let stating_y = if x1 < x2 { y1 } else { y2 };
                let gradient = (y1 - y2) / (x1 - x2);
                for x_offset in 0..=max(x1, x2) - min(x1, x2) {
                    field[(stating_y + (gradient * x_offset)) as usize][(x_offset + min(x1, x2)) as usize] += 1;
                }
            }
        }
        field.into_iter().flatten().filter(|&x| x >= 2).count().to_string()
    }
}
