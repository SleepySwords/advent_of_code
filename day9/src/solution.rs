use std::collections::HashMap;
use utils;

pub struct Day;

impl Day {
    fn find_lowest_point(input: &Vec<Vec<u32>>, pos: (usize, usize)) -> (usize, usize) {
        let directions: Vec<Vec<i32>> = vec![vec![0, 1], vec![1, 0], vec![-1, 0], vec![0, -1]];
        let x = pos.0;
        let y = pos.1;
        let s = input[y][x];
        let is_lowest = directions.iter().all(|direction| {
            if let Some(line) = input.get((y as i32 + direction[1]) as usize) {
                if let Some(value) = line.get((x as i32 + direction[0]) as usize) {
                    return s < *value
                }
            }
            true
        });
        if is_lowest {
            return pos;
        } else {
            let direction = directions.iter().find(|direction| {
                if let Some(line) = input.get((y as i32 + direction[1]) as usize) {
                    if let Some(value) = line.get((x as i32 + direction[0]) as usize) {
                        return s > *value
                    }
                }
                false
            }).unwrap();
            return Self::find_lowest_point(input, ((pos.0 as i32 + direction[0]) as usize, (pos.1 as i32 + direction[1]) as usize))
        }
    }
}
impl utils::Solution for Day {
    fn part1(&self, input: &str) -> String {
        let input: Vec<Vec<u32>> = input
            .split("\n")
            .map(|x| {
                x.chars()
                    .map(|y| char::to_digit(y, 10).unwrap())
                    .collect::<Vec<u32>>()
            })
            .collect();

        let directions: Vec<Vec<i32>> = vec![vec![0, 1], vec![1, 0], vec![-1, 0], vec![0, -1]];

        input.iter().enumerate().map(|(y, v)| {
            v.iter()
                .enumerate()
                .filter(|(x, &s)| {
                    directions.iter().all(|direction| {
                        if let Some(line) = input.get((y as i32 + direction[1]) as usize) {
                            if let Some(value) = line.get((*x as i32 + direction[0]) as usize) {
                                return s < *value
                            }
                        }
                        true
                    })
                })
                .map(|(x, &s)| s + 1)
                .sum::<u32>()
        }).sum::<u32>().to_string()
    }

    fn part2(&self, input: &str) -> String {
        let input: Vec<Vec<u32>> = input
            .split("\n")
            .map(|x| {
                x.chars()
                    .map(|y| char::to_digit(y, 10).unwrap())
                    .collect::<Vec<u32>>()
            })
            .collect();

        let directions: Vec<Vec<i32>> = vec![vec![0, 1], vec![1, 0], vec![-1, 0], vec![0, -1]];

        let lowest_points = input.iter().enumerate().map(|(y, v)| {
            v.iter()
                .enumerate()
                .filter(|(x, &s)| {
                    directions.iter().all(|direction| {
                        if let Some(line) = input.get((y as i32 + direction[1]) as usize) {
                            if let Some(value) = line.get((*x as i32 + direction[0]) as usize) {
                                return s < *value
                            }
                        }
                        true
                    })
                })
                .map(|(x, &s)| (x, y))
                .collect::<Vec<(usize, usize)>>()
        }).flatten().collect::<Vec<(usize, usize)>>();

        let mut basin = HashMap::new();

        for x in lowest_points {
            basin.insert(x, 0);
        }

        for y in 0..input.len() {
            for x in 0..input[y].len() {
                if input[y][x] == 9 { continue; }
                let pos = Self::find_lowest_point(&input, (x, y));
                basin.entry(pos).and_modify(|x| *x += 1).or_insert(0);
            }
        }

        let mut value = basin.values().cloned().collect::<Vec<usize>>();
        value.sort_by(|a, b| b.cmp(a));
        (value[0] * value[1] * value[2]).to_string()
    }
}
