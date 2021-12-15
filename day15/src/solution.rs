use std::{collections::HashMap, collections::HashSet, collections::BTreeMap};
use utils;

pub struct Day;

impl Day {
    fn add_vertex(pos: (isize, isize), adjacent: (isize, isize)) -> (isize, isize) {
        (pos.0 + adjacent.0, pos.1 + adjacent.1)
    }
}

impl utils::Solution for Day {
    fn part1(&self, input: &str) -> String {
        let adjacents = vec![(0, 1), (1, 0), (0, -1), (-1, 0)];
        // let adjacents = vec![(0, 1), (1, 0), (0, -1), (-1, 0), (-1, -1), (1, 1), (1, -1), (-1, 1)];
        // Key: Coords, Value: Distance + Previous path
        let mut distance: BTreeMap<(isize, isize), u32> = BTreeMap::new();
        let mut queue = HashSet::new();

        let input: Vec<Vec<u32>> = input
            .split("\n")
            .map(|x| x.chars().map(|y| y.to_digit(10).unwrap()).collect())
            .collect();

        let start = (0, 0);
        let end = ((input[0].len() - 1) as isize, (input.len() - 1) as isize);
        for y in start.1..=end.1 {
            for x in start.0..=end.0 {
                if (x, y) == start {
                    distance.insert((x, y), 0);
                } else {
                    distance.insert((x, y), u32::MAX);
                }
                queue.insert((x, y));
            }
        }

        while !queue.is_empty() {
            println!("{}", queue.len());
            if queue.len() == 0 { break; }
            let current = distance
                .iter()
                .filter(|x| queue.contains(x.0))
                .min_by(|a, b| a.1.cmp(&b.1))
                .map(|(k, _v)| k).unwrap().clone();

            queue.remove(&current);

            for adjacent in &adjacents {
                let u = Self::add_vertex(current, *adjacent);
                if distance.iter().any(|f| f.0 == &u) {
                    let alt = distance[&current] + input[u.1 as usize][u.0 as usize];
                    if alt < distance[&u] {
                        distance.insert(u, alt);
                    }
                }
            }
        }

        distance[&end].to_string()
    }

    fn part2(&self, input: &str) -> String {
        let adjacents = vec![(0, 1), (1, 0), (0, -1), (-1, 0)];
        // let adjacents = vec![(0, 1), (1, 0), (0, -1), (-1, 0), (-1, -1), (1, 1), (1, -1), (-1, 1)];
        // Key: Coords, Value: Distance + Previous path
        let mut distance: HashMap<(isize, isize), u32> = HashMap::new();
        let mut min_distance: HashSet<(u32, (isize, isize))> = HashSet::new();
        let mut queue = HashSet::new();

        let mut input: Vec<Vec<u32>> = input
            .split("\n")
            .map(|x| x.chars().map(|y| y.to_digit(10).unwrap()).collect())
            .collect();

        let mut i: Vec<Vec<u32>> = vec![vec![0; 5 * input[0].len()]; 5 * input.len()];

        for tile_y in 0..5 {
            for tile_x in 0..5 {
                for y in 0..input.len() {
                    for x in 0..input[0].len() {
                        i[((tile_y * input.len()) + y) as usize][((tile_x * input[0].len()) + x) as usize] = (((input[y][x] as i32 + tile_x as i32 + tile_y as i32 - 1) % 9) + 1) as u32;
                    }
                }
            }
        }

        let mut input = i;

        let start = (0, 0);
        let end = ((input[0].len() - 1) as isize, (input.len() - 1) as isize);
        for y in start.1..=end.1 {
            for x in start.0..=end.0 {
                if (x, y) == start {
                    distance.insert((x, y), 0);
                } else {
                    distance.insert((x, y), u32::MAX);
                }
                queue.insert((x, y));
            }
        }
        min_distance.insert((0, start));

        while !queue.is_empty() {
            println!("Left to do: {}", queue.len());
            if queue.len() == 0 { break; }
            let current = min_distance
                .iter()
                .min_by(|a, b| a.0.cmp(&b.0))
                .unwrap()
                .clone();

            let current_pos = current.1;
            let current_distance = current.0;

            queue.remove(&current_pos);
            min_distance.remove(&current);

            for adjacent in &adjacents {
                let u = Self::add_vertex(current_pos, *adjacent);
                if distance.iter().any(|f| f.0 == &u) {
                    let alt = current_distance + input[u.1 as usize][u.0 as usize];
                    if alt < distance[&u] {
                        distance.insert(u, alt);
                        min_distance.insert((alt, u));
                    }
                }
            }
        }
        distance[&end].to_string()
    }
}

