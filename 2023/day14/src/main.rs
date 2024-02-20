use std::error::Error;

use advent_of_code_lib::{self, Solver};
use itertools::Itertools;

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2023", "14")
}

struct Day;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Tile {
    Hard,
    Round,
    None,
}

impl Day {
    pub fn simulate_north(&self, map: &mut Vec<Vec<Tile>>) {
        let map_len = map[0].len();
        let mut heightmap = vec![0; map_len];
        for y in 0..map.len() {
            for x in 0..map_len {
                let tile = map[y][x];
                match tile {
                    Tile::Hard => {
                        heightmap[x] = y + 1;
                    }
                    Tile::Round => {
                        if heightmap[x] == y {
                            heightmap[x] = heightmap[x] + 1;
                            continue;
                        }
                        map[heightmap[x]][x] = Tile::Round;
                        heightmap[x] = heightmap[x] + 1;
                        map[y][x] = Tile::None;
                    }
                    Tile::None => {}
                }
            }
        }
    }

    pub fn simulate_west(&self, map: &mut Vec<Vec<Tile>>) {
        let map_len = map[0].len();
        let mut heightmap = vec![0; map.len()];
        for x in 0..map_len {
            for y in 0..map.len() {
                let tile = map[y][x];
                match tile {
                    Tile::Hard => {
                        heightmap[y] = x + 1;
                    }
                    Tile::Round => {
                        if heightmap[y] == x {
                            heightmap[y] += 1;
                            continue;
                        }
                        map[y][heightmap[y]] = Tile::Round;
                        heightmap[y] += 1;
                        map[y][x] = Tile::None;
                    }
                    Tile::None => {}
                }
            }
        }
    }

    pub fn simulate_south(&self, map: &mut Vec<Vec<Tile>>) {
        let map_len = map[0].len();
        let mut heightmap = vec![map.len() - 1; map_len];
        for y in (0..map.len()).rev() {
            for x in 0..map_len {
                let tile = map[y][x];
                match tile {
                    Tile::Hard => {
                        if y == 0 {
                            // No boulders above can actually go here.
                            continue;
                        }
                        heightmap[x] = y - 1;
                    }
                    Tile::Round => {
                        if heightmap[x] == y {
                            if heightmap[x] != 0 {
                                heightmap[x] -= 1;
                            }
                            continue;
                        }
                        map[heightmap[x]][x] = Tile::Round;
                        heightmap[x] -= 1;
                        map[y][x] = Tile::None;
                    }
                    Tile::None => {}
                }
            }
        }
    }

    pub fn simulate_east(&self, map: &mut Vec<Vec<Tile>>) {
        let map_len = map[0].len();
        let mut heightmap = vec![map_len - 1; map.len()];
        for x in (0..map_len).rev() {
            for y in 0..map.len() {
                let tile = map[y][x];
                match tile {
                    Tile::Hard => {
                        if x == 0 {
                            // No boulders above can actually go here.
                            continue;
                        }
                        heightmap[y] = x - 1;
                    }
                    Tile::Round => {
                        if heightmap[y] == x {
                            if heightmap[y] != 0 {
                                heightmap[y] -= 1;
                            }
                            continue;
                        }
                        map[y][heightmap[y]] = Tile::Round;
                        heightmap[y] -= 1;
                        map[y][x] = Tile::None;
                    }
                    Tile::None => {}
                }
            }
        }
    }

    pub fn print_map(&self, map: &Vec<Vec<Tile>>) {
        let map_len = map[0].len();
        for y in 0..map.len() {
            for x in 0..map_len {
                print!(
                    "{}",
                    match map[y][x] {
                        Tile::Hard => '#',
                        Tile::Round => 'O',
                        Tile::None => '.',
                    }
                )
            }
            println!()
        }
        println!()
    }

    pub fn cycle(&self, map: &mut Vec<Vec<Tile>>) {
        self.simulate_north(map);
        self.simulate_west(map);
        self.simulate_south(map);
        self.simulate_east(map);
    }

    pub fn calculate_load(&self, map: &Vec<Vec<Tile>>) -> usize {
        let map_len = map[0].len();
        let mut total = 0;
        for y in 0..map.len() {
            for x in 0..map_len {
                if map[y][x] == Tile::Round {
                    total += map.len() - y;
                }
            }
        }
        total
    }

    pub fn parse(&self, input: &str) -> Vec<Vec<Tile>> {
        input
            .lines()
            .map(|line| {
                line.chars()
                    .map(|c| match c {
                        '#' => Tile::Hard,
                        'O' => Tile::Round,
                        '.' => Tile::None,
                        _ => panic!("Invalid input"),
                    })
                    .collect_vec()
            })
            .collect_vec()
    }
}

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        let mut map = self.parse(input);
        self.simulate_north(&mut map);

        return self.calculate_load(&map).to_string();
    }

    fn part2(&self, input: &str) -> String {
        let mut map = self.parse(input);

        let mut seen = Vec::new();

        for x in 0..1000000000 {
            self.cycle(&mut map);
            if seen.contains(&map) {
                let seen_index = seen.iter().position(|m| m == &map).expect("aoijef");
                // 0 Based indexing, hence we actually want to find the index of one lower
                let cycle_len = x as usize - seen_index;
                let cycle_index = (1000000000 - 1 - seen_index as usize) % cycle_len;
                let index = cycle_index + seen_index;
                let found_map: &Vec<Vec<Tile>> = &seen[index];

                let total = self.calculate_load(&found_map);
                return total.to_string();
            } else {
                seen.push(map.clone());
            }
        }

        return self.calculate_load(&mut map).to_string();
    }
}
