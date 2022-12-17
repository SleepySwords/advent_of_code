use std::{
    collections::{HashMap, HashSet},
    error::Error,
    iter::{Cycle, Enumerate, Peekable},
};

use advent_of_code_lib::{self, Solver};
use itertools::Itertools;

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2022", "17")
}

// OMG TETRIS!!

struct Day;

fn max_floor_height(floor: &[HashSet<usize>; 7]) -> usize {
    *floor
        .iter()
        .map(|f| f.iter().max().unwrap_or(&0))
        .max()
        .unwrap()
}

fn is_overlapping(
    block_x: isize,
    block_y: usize,
    block_width: isize,
    block: &[[bool; 4]; 4],
    floor: &mut [HashSet<usize>; 7],
) -> bool {
    if block_x < 0 || block_x + block_width > 7 || block_y == 0 {
        return true;
    }
    for y_offset in 0..block.len() {
        for x_offset in 0..block_width {
            if block[y_offset][x_offset as usize] {
                if floor[block_x as usize + x_offset as usize].contains(&(block_y + y_offset)) {
                    return true;
                }
            }
        }
    }
    false
}

fn simulate<T: Iterator<Item = (usize, isize)>>(
    jet_stream: &mut T,
    block: &[[bool; 4]; 4],
    block_width: isize,
    floor: &mut [HashSet<usize>; 7],
) {
    let mut block_x = 2;
    // Since we are counting from 0, the next height would have to be + 1 to get the correct
    // distance. ie: since we start with 0, the max of below is also 0, but it would need to be
    // 1.
    let mut block_y = 3 + floor
        .iter()
        .map(|f| f.iter().max().map(|f| f + 1).unwrap_or(0))
        .max()
        .unwrap();

    loop {
        let x_offset = jet_stream.next().unwrap().1;
        if !is_overlapping(block_x + x_offset, block_y, block_width, block, floor) {
            block_x += x_offset;
        }

        if is_overlapping(block_x, block_y - 1, block_width, block, floor) {
            break;
        }
        block_y -= 1;
    }

    for (y, level) in block.iter().enumerate() {
        for x in 0..level.len() {
            if level[x] {
                floor[x + block_x as usize].insert(y + block_y);
            }
        }
    }
}

fn extrapolate_and_solve(
    previous_i: usize,
    previous_height: usize,
    i: usize,
    height: usize,
    floor: &mut [HashSet<usize>; 7],
    mut jet_stream: Peekable<impl Iterator<Item = (usize, isize)>>,
    mut blocks: Cycle<Peekable<Enumerate<std::slice::Iter<'_, ([[bool; 4]; 4], isize)>>>>,
) -> usize {
    // println!("Height difference {:?}", height - previous_height);
    // println!("Index difference: {:?}", i - previous_i);
    // println!("Prev Index: {:?}", previous_i);
    // println!("Prev Height: {:?}", previous_height);

    let index_left_to_proccess = 1_000_000_000_000 - previous_i;

    let height_multiplier = index_left_to_proccess / (i - previous_i);
    let height = (height_multiplier * (height - previous_height)) + previous_height;

    // println!("Current Height: {:?}", height);
    // println!("Current Index: {:?}", height_multiplier * (i - previous_i));

    let before_catchup_height = max_floor_height(floor);

    for _ in 0..index_left_to_proccess - (height_multiplier * (i - previous_i)) {
        let block = blocks.next().unwrap();
        simulate(&mut jet_stream, &block.1 .0, block.1 .1, floor);
    }

    let after_catchup_height = max_floor_height(floor);

    // println!(
    //     "Difference in height: {:?}",
    //     after_catchup_height - before_catchup_height
    // );
    // println!(
    //     "Total height: {:?}",
    //     height + after_catchup_height - before_catchup_height
    // );

    return height + after_catchup_height - before_catchup_height;
}

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        let (blocks, mut jet_stream) = parse(input);

        let mut blocks = blocks.iter().cycle();

        let mut floor = [
            HashSet::new(),
            HashSet::new(),
            HashSet::new(),
            HashSet::new(),
            HashSet::new(),
            HashSet::new(),
            HashSet::new(),
        ];

        for _ in 0..2022 {
            let block = blocks.next().unwrap();
            simulate(&mut jet_stream, &block.0, block.1, &mut floor);
        }

        // for y in (0..=*floor
        //     .iter()
        //     .map(|f| f.iter().max().unwrap_or(&0))
        //     .max()
        //     .unwrap())
        //     .rev()
        // {
        //     for x in 0..7 {
        //         if floor[x].contains(&y) {
        //             print!("#");
        //         } else {
        //             print!(".")
        //         }
        //     }
        //     println!()
        // }
        (max_floor_height(&floor) + 1).to_string()
    }

    fn part2(&self, input: &str) -> String {
        let (blocks, mut jet_stream) = parse(input);

        let mut blocks = blocks.iter().enumerate().peekable().cycle();

        let mut floor = [
            HashSet::new(),
            HashSet::new(),
            HashSet::new(),
            HashSet::new(),
            HashSet::new(),
            HashSet::new(),
            HashSet::new(),
        ];

        // What is the minumum amount until it repeats?
        // What can cache?
        // - Relative floor positions
        // - Current block
        // - Place in input
        let mut repeated = HashMap::new();
        for i in 0..4044 {
            let (block_id, block) = blocks.next().unwrap();

            simulate(&mut jet_stream, &block.0, block.1, &mut floor);

            let input_index = jet_stream.peek().unwrap().0;
            let min = floor
                .iter()
                .map(|f| f.iter().max().unwrap_or(&0))
                .min()
                .unwrap();
            let re = floor
                .iter()
                .map(|f| f.iter().max().unwrap_or(&0) - min)
                .collect_vec();

            let key = (block_id, input_index, re);
            if repeated.contains_key(&key) {
                let max_floor = max_floor_height(&floor);
                let (prev_i, prev_floor) = repeated.get(&key).unwrap();
                return extrapolate_and_solve(
                    *prev_i,
                    *prev_floor,
                    i,
                    max_floor,
                    &mut floor,
                    jet_stream,
                    blocks,
                )
                .to_string();
            } else {
                let max_floor = max_floor_height(&floor);
                repeated.insert(key, (i, max_floor));
            }
        }

        "Increase the range to find repeated values".to_string()
    }
}

fn parse<'a>(
    input: &'a str,
) -> (
    Vec<([[bool; 4]; 4], isize)>,
    Peekable<impl Iterator<Item = (usize, isize)> + 'a>,
) {
    let blocks: Vec<([[bool; 4]; 4], isize)> = vec![
        (
            [
                [true, true, true, true],
                [false, false, false, false],
                [false, false, false, false],
                [false, false, false, false],
            ],
            4,
        ),
        (
            [
                [false, true, false, false],
                [true, true, true, false],
                [false, true, false, false],
                [false, false, false, false],
            ],
            3,
        ),
        (
            [
                [true, true, true, false],
                [false, false, true, false],
                [false, false, true, false],
                [false, false, false, false],
            ],
            3,
        ),
        (
            [
                [true, false, false, false],
                [true, false, false, false],
                [true, false, false, false],
                [true, false, false, false],
            ],
            1,
        ),
        (
            [
                [true, true, false, false],
                [true, true, false, false],
                [false, false, false, false],
                [false, false, false, false],
            ],
            2,
        ),
    ];

    let jet_stream = input
        .lines()
        .next()
        .unwrap()
        .chars()
        .map(|f| match f {
            '>' => 1isize,
            '<' => -1isize,
            _ => unreachable!(),
        })
        .enumerate()
        .cycle()
        .peekable();

    (blocks, jet_stream)
}
