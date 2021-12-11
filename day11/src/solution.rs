use utils;

pub struct Day;

impl Day {
    fn flash(input: &mut Vec<Vec<u32>>, pos: (usize, usize), flash_total: &mut u32) {
        let adjacent: Vec<(i32, i32)> = vec![(1, 0), (0, 1), (-1, 0), (0, -1), (1, 1), (-1, -1), (-1, 1), (1, -1)];
        
        input[pos.1][pos.0] = 0;
        *flash_total += 1;

        let x = pos.0 as i32;
        let y = pos.1 as i32;
        for ele in adjacent {
            let adj_x = (x + ele.0) as usize;
            let adj_y = (y + ele.1) as usize;
            if !(0..input.len()).contains(&adj_x) { continue };
            if !(0..input.len()).contains(&adj_y) { continue };
            if input[adj_y][adj_x] == 0 { continue; }
            if input[adj_y][adj_x] == 9 {
                Self::flash(input, (adj_x, adj_y), flash_total)
            } else {
                input[adj_y][adj_x] += 1;
            }
        }
    }
}

impl utils::Solution for Day {
    fn part1(&self, input: &str) -> String {
        let mut input: Vec<Vec<u32>> = input
            .split("\n")
            .map(|x| x.chars().map(|y| y.to_digit(10).unwrap()).collect())
            .collect();

        let flash_total: &mut u32 = &mut 0;

        for t in 0..100 {
            let mut flashed: Vec<(usize, usize)> = Vec::new();
            for y in 0..input.len() {
                for x in 0..input.len() {
                    if input[y][x] == 9 {
                        flashed.push((x, y));
                        input[y][x] = 0;
                    } else {
                        input[y][x] += 1;
                    }
                }
            }
            for flash in flashed {
                Self::flash(&mut input, flash, flash_total);
            }
            // for y in 0..input.len() {
            //     for x in 0..input.len() {
            //         print!("{}", input[y][x]);
            //     }
            //     println!();
            // }
            // println!("Flashed: {}", *flash_total);
            // println!();
        }
        flash_total.to_string()
    }

    fn part2(&self, input: &str) -> String {
        let mut input: Vec<Vec<u32>> = input
            .split("\n")
            .map(|x| x.chars().map(|y| y.to_digit(10).unwrap()).collect())
            .collect();

        let flash_total: &mut u32 = &mut 0;

        for t in 0..10000 {
            let mut flashed: Vec<(usize, usize)> = Vec::new();
            for y in 0..input.len() {
                for x in 0..input.len() {
                    if input[y][x] == 9 {
                        flashed.push((x, y));
                        input[y][x] = 0;
                    } else {
                        input[y][x] += 1;
                    }
                }
            }
            for flash in flashed {
                Self::flash(&mut input, flash, flash_total);
            }
            if input.iter().flatten().filter(|&x| x != &0).count() == 0 {
                return (t + 1).to_string();
            }
        }
        unreachable!();
    }
}

