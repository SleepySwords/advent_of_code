use std::collections::HashMap;
use utils;

pub struct Day;

impl utils::Solution for Day {
    fn part1(&self, input: &str) -> String {
        let mut iter = input.split("\n");
        let adjacents = vec![(-1, -1), (0, -1), (1, -1), (-1, 0), (0, 0), (1, 0), (-1, 1), (0, 1), (1, 1)];
        let mut image = HashMap::new();
        let img_enhancement_algorithm: Vec<u32> = iter.next().unwrap().chars().map(|x| if x == '#' { 1 } else { 0 }).collect();
        iter.next();
        let input = iter.collect::<Vec<&str>>();
        let image_y_len = input.len() as isize;
        let image_x_len = input[0].len() as isize;
        for y in 0..input.len() {
            for x in 0..input[y].len() {
                image.insert((x as isize, y as isize), if input[y].chars().nth(x).unwrap() == '#' { 1 } else { 0 });
            }
        }
        let mut back_image = image.clone();

        let mut defaults = 0;

        for it in 1..=2 {
            let it = it as isize;
            for y in -it..image_y_len + it {
                for x in -it..image_x_len + it {

                    let index = adjacents.iter()
                        .map(|adj| *back_image.get(&(adj.0 + x, adj.1 + y)).unwrap_or(&defaults))
                        .fold(String::new(), |mut acc, f| {
                            acc.push(char::from_digit(f, 10).unwrap());
                            acc
                        });
                    image.insert((x, y), img_enhancement_algorithm[usize::from_str_radix(&index, 2).unwrap()]);

                    // print!("{}", if img_enhancement_algorithm[usize::from_str_radix(&index, 2).unwrap()] == 1 { '#' } else { '.' });
                }
                // println!("");
            }
            // println!("");
            let b_img = back_image;
            back_image = image;
            image = b_img;
            defaults = img_enhancement_algorithm[usize::from_str_radix(&defaults.to_string().repeat(9), 2).unwrap()];
        }
        back_image.iter().filter(|&x| *x.1 == 1).count().to_string()
    }

    fn part2(&self, input: &str) -> String {
        let mut iter = input.split("\n");
        let adjacents = vec![(-1, -1), (0, -1), (1, -1), (-1, 0), (0, 0), (1, 0), (-1, 1), (0, 1), (1, 1)];
        let mut image = HashMap::new();
        let img_enhancement_algorithm: Vec<u32> = iter.next().unwrap().chars().map(|x| if x == '#' { 1 } else { 0 }).collect();
        iter.next();
        let input = iter.collect::<Vec<&str>>();
        let image_y_len = input.len() as isize;
        let image_x_len = input[0].len() as isize;
        for y in 0..input.len() {
            for x in 0..input[y].len() {
                image.insert((x as isize, y as isize), if input[y].chars().nth(x).unwrap() == '#' { 1 } else { 0 });
            }
        }
        let mut back_image = image.clone();

        let mut defaults = 0;

        for it in 1..=50 {
            let it = it as isize;
            for y in -it..image_y_len + it {
                for x in -it..image_x_len + it {

                    let index = adjacents.iter()
                        .map(|adj| *back_image.get(&(adj.0 + x, adj.1 + y)).unwrap_or(&defaults))
                        .fold(String::new(), |mut acc, f| {
                            acc.push(char::from_digit(f, 10).unwrap());
                            acc
                        });
                    image.insert((x, y), img_enhancement_algorithm[usize::from_str_radix(&index, 2).unwrap()]);

                    // print!("{}", if img_enhancement_algorithm[usize::from_str_radix(&index, 2).unwrap()] == 1 { '#' } else { '.' });
                }
                // println!("");
            }
            // println!("");
            let b_img = back_image;
            back_image = image;
            image = b_img;
            // Laughs, i can't believe they didn't mention that the 0th index of the alg can be a 1
            defaults = img_enhancement_algorithm[usize::from_str_radix(&defaults.to_string().repeat(9), 2).unwrap()];
        }
        back_image.iter().filter(|&x| *x.1 == 1).count().to_string()
    }
}

