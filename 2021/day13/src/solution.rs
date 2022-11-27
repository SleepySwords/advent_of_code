use utils;

pub struct Day;

impl Day {
    fn fold_horizontal(paper: Vec<Vec<bool>>, fold: u32) -> Vec<Vec<bool>> {
        let len_x = paper[0].len();
        let mut new = vec![vec![false; len_x]; fold as usize];
        for y in 1..=fold {
            let top_y = fold - y;
            let bottom_y = fold + y;
            for x in 0..len_x {
                if let Some(write) = paper.get(bottom_y as usize) {
                    new[top_y as usize][x] = paper[top_y as usize][x] || write[x]
                }
            }
        }
        new
    }

    fn fold_vertical(paper: Vec<Vec<bool>>, fold: u32) -> Vec<Vec<bool>> {
        let len_y = paper.len();
        let mut new = vec![vec![false; fold as usize]; len_y];
        for x in 1..=fold {
            let left_x = fold - x;
            let right_x = fold + x;
            for y in 0..len_y {
                new[y][left_x as usize] =
                    paper[y][left_x as usize] || *paper[y].get(right_x as usize).unwrap_or(&false);
            }
        }
        new
    }

    fn print_paper(paper: &Vec<Vec<bool>>) {
        for y in 0..paper.len() {
            for x in 0..paper[y].len() {
                if paper[y][x] {
                    print!("#")
                } else {
                    print!(".")
                }
            }
            println!();
        }
        println!();
    }
}

impl utils::Solution for Day {
    fn part1(&self, input: &str) -> String {
        let max_x = input
            .split("\n\n")
            .nth(0)
            .unwrap()
            .split("\n")
            .map(|x| x.split(",").nth(0).unwrap().parse::<usize>().unwrap())
            .max()
            .unwrap();
        
        let max_y = input
            .split("\n\n")
            .nth(0)
            .unwrap()
            .split("\n")
            .map(|y| y.split(",").nth(1).unwrap().parse::<usize>().unwrap())
            .max()
            .unwrap();
       
        let mut paper = vec![vec![false; max_x + 1]; max_y + 1];

        for coords in input.split("\n\n").nth(0).unwrap().split("\n") {
            let x = coords.split(",").nth(0).unwrap().parse::<usize>().unwrap();
            let y = coords.split(",").nth(1).unwrap().parse::<usize>().unwrap();
            paper[y][x] = true;
        }
        
        let line = input
            .split("\n\n")
            .nth(1)
            .unwrap()
            .split("\n")
            .nth(0)
            .unwrap();
        
        if line.contains("y") {
            let y = line.replace("fold along y=", "").parse::<u32>().unwrap();
            paper = Self::fold_horizontal(paper, y);
        } else {
            let x = line.replace("fold along x=", "").parse::<u32>().unwrap();
            paper = Self::fold_vertical(paper, x);
        }
        
        paper.iter().flatten().filter(|&x| *x).count().to_string()
    }

    fn part2(&self, input: &str) -> String {
        let max_x = input
            .split("\n\n")
            .nth(0)
            .unwrap()
            .split("\n")
            .map(|x| x.split(",").nth(0).unwrap().parse::<usize>().unwrap())
            .max()
            .unwrap();

        let max_y = input
            .split("\n\n")
            .nth(0)
            .unwrap()
            .split("\n")
            .map(|y| y.split(",").nth(1).unwrap().parse::<usize>().unwrap())
            .max()
            .unwrap();

        let mut paper = vec![vec![false; max_x + 1]; max_y + 1];
        
        for coords in input.split("\n\n").nth(0).unwrap().split("\n") {
            let x = coords.split(",").nth(0).unwrap().parse::<usize>().unwrap();
            let y = coords.split(",").nth(1).unwrap().parse::<usize>().unwrap();
            paper[y][x] = true;
        }

        for line in input.split("\n\n").nth(1).unwrap().split("\n") {
            if line.contains("y") {
                let y = line.replace("fold along y=", "").parse::<u32>().unwrap();
                paper = Self::fold_horizontal(paper, y);
            } else {
                let x = line.replace("fold along x=", "").parse::<u32>().unwrap();
                paper = Self::fold_vertical(paper, x);
            }
        }
        println!();
        println!();
        println!();
        Self::print_paper(&paper);
        String::from("Above!!")
    }
}
