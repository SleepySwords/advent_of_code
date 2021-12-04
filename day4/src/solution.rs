use regex::Regex;
use std::str::Split;
use utils;

pub struct Day;

impl utils::Solution for Day {
    fn part1(&self, input: &str) -> String {
        let mut input = input.split("\n\n");
        let numbers: Vec<u32> = input.next().unwrap().split(",").map(|x| u32::from_str_radix(x, 10).unwrap()).collect();
        let boards = get_boards(input);
        
        let mut called_numbers: Vec<u32> = Vec::new();

        for num in numbers {
            called_numbers.push(num);
            for board in &boards {
                if let Some(x) = board.has_bingo(&called_numbers) {
                    return (x * num).to_string();
                }
            }
        }

        String::from("Not found!")
    }

    fn part2(&self, input: &str) -> String {
        let mut input = input.split("\n\n");
        let mut numbers = input.next().unwrap().split(",").map(|x| u32::from_str_radix(x, 10).unwrap());
        let mut boards = get_boards(input);
        let mut called_numbers: Vec<u32> = Vec::new();
        loop {
            let num = numbers.next();
            if let None = num {
                break;
            }
            let num = num.unwrap();

            called_numbers.push(num);
            boards.retain(|board| {
                if let Some(_) = board.has_bingo(&called_numbers) {
                    return false
                }
                return true
            });
            if boards.len() == 1 {
                loop {
                    let num = numbers.next();
                    if let None = num {
                        break;
                    }
                    let num = num.unwrap();
                    called_numbers.push(num);

                    if let Some(x) = boards[0].has_bingo(&called_numbers) {
                        return (x * num).to_string()
                    }
                }
            }
        }

        String::from("Not implemented!")
    }
}

fn get_boards(input: Split<&str>) -> Vec<Board> {
    let mut boards: Vec<Board> = Vec::new();
    for board_data in input {
        let mut board = [[0u32; 5]; 5];
        let rows: Vec<&str> = board_data.split("\n").collect();
        for x in 0..5 {
            let row = rows[x];
            let r = Regex::new(r"(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)").unwrap();
            let captured = r.captures_iter(row);
            for capture in captured {
                for y in 0..5 {
                    board[x][y] = capture[y + 1].parse::<u32>().unwrap();
                }
            }
        }
        boards.push(Board { board });
    }
    return boards;
}

struct Board {
    board: [[u32; 5]; 5]
}

impl Clone for Board {
    fn clone(&self) -> Self {
        Board { board: self.board.clone() }
    }
}

impl Board {
    fn has_bingo(&self, called_numbers: &Vec<u32>) -> Option<u32> {
        // Checks rows
        'rows: for row in self.board {
            for value in row {
                if !called_numbers.contains(&value) {
                    continue 'rows;
                }
            }
            return Some(self.board.iter().flatten().filter(|&x| !called_numbers.contains(&x)).sum::<u32>());
        }

        // Checks columns
        'column: for column in 0..5 {
            for row in self.board {
                let value = row[column];
                if !called_numbers.contains(&value) {
                    continue 'column;
                }
            }
            return Some(self.board.iter().flatten().filter(|&x| !called_numbers.contains(&x)).sum::<u32>());
        }
        None
    }
}

