use std::iter::Peekable;
use std::str::Chars;

use utils;

pub struct Day;

impl Day {
    fn get_closing(opening: char) -> char {
        match opening {
            '{' => '}',
            '(' => ')',
            '[' => ']',
            '<' => '>',
            _ => '.',
        }
    }

    fn corrupt(input: &mut Peekable<Chars>) -> Option<char> {
        let opening_chars = vec!['{', '(', '[', '<'];
        let opening = input.next().unwrap();

        loop {
            match input.peek() {
                Some(next_char) => {
                    if !opening_chars.contains(next_char) { break; }
                    let corrupted = Self::corrupt(input);
                    if let Some(t) = corrupted {
                        return Some(t); 
                    }
                }
                None => {
                    return None;
                }
            };
        }

        let complement_char = input.next();
        return match complement_char {
            Some(complement) => {
                if Self::get_closing(opening) != complement {
                    Some(complement)
                } else {
                    None
                }
            }
            None => None,
        };
    }

    fn auto_correct(input: &mut Peekable<Chars>) -> Option<String> {
        let opening_chars = vec!['{', '(', '[', '<'];
        let current_char = input.next().unwrap();

        loop {
            match input.peek() {
                Some(next_char) => {
                    if !opening_chars.contains(next_char) { break; }
                    let auto_correct = Self::auto_correct(input);
                    if let Some(t) = auto_correct {
                        let mut correction = Self::get_closing(current_char).to_string();
                        correction.insert_str(0, &t);
                        return Some(correction);
                    }
                }
                None => {
                    return Some(Self::get_closing(current_char).to_string());
                }
            };
        }

        let complement_char = input.next();
        return match complement_char {
            Some(complement_char) => {
                if Self::get_closing(current_char) != complement_char {
                    panic!("They don't match")
                } else {
                    None
                }
            }
            None => Some(Self::get_closing(current_char).to_string()),
        };
    }
}

impl utils::Solution for Day {
    fn part1(&self, input: &str) -> String {
        input
            .split('\n')
            .map(|x| {
                if let Some(char) = Self::corrupt(&mut x.chars().peekable()) {
                    match char {
                        ')' => 3,
                        ']' => 57,
                        '}' => 1197,
                        '>' => 25137,
                        _ => 0,
                    }
                } else {
                    0
                }
            })
            .sum::<u32>()
            .to_string()
    }

    fn part2(&self, input: &str) -> String {
        let mut data: Vec<u64> = input
            .split('\n')
            .map(|x| {
                if let None = Self::corrupt(&mut x.chars().peekable()) {
                    let mut total = 0;
                    for c in Self::auto_correct(&mut x.chars().peekable())
                        .unwrap()
                        .chars()
                    {
                        total *= 5;
                        total += match c {
                            ')' => 1,
                            ']' => 2,
                            '}' => 3,
                            '>' => 4,
                            _ => 0,
                        }
                    }
                    total
                } else {
                    0
                }
            })
            .filter(|&x| x != 0)
            .collect();
        data.sort();
        data[data.len() / 2].to_string()
    }
}
