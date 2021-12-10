use std::str::Chars;
use std::iter::Peekable;

use utils;

pub struct Day;

impl Day {
    fn corrupt(chars: &mut Peekable<Chars>) -> Option<char> {
        let openings = vec!['{', '(', '[', '<'];
        let opening = chars.next().unwrap();
        let mut next = chars.peek();
        if let None = next {
            return None;
        }
        while openings.contains(next.unwrap()) {
            let char = Self::corrupt(chars);
            if let Some(t) = char {
                return Some(t);
            }
            next = chars.peek();
            if let None = next {
                return None;
            }
        }
        let closing = chars.next();
        return match closing {
            Some(close) => {
                if !((opening == '[' && close == ']' ) || (opening == '(' && close == ')') || (opening == '{' && close == '}') || (opening == '<' && close == '>')) {
                    Some(close)
                } else {
                    None
                }
            },
            None => None,
        }
    }

    fn get_closing(opening: char) -> char {
        match opening {
            '{' => '}',
            '(' => ')',
            '[' => ']',
            '<' => '>',
            _ => '.'
        }
    }

    fn auto_correct(chars: &mut Peekable<Chars>) -> Option<String> {
        let openings = vec!['{', '(', '[', '<'];
        let opening = chars.next().unwrap();
        let mut next = chars.peek();
        if let None = next {
            return Some(Self::get_closing(opening).to_string());
        }
        while openings.contains(next.unwrap()) {
            let char = Self::auto_correct(chars);
            if let Some(t) = char {
                let mut str = Self::get_closing(opening).to_string();
                str.insert_str(0, &t);
                return Some(str);
            }
            next = chars.peek();
            if let None = next {
                return Some(Self::get_closing(opening).to_string());
            }
        }
        let closing = chars.next();
        return match closing {
            Some(close) => {
                if !((opening == '[' && close == ']' ) || (opening == '(' && close == ')') || (opening == '{' && close == '}') || (opening == '<' && close == '>')) {
                    panic!("They don't match")
                } else {
                    None
                }
            },
            None => Some(Self::get_closing(opening).to_string()),
        }
    }
}

impl utils::Solution for Day {
    fn part1(&self, input: &str) -> String {
        input.split('\n').map(|x| {
            if let Some(char) = Self::corrupt(&mut x.chars().peekable()) {
                match char {
                    ')' => 3,
                    ']' => 57,
                    '}' => 1197,
                    '>' => 25137,
                    _ => 0
                }
            } else {
                0
            }
        }).sum::<u32>().to_string()
        // println!("{:?}", Self::next_char(&mut input.chars().peekable()));
        // String::from("Not implemented!")
    }

    fn part2(&self, input: &str) -> String {
        let mut data: Vec<u64> = input.split('\n').map(|x| {
            if let None = Self::corrupt(&mut x.chars().peekable()) {
                let mut total = 0;
                for c in Self::auto_correct(&mut x.chars().peekable()).unwrap().chars() {
                    total *= 5;
                    total += match c {
                        ')' => 1,
                        ']' => 2,
                        '}' => 3,
                        '>' => 4,
                        _ => 0
                    }
                }
                total
            } else {
                0
            }
        }).filter(|&x| x != 0).collect();
        data.sort();
        data[data.len() / 2].to_string()
    }
}

