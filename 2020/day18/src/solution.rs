use std::{fmt::Display, str::Chars};

use utils;

pub struct Day;

#[derive(Debug, Clone)]
enum Direction {
    LEFT,
    RIGHT
}

#[derive(Debug, Clone)]
enum SnailNumber {
    PAIR(Box<SnailNumber>, Box<SnailNumber>),
    LITERAL(u64)
}

// Adding only adds one more nest.
impl SnailNumber {

    fn to_string(&self) -> String {
        match self {
            SnailNumber::PAIR(l, r) => {
                let mut str = String::from("[");
                str.push_str(&l.to_string());
                str.push_str(", ");
                str.push_str(&r.to_string());
                str.push(']');
                str
            },
            SnailNumber::LITERAL(v) => v.to_string(),
        }
    }
   
    // Checks if the left is a pair and if it is return a Some + combine with the right value
    fn check_combine_left(left: &mut Self, right: &mut Self) -> Option<(u64, Direction, bool)> {
        if let Self::PAIR(exploded_left, exploded_right) = &*left {
            if let Self::LITERAL(exploded_right) = **exploded_right {
                if let Self::LITERAL(value) = *right {
                    *right = SnailNumber::LITERAL(value + exploded_right);
                } else if let Self::PAIR(right_left, right_right) = &*right {
                    if let Self::LITERAL(right_value) = **right_left {
                        *right = Self::PAIR(Box::new(SnailNumber::LITERAL(exploded_right + right_value)), right_right.clone());
                    }
                }
            }
            if let Self::LITERAL(exploded_left) = **exploded_left {
                *left = SnailNumber::LITERAL(0);
                return Some((exploded_left, Direction::LEFT, false))
            } else {
                // We know that the 4th nested values will always be Literals
                unreachable!();
            }
        } else {
            return None;
        }
    }

    // Checks if the right is a pair and if it is return a Some + combine with the left value
    fn check_combine_right(left: &mut Self, right: &mut Self) -> Option<(u64, Direction, bool)>{
        if let Self::PAIR(exploded_left, exploded_right) = &*right {
            if let Self::LITERAL(exploded_left) = **exploded_left {
                if let Self::LITERAL(value) = *left {
                        *left = SnailNumber::LITERAL(value + exploded_left);
                } else if let Self::PAIR(left_left, left_right) = &*left {
                    if let Self::LITERAL(left_value) = **left_right {
                        *left = Self::PAIR(Box::new(SnailNumber::LITERAL(exploded_left + left_value)), left_left.clone());
                    }
                }
            }
            if let Self::LITERAL(exploded_right) = **exploded_right {
                *right = SnailNumber::LITERAL(0);
                return Some((exploded_right, Direction::RIGHT, false))
            }
            // We know that the 4th nested values will always be Literals
            unreachable!()
        } else {
            return None;
        }
    }

    // The bool is for if it has been handled.
    fn explode(&mut self, nest: u32) -> Option<(u64, Direction, bool)> {
        // If the nest value is 3, the next value must be 4
        if let SnailNumber::PAIR(left, right) = self {
            let left = left.as_mut();
            let right = right.as_mut();
            if nest == 3 {
                let mut result = Self::check_combine_left(left, right);
                if let None = result {
                    result = Self::check_combine_right(left, right)
                }
                return result;
            } else {
                // If the check returns true, combine if possible
                if let Self::PAIR(..) = *left {
                    if let Some((explode_value, direction, handled)) = left.explode(nest + 1) {
                        return if !handled {
                            if let Direction::LEFT = direction {
                                Some((explode_value, direction, handled))
                            } else {
                                let mut value = right;
                                while let Self::PAIR(l, _) = value {
                                    value = l.as_mut();
                                }
                                if let Self::LITERAL(v) = value {
                                    *v += explode_value;
                                }
                                Some((explode_value, direction, true))
                            }
                        } else {
                            Some((explode_value, direction, handled))
                        }
                    }
                }
                if let Self::PAIR(..) = *right { 
                    if let Some((explode_value, direction, handled)) = right.explode(nest + 1) {
                        return if !handled {
                            if let Direction::LEFT = direction {
                                let mut value = left;
                                while let Self::PAIR(_, r) = value {
                                    value = r.as_mut();
                                }
                                if let Self::LITERAL(v) = value {
                                    *v += explode_value;
                                }
                                Some((explode_value, direction, true))
                            } else {
                                Some((explode_value, direction, handled))
                            }
                        } else {
                            Some((explode_value, direction, handled))
                        }
                    }
                }
            }
        }
        return None;
    }
    
    fn split(&mut self) -> bool {
        if let Self::PAIR(l, r) = self {
            let l = l.as_mut();
            let r = r.as_mut();
            if let Self::LITERAL(v) = *l {
                if v > 9 {
                    *l = Self::PAIR(Box::new(Self::LITERAL(v / 2)), Box::new(Self::LITERAL(v - (v / 2))));
                    return true;
                }
            }
            let l_split = l.split();
            if l_split {
                return l_split;
            }
            if let Self::LITERAL(v) = *r {
                if v > 9 {
                    *r = Self::PAIR(Box::new(Self::LITERAL(v / 2)), Box::new(Self::LITERAL(v - (v / 2))));
                    return true;
                }
            }
            let r_split = r.split();
            if r_split {
                return r_split;
            }
        }
        return false
    }
}

fn add_snail(left_value: SnailNumber, right_value: SnailNumber) -> SnailNumber {
    SnailNumber::PAIR(Box::new(left_value), Box::new(right_value))
}

fn parse_snail(mut input: Chars) -> (SnailNumber, Chars) {
    let c = input.next().unwrap();
    if c == '[' {
        let mut first = parse_snail(input);
        let _comma = first.1.next();
        let mut second = parse_snail(first.1);
        let _bracket = second.1.next();
        return (SnailNumber::PAIR(Box::new(first.0), Box::new(second.0)), second.1)
    } else if c.is_numeric() {
        return (SnailNumber::LITERAL(c.to_digit(10).unwrap().into()), input);
    } else {
        unreachable!();
    }
}

fn reduce(number: &mut SnailNumber) {
    loop {
        if let Some((_, _, _)) = number.explode(0) {
            continue;
        }
        if number.split() {
            continue;
        }
        if let Some((_, _, _)) = number.explode(0) {
            continue;
        }
        break;
    }
}

fn magnitude(number: &SnailNumber) -> u64 {
    if let SnailNumber::PAIR(l, r) = number {
        return 3 * magnitude(&l) + 2 * magnitude(&r);
    } else if let SnailNumber::LITERAL(v) = number {
        return *v;
    } else {
        unreachable!();
    }
}

impl utils::Solution for Day {
    fn part1(&self, input: &str) -> String {
        let result = input.split("\n").map(|x| parse_snail(x.chars()).0).reduce(|x, y| {
            let mut s = add_snail(x, y);
            reduce(&mut s);
            return s;
        }).unwrap();
        magnitude(&result).to_string()
    }

    fn part2(&self, input: &str) -> String {
        let snails = input.split("\n");
        let mut highest = 0;
        for x in snails {
            let snails2 = input.split("\n");
            for y in snails2 {
                if x == y { continue; }
                let mut result = add_snail(parse_snail(x.chars()).0, parse_snail(y.chars()).0);
                reduce(&mut result);
                if highest < magnitude(&result) {
                    highest = magnitude(&result)
                }
            }
        }
        highest.to_string()
    }
}

