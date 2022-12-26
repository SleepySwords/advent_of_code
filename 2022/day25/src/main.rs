use std::error::Error;

use advent_of_code_lib::{self, Solver};

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2022", "25")
}

struct Day;

// Base conversion

fn convert_snafu_to_dec(number: &str) -> i64 {
    let mut dec_number = 0;
    for (i, ch) in number.chars().rev().enumerate() {
        let num = match ch {
            '1' => 1,
            '2' => 2,
            '0' => 0,
            '-' => -1,
            '=' => -2,
            _ => unreachable!(),
        };
        dec_number += num * 5i64.pow(i as u32);
    }
    dec_number
}

// 12345
// 12345 / 5 = 2459
// 12345 % 5 = 0
//
// n = 0
// 
// 2469 / 5 = 493
// 2459 % 5 = 4
// c = 1-
//
// n = -0
// c = 1
//
// 493 / 5 = 98
// 493 % 5 = 3
// 3 + c = 3 + 1 = 4
// c = 1-
//
// n = --0
// c = 1
//
// 98 / 5 = 19
// 98 % 5 = 3
// 3 + c = 3 + 1 = 4
// c = 1-
//
// n = ---0
// c = 1
//
// 19 / 5 = 3
// 19 % 5 = 4
// 4 + c = 4 + 1 = 5
// c = 10
//
// n = 0---0
// c = 1
//
// 3 / 5 = 0
// 3 % 5 = 3
// 3 + c = 3 + 1 = 4
// c = 1-
// n = 1-0---0

fn convert_dec_to_snafu(mut number: i64) -> String {
    let mut carry_over = 0;
    let mut number_builder = String::new();
    loop {
        if number == 0 {
            break;
        }
        let remainder = number % 5;
        number /= 5;
        let (value, carry) = match remainder + carry_over {
            0 => ('0', 0),
            1 => ('1', 0),
            2 => ('2', 0),
            3 => ('=', 1),
            4 => ('-', 1),
            5 => ('0', 1),
            a => unreachable!("{} {} {}", a, remainder, carry_over)
        };
        number_builder.push(value);
        carry_over = carry;
    }
    if carry_over != 0 {
        number_builder.push_str(match carry_over {
            1 => "1",
            2 => "2",
            3 => "1=",
            4 => "2=",
            5 => "10",
            _ => unreachable!()
        });
    }
    number_builder.chars().rev().collect::<String>()
}

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
            convert_dec_to_snafu(input.lines().map(|f| convert_snafu_to_dec(f)).sum::<i64>())
        .to_string()
    }

    // fn part2(&self, input: &str) -> String {
    // }
}
