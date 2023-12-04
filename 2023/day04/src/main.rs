use std::error::Error;

use advent_of_code_lib::{self, Solver};
use itertools::Itertools;

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2023", "4")
}

struct Day;

fn parse(input: &str) -> Vec<usize> {
    let number_matches = input
        .lines()
        .map(|f| {
            let mut num = f.split(": ");
            let _ = num
                .next()
                .unwrap()
                .replace("Card", "")
                .trim()
                .parse::<u32>();
            let mut cards = num.next().unwrap().split(" | ");
            let (winning_cards, current_cards) = (cards.next().unwrap(), cards.next().unwrap());

            let cards = winning_cards
                .chars()
                .chunks(3)
                .into_iter()
                .map(|f| f.collect::<String>().trim().parse::<u32>().unwrap())
                .collect::<Vec<u32>>();
            let current = current_cards
                .chars()
                .chunks(3)
                .into_iter()
                .map(|f| f.collect::<String>().trim().parse::<u32>().unwrap())
                .collect::<Vec<u32>>();
            cards
                .iter()
                .filter(|c| current.contains(c))
                .collect_vec()
                .len()
        })
        .collect::<Vec<usize>>();

    return number_matches
}

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        let number_matches = parse(input);

        number_matches
            .into_iter()
            .map(|f| {
                if f == 0 {
                    0
                } else {
                    2usize.pow((f - 1) as u32)
                }
            })
            .sum::<usize>()
            .to_string()
    }

    fn part2(&self, input: &str) -> String {
        let number_matches = parse(input);

        let mut copies = vec![1; number_matches.len()];

        for i in 0..copies.len() {
            for indexes in (i + 1)..=i + number_matches[i] {
                if indexes > copies.len() {
                    continue;
                }
                copies[indexes] += copies[i];
            }
        }

        copies.into_iter().sum::<u32>().to_string()
    }
}
