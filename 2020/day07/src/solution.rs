use utils;

pub struct Day;

impl Day {
    fn sum_of_consecutives(num: i32) -> i32 {
        (num * (num + 1)) / 2
    }
}

impl utils::Solution for Day {
    fn part1(&self, input: &str) -> String {
        let input: Vec<i32> = input.split(",").map(|x| x.parse::<i32>().unwrap()).collect();
        let mut lowest = i32::MAX;
        for pos in 0..*input.iter().max().unwrap() {
            let amount = input.
                iter()
                .map(|x| (x - pos).abs())
                .sum();
            if amount < lowest {
                lowest = amount;
            }
        }
        lowest.to_string()
    }

    fn part2(&self, input: &str) -> String {
        let input: Vec<i32> = input.split(",").map(|x| x.parse::<i32>().unwrap()).collect();
        let mut lowest = i32::MAX;
        for pos in 0..*input.iter().max().unwrap() {
            let amount = input
                .iter()
                .map(|x| Day::sum_of_consecutives((x - pos).abs()))
                .sum();
            if amount < lowest {
                lowest = amount;
            }
        }
        lowest.to_string()
    }
}
