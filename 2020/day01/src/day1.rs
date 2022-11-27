use crate::Solution;
pub struct Day1;

impl Solution for Day1 {
    fn part1(input: &String) -> String {
        let input: Vec<u32> = input.split('\n').filter_map(|s| s.parse::<u32>().ok()).collect();
        let mut count = 0;
        for x in 0..input.len() - 1 {
            if input[x] < input[x + 1] { count += 1 };
        }
        count.to_string()
    }

    fn part2(input: &String) -> String {
        let input: Vec<u32> = input.split('\n').filter_map(|s| s.parse::<u32>().ok()).collect();
        let mut count = 0;
        for x in 0..input.len() - 3 {
            if input[x] < input[x + 3] { count += 1 };
        }
        count.to_string()
    }
}

