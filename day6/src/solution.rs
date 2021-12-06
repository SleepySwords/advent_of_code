
use utils;

pub struct Day;

impl utils::Solution for Day {
    fn part1(&self, input: &str) -> String {
        let mut adult_fish = [0u32; 7];
        let mut baby_fish = [0u32; 9];

        for fish in input.split(",").map(|x| x.parse::<usize>().unwrap()) {
            adult_fish[fish] += 1;
        }

        for day in 0..=80 {
            let grown_up = baby_fish[day % 9];
            baby_fish[day % 9] = 0;
            adult_fish[day % 7] += grown_up;

            let repo_fish = adult_fish[(day + 6) % 7];
            baby_fish[(day + 8) % 9] += repo_fish;
        }
        let count = baby_fish.into_iter().sum::<u32>()
            + adult_fish.into_iter().sum::<u32>();
        count.to_string()
    }

    fn part2(&self, input: &str) -> String {
        let mut adult_fish = [0u64; 7];
        let mut baby_fish = [0u64; 9];

        for fish in input.split(",").map(|x| x.parse::<usize>().unwrap()) {
            adult_fish[fish] += 1;
        }

        for day in 0..=256 {
            let grown_up = baby_fish[day % 9];
            baby_fish[day % 9] = 0;
            adult_fish[day % 7] += grown_up;

            let repo_fish = adult_fish[(day + 6) % 7];
            baby_fish[(day + 8) % 9] += repo_fish;
        }
        let count = baby_fish.into_iter().sum::<u64>()
            + adult_fish.into_iter().sum::<u64>();
        count.to_string()
    }
}
