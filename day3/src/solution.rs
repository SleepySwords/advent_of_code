
use utils;

pub struct Day;

impl utils::Solution for Day {
    fn part1(&self, input: &str) -> String {
        let data: Vec<&str> = input.split('\n').collect();
        let diagnostic_entry_length = data[0].len();
        let mut gamma_rate = String::from("");

        for index in 0..diagnostic_entry_length {
            let num_of_1s = data.iter().filter(|x| x.chars().nth(index).unwrap() == '1').count();
            if num_of_1s > (data.len() - num_of_1s) {
                gamma_rate += "1"
            } else {
                gamma_rate += "0"
            }
        }

        let gamma_rate = u32::from_str_radix(&gamma_rate, 2).unwrap();
        let epsilon = !gamma_rate & (1 << diagnostic_entry_length) - 1;
        (gamma_rate * epsilon).to_string()
    }

    fn part2(&self, input: &str) -> String {
        let mut data: Vec<&str> = input.split('\n').collect();
        let binary_length = data[0].len();

        for index in 0..binary_length {
            let filtered = data.clone().into_iter().filter(|&x| x.chars().nth(index).unwrap() == '1');
            let bit_number = filtered.clone().count();
            if bit_number >= (data.len() - bit_number) {
                data = filtered.collect();
            } else {
                for bad_value in filtered {
                    data.retain(|&x| x != bad_value);
                }
            }
        }

        let oxygen_rating = u32::from_str_radix(data[0], 2).unwrap();
        
        let mut data: Vec<&str> = input.split('\n').collect();
        for index in 0..binary_length {
            if data.len() == 1 { continue; }
            let filtered = data.clone().into_iter().filter(|&x| x.chars().nth(index).unwrap() == '1');
            let bit_number = filtered.count();
            if bit_number >= (data.len() - bit_number) {
                for bad_value in filtered {
                    data.retain(|&x| x != bad_value);
                }
            } else {
                data = filtered.clone().collect();
            }
        }

        let co2_rating = u32::from_str_radix(data[0], 2).unwrap();

        (oxygen_rating * co2_rating).to_string()
    }
}

