
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
        let mut oxygen_data: Vec<&str> = input.split('\n').collect();
        let mut co2_data: Vec<&str> = input.split('\n').collect();
        let binary_length = oxygen_data[0].len();

        for index in 0..binary_length {
            let data_with_1_bit: Vec<&str> = oxygen_data
                .clone()
                .into_iter()
                .filter(|&x| x.chars().nth(index).unwrap() == '1')
                .collect();

            let size_of_1s = data_with_1_bit.len();
            if size_of_1s >= (oxygen_data.len() - size_of_1s) {
                oxygen_data = data_with_1_bit;
            } else {
                for bad_value in data_with_1_bit {
                    oxygen_data.retain(|&x| x != bad_value);
                }
            }
        }

        for index in 0..binary_length {
            if co2_data.len() == 1 { continue; }
            let data_with_1_bit: Vec<&str> = co2_data
                .clone()
                .into_iter()
                .filter(|&x| x.chars().nth(index).unwrap() == '1')
                .collect();

            let size_of_1s = data_with_1_bit.len();
            if size_of_1s >= (co2_data.len() - size_of_1s) {
                for bad_value in data_with_1_bit {
                    co2_data.retain(|&x| x != bad_value);
                }
            } else {
                co2_data = data_with_1_bit;
            }
        }

        let oxygen_rating = u32::from_str_radix(oxygen_data[0], 2).unwrap();
        let co2_rating = u32::from_str_radix(co2_data[0], 2).unwrap();

        (oxygen_rating * co2_rating).to_string()
    }
}

