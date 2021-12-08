use utils;

pub struct Day;

impl utils::Solution for Day {
    fn part1(&self, input: &str) -> String {
        let number: usize = input
            .split("\n")
            .map(|x| x.split("|").nth(1).unwrap().trim())
            .map(|x| x.split(" ").filter(|y| y.len() == 2 || y.len() == 4 || y.len() == 3 || y.len() == 7).count())
            .sum();
        number.to_string()
    }

    fn part2(&self, input: &str) -> String {
        let input = input.split("\n");
        let mut total = 0;
        for line in input {
            let unique: Vec<&str> = line
                .split("|")
                .nth(0)
                .unwrap()
                .trim()
                .split(" ")
                .collect();

            let num7 = unique.iter().filter(|x| x.len() == 3).nth(0).unwrap();
            let num1 = unique.iter().filter(|x| x.len() == 2).nth(0).unwrap();
            let num4 = unique.iter().filter(|x| x.len() == 4).nth(0).unwrap();
            let num8 = unique.iter().filter(|x| x.len() == 7).nth(0).unwrap();

            let num3 = unique.iter().filter(|x| x.len() == 5)
                .filter(|x| x.contains(num1.chars().nth(0).unwrap()) && x.contains(num1.chars().nth(1).unwrap()))
                .nth(0)
                .unwrap();

            let a = num7
                .chars()
                .filter(|&x| !num1.contains(x))
                .nth(0)
                .unwrap();

            let b = num4
                .split("")
                .filter(|x| !num3.contains(x))
                .nth(0)
                .unwrap();
            
            let d = num4
                .split("")
                .filter(|x| num3.contains(x))
                .filter(|x| !num1.contains(x))
                .nth(0)
                .unwrap();

            let num0 = unique.iter().filter(|x| x.len() == 6)
                .filter(|&x| !x.contains(d))
                .nth(0)
                .unwrap();

            let num9 = unique.iter().filter(|x| x.len() == 6)
                .filter(|&x| x.contains(d))
                .filter(|&x| x.contains(num1.chars().nth(0).unwrap()) && 
                        x.contains(num1.chars().nth(1).unwrap()))
                .nth(0)
                .unwrap();

            let num6 = unique.iter().filter(|x| x.len() == 6)
                .filter(|&x| x.contains(d))
                .filter(|&x| x.contains(num1.chars().nth(0).unwrap()) ^
                        x.contains(num1.chars().nth(1).unwrap()))
                .nth(0)
                .unwrap();

            let num5 = unique.iter().filter(|x| x.len() == 5)
                .filter(|&x| x.contains(b))
                .nth(0)
                .unwrap();

            let num2 = unique.iter().filter(|x| x.len() == 5)
                .filter(|&x| !x.contains(b))
                .filter(|&x| x.chars().filter(|&y| num3.contains(y)).count() != 5)
                .nth(0)
                .unwrap();

            let nums = vec![
                num0,
                num1,
                num2,
                num3,
                num4,
                num5,
                num6,
                num7,
                num8,
                num9
            ];
            
            let output: Vec<&str> = line
                .split("|")
                .nth(1)
                .unwrap()
                .trim()
                .split(" ")
                .collect();

            let mut output_str = String::from("");
            
            for digit in output {
                for x in 0..=9 {
                    if digit.len() != nums[x].len() { continue; }
                    if digit.chars().filter(|&y| nums[x].contains(y)).count() == digit.len() {
                        output_str.push(char::from_digit(x as u32, 10).unwrap());
                    }
                }
            }
            total += u32::from_str_radix(&output_str, 10).unwrap();
        }
        total.to_string()
    }
}
