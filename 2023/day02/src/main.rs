use std::error::Error;

use advent_of_code_lib::{self, Solver};

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2023", "2")
}

struct Day;

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        input
            .lines()
            .filter_map(|f| {
                let mut split = f.split(": ");
                let id = split
                    .next()
                    .unwrap()
                    .replace("Game ", "")
                    .parse::<u32>()
                    .unwrap();

                let sets = split.next().unwrap().split("; ");
                for st in sets {
                    let balls = st.split(", ");
                    for ball in balls {
                        let mut comp = ball.split(" ");
                        let num = comp.next().unwrap().parse::<u32>().unwrap();
                        let colour = comp.next().unwrap();

                        if colour == "red" && num > 12 {
                            return None;
                        }
                        if colour == "green" && num > 13 {
                            return None;
                        }
                        if colour == "blue" && num > 14 {
                            return None;
                        }
                    }
                }
                return Some(id);
            })
            .sum::<u32>()
            .to_string()
    }

    fn part2(&self, input: &str) -> String {
        input
            .lines()
            .filter_map(|f| {
                let mut split = f.split(": ");
                split
                    .next()
                    .unwrap()
                    .replace("Game ", "")
                    .parse::<u32>()
                    .unwrap();

                let sets = split.next().unwrap().split("; ");
                let (red, green, blue) = sets.fold((0, 0, 0), |(red, green, blue), st| {
                    let balls = st.split(", ");
                    let (r, g, b) =
                        balls.fold((0, 0, 0), |(mut red, mut green, mut blue), ball| {
                            let mut comp = ball.split(" ");
                            let num = comp.next().unwrap().parse::<u32>().unwrap();
                            let colour = comp.next().unwrap();

                            match colour {
                                "red" => red = red.max(num),
                                "green" => green = green.max(num),
                                "blue" => blue = blue.max(num),
                                _ => panic!("Invalid state"),
                            }
                            return (red, green, blue);
                        });

                    return (red.max(r), green.max(g), blue.max(b));
                });
                return Some(red * green * blue);
            })
            .sum::<u32>()
            .to_string()
    }
}
