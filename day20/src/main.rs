mod solution;

use utils;

fn main() {
    let day = env!("CARGO_PKG_NAME").replace("day", "").parse::<u32>().unwrap();
    let solution = solution::Day {  };
    utils::test(String::from("part1"), &solution);
    utils::test(String::from("part2"), &solution);

    println!();

    utils::setup(day, solution);
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_testcases() {
        unimplemented!();
    }
}

