// OMG i mmmmmmmmmmmm
// I was doing this the whole time, fml i though that duplicates all moved at the same time
// I'm so stupid, i wasted so much of my life because of that pretense.



use std::{cell::RefCell, error::Error, rc::Rc};

use advent_of_code_lib::{self, Solver};
use itertools::Itertools;

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2022", "20")
}

struct Day;

fn cycle(pos: usize, offset: i32, max: usize) -> usize {
    let new = (pos as i32 + offset) % (max - 1) as i32;
    if new < 0 {
        (max as i32 - 1 + new) as usize
    } else {
        new as usize
    }
}

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        let count = input.lines().count();
        let numbers = input
            .lines()
            .map(|f| f.parse::<i32>().unwrap())
            .enumerate()
            .collect_vec();
        let mut mixed_numbers = numbers.clone();
        for (_, mix_number) in numbers {
            if mix_number == 0 {
                continue;
            }
            let ids = mixed_numbers
                .iter()
                .filter(|&&(_, z)| z == mix_number)
                .map(|&f| f)
                .collect_vec();
            let mut to_add = vec![];
            for (id_to_use, _) in ids {
                let pos = mixed_numbers
                    .iter()
                    .position(|&(id, _)| id == id_to_use)
                    .unwrap();
                // Use iterator, find prev point, filter iter + cycle, take however many, new
                // point is where to insert.
                let mut prev_pos = if pos == 0 { count - 1 } else { pos - 1 };
                println!("{}", pos);
                while mix_number == mixed_numbers[prev_pos].1 {
                    prev_pos = if prev_pos == 0 {
                        count - 1
                    } else {
                        prev_pos - 1
                    };
                }
                to_add.push((id_to_use, prev_pos));
            }
            println!("{:?}", to_add);
            mixed_numbers.retain(|&(_, v)| v != mix_number);
            for id in to_add {
                let pos = mixed_numbers.iter().position(|&(i, _)| i == id.1).unwrap();
                let pos = (pos as i32 + mix_number) % count as i32;
                mixed_numbers.insert(pos as usize, (id.0, mix_number));
            }
            println!("{:?} {}", mixed_numbers.iter().map(|f| f.1).collect_vec(), mix_number);
        }
        println!("{:?}", mixed_numbers);
        // let position_0 = mix.iter().position(|&z| z == 0).unwrap();
        // (mix[(position_0 + 1000) % count]
        //     + mix[(position_0 + 2000) % count]
        //     + mix[(position_0 + 3000) % count])
        0.to_string()
    }

    // fn part2(&self, input: &str) -> String {
    // }
}

// 1, 2, 3, 2, 6, 5 <- FFS this also shifts the entire shit.
// 1, 3, 6, 2, 5 <- FFS this also shifts the entire shit.
// -1, 6, -1, 5 <- FFS this also shifts the entire shit.

// #[test]
// fn test_file1() -> Result<(), Box<dyn Error>> {
//     advent_of_code_lib::test_file(Day, "test1", advent_of_code_lib::Part::Part1)
// }
//
//

// I give up...
//

struct Node {
    prev: Option<Rc<RefCell<Node>>>,
    next: Option<Rc<RefCell<Node>>>,
    value: i32,
    id: usize,
}

fn find(value: i32, node: RefCell<Node>) -> Vec<Rc<RefCell<Node>>> {
    let id = node.borrow().id;
    let c = node;
    let mut found = vec![];
    loop {
        let next = &c.borrow().next;
        let c = next.clone().unwrap().clone();
        if c.borrow().value == value {
            found.push(c.clone());
        }
        if c.borrow().id == id {
            break;
        }
    }
    found
}

fn parse(input: &str) {}
