use core::fmt;
use std::{cell::RefCell, error::Error, rc::Rc};

use advent_of_code_lib::{self, Solver};

fn main() -> Result<(), Box<dyn Error>> {
    advent_of_code_lib::run_and_print(Day, "2022", "7")
}

struct Day;

#[derive(Debug)]
struct File {
    size: u32,
}

struct Directory {
    name: String,
    files: Vec<File>,
    directories: Vec<Rc<RefCell<Directory>>>,
    parent: Option<Rc<RefCell<Directory>>>,
}

impl Directory {
    fn size(&self) -> u32 {
        self.directories
            .iter()
            .map(|f| f.borrow().size())
            .sum::<u32>()
            + self.files.iter().map(|f| f.size).sum::<u32>()
    }

    fn count_files_less_than(&self, count: &mut u32) {
        if self.size() < 100000 {
            *count += self.size();
        }
        for ele in &self.directories {
            ele.borrow().count_files_less_than(count);
        }
    }

    fn smallest_to_delete(&self, min_clear: u32, count: &mut u32) {
        if self.size() >= min_clear {
            if *count > self.size() {
                *count = self.size();
            }
        }
        for ele in &self.directories {
            ele.borrow().smallest_to_delete(min_clear, count);
        }
    }
}

impl fmt::Debug for Directory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} [files: {:?}, directories: {:?}]",
            self.name, self.files, self.directories
        )
    }
}

fn generate_file_structure(input: &str) -> Rc<RefCell<Directory>> {
    let root = Directory {
        name: String::from("/"),
        files: vec![],
        directories: vec![],
        parent: None,
    };
    let mut head = Rc::new(RefCell::new(root));
    let mut commands = input.lines().peekable();
    commands.next();
    while let Some(command) = commands.next() {
        if command == "$ ls" {
            while let Some(response) = commands.next_if(|response| !response.starts_with("$ ")) {
                if response.starts_with("dir") {
                    let name = String::from(&response[4..]);
                    head.borrow_mut()
                        .directories
                        .push(Rc::new(RefCell::new(Directory {
                            name,
                            files: vec![],
                            directories: vec![],
                            parent: Some(head.clone()),
                        })))
                } else {
                    let (size, _) = response.split_once(" ").unwrap();
                    head.borrow_mut().files.push(File {
                        size: size.parse().unwrap(),
                    })
                }
            }
        } else {
            if command == "$ cd .." {
                let changed = (&head.borrow().parent).clone().unwrap();
                head = changed;
            } else {
                let name = command[5..].to_string();
                let changed = head
                    .borrow()
                    .directories
                    .iter()
                    .find(|f| f.borrow().name == name)
                    .unwrap()
                    .clone();
                head = changed;
            }
        }
    }
    while let Some(_) = &head.clone().borrow().parent {
        let changed = (&head.borrow().parent).clone().unwrap();
        head = changed;
    }
    head
}

impl Solver for Day {
    fn part1(&self, input: &str) -> String {
        let head = generate_file_structure(input);
        let mut count = 0;
        head.borrow().count_files_less_than(&mut count);
        count.to_string()
    }

    fn part2(&self, input: &str) -> String {
        let head = generate_file_structure(input);
        let mut size_to_delete = 70000000;
        let min_clear = head.borrow().size() - 40_000_000;
        head.borrow().smallest_to_delete(min_clear, &mut size_to_delete);

        size_to_delete.to_string()
    }
}
