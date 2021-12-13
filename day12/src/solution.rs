use utils;
use std::collections::HashMap;
use std::cell::RefCell;

pub struct Day;

#[derive(Debug)]
struct Node {
    display: String,
    connections: Vec<RefCell<Node>>,
}

impl Clone for Node {
    fn clone(&self) -> Self {
        Node {
            display: self.display.clone(),
            connections: self.connections.clone(),
        }
    }
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.display == other.display
    }
}

impl Node {
    fn number_paths_to(&self, node: &Node, banned: Vec<&Node>) -> u32 {
        if self == node {
            return 1;
        }
        let allowed_connections = self.connections.iter().filter(|&x| !banned.contains(&*(&x.borrow())));
        let mut paths = 0;
        for n in allowed_connections {
            let mut new = banned.clone();
            if self.display.chars().all(char::is_lowercase) {
                new.push(&self);
            }
            paths += self.number_paths_to(n, new);
        }
        return paths;
    }
}

impl utils::Solution for Day {
    fn part1(&self, input: &str) -> String {
        let mut map = HashMap::new();
        unsafe {
        for line in input.split("\n") {
            let cave1 = line.split("-").nth(0).unwrap();
            let cave2 = line.split("-").nth(1).unwrap();
            let node1 = map.entry(cave1).or_insert(Node {
                display: cave1.to_string(),
                connections: Vec::new()
            });
            let node2 = map.entry(cave2).or_insert(Node {
                display: cave2.to_string(),
                connections: Vec::new()
            });
            node1.connections.push(node2);
            node2.connections.push(node1);
        }
        }
       println!("{:?}", map);
        String::from("Not implemented!")
    }

    fn part2(&self, input: &str) -> String {
        String::from("Not implemented!")
    }
}
