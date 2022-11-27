use std::collections::HashMap;

use utils;

pub struct Day;

impl utils::Solution for Day {
    fn part1(&self, input: &str) -> String {
        let mut map = HashMap::new();
        for x in input.split("\n\n").nth(1).unwrap().split("\n") {
            map.insert(x.split(" -> ").nth(0).unwrap(), x.split(" -> ").nth(1).unwrap());
        }
        let mut inp = input.split("\n\n").nth(0).unwrap().trim().to_string();
        let mut clone = inp.clone();
        for x in 0..10 {
            for index in 0..inp.len() - 1 {
                if let Some(string) = map.get(&inp[index..index + 2]) { 
                    clone.insert_str((2 * index) + 1, string)
                }
            }
            inp = clone;
            clone = inp.clone();
        }
        let hm = inp.chars().fold(HashMap::new(), |mut hm, c| {
            match hm.get(&c) { // borrow of hm
                None => hm.insert(c, 1),
                Some(i) => { // i is a reference
                    let j = *i; // i is Copy so we copy it, j is not a reference owned by hm, so hm is not borrowed anymore
                    hm.insert(c, j + 1) // we can borrow hm mutable
                }
            };
            hm
        });
        println!("{:?}", &hm);
        (hm.values().max().unwrap() - hm.values().min().unwrap()).to_string()
    }

    fn part2(&self, input: &str) -> String {
        let mut map = HashMap::new();
        for x in input.split("\n\n").nth(1).unwrap().split("\n") {
            map.insert(x.split(" -> ").nth(0).unwrap().to_string(), x.split(" -> ").nth(1).unwrap().to_string());
        }
        let mut count = HashMap::<String, u128>::new();
        let mut inp = input.split("\n\n").nth(0).unwrap().trim().to_string();
        for index in 0..inp.len() - 1 {
            let c = count.entry(inp[index..index + 2].to_string()).or_insert(0);
            *c += 1;
        }
        for x in 0..40 {
            let cloned = count.clone();
            count.clear();
            for (key, value) in cloned {
                if value < 1 { continue }
                let string = &map[&*key];
                // if let Some(string) = map.get(&(&key).to_string()) {
                    let mut clone = key.clone().to_string();
                    clone.insert_str(1, &string);
                    let map_value = count.entry(clone[0..2].to_string()).or_insert(0);
                    *map_value += value;
                    
                    let map_value = count.entry(clone[1..3].to_string()).or_insert(0);
                    *map_value += value;
                // }
            }
            dbg!(&count);
            println!();
        }

        let hm = count.iter().fold(HashMap::new(), |mut hm, (key, value)| {
            let char1 = key.chars().nth(0).unwrap();
            let char2 = key.chars().nth(1).unwrap();

                let v2 = hm.entry(char1).or_insert(0);
                *v2 += *value;
                
                let v2 = hm.entry(char2).or_insert(0);
                *v2 += *value;
            hm
        });

        dbg!(&hm);

        ((hm.values().max().unwrap() / 2) - (hm.values().min().unwrap() / 2) + 1).to_string()
    }
}

