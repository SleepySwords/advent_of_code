use std::{collections::HashMap, ops::RangeBounds};

use utils;

pub struct Day;

// Things to note, there are at least 12 shared beacons between each scanner.


// Rather than pythagoras for efficency
fn manhatten_distance(beacon1: (isize, isize, isize), beacon2: (isize, isize, isize)) -> isize {
    (beacon1.0 - beacon2.0).abs() + (beacon1.1 - beacon2.1).abs() + (beacon1.2 - beacon2.2).abs()
}

// Rather than pythagoras for efficency
fn manhatten_distance_2d(beacon1: (isize, isize), beacon2: (isize, isize)) -> isize {
    (beacon1.0 - beacon2.0).abs() + (beacon1.1 - beacon2.1).abs()
}

fn get_distances(beacon: &Vec<(isize, isize, isize)>) -> HashMap<isize, ((isize, isize, isize), (isize, isize, isize))> {
    let mut distances = HashMap::new();
    for x in 0..beacon.len(){
        for y in &beacon[(x + 1)..beacon.len()] {
            distances.insert(manhatten_distance(beacon[x], *y), (beacon[x], *y));
        }
    }
    return distances;
}

fn get_offset(base_scanner: &Vec<(isize, isize, isize)>, relative_beacons: &Vec<(isize, isize, isize)>) {
    let map = get_distances(base_scanner);
    println!("{:#?}", map.keys().count());
    println!("{:?}", get_distances(relative_beacons).iter().filter(|&x| map.contains_key(x.0)).map(|x| *x.0).collect::<Vec<isize>>().len());
    println!("{:?}", get_distances(relative_beacons).iter().filter(|&x| map.contains_key(x.0)).map(|x| x.1).fold(Vec::new(), |mut acc, f| {
        if !acc.contains(&f.0) {
            acc.push(f.0);
        }
        if !acc.contains(&f.1) {
            acc.push(f.1);
        }
        acc
    }).len());
}

// Find the most common connected point and map that.

impl utils::Solution for Day {
    fn part1(&self, input: &str) -> String {
        let input = input.split("\n\n").map(|x| {
            let mut iter = x.split("\n");
            iter.next();
            iter.fold(Vec::new(), |mut acc, x| {
                let mut i = x.split(",").map(|v| isize::from_str_radix(v, 10).unwrap());
                acc.push((i.next().unwrap(), i.next().unwrap(), i.next().unwrap()));
                acc
            })
        }).collect::<Vec<Vec<(isize, isize, isize)>>>();
       
        // Compare everything to this, if not 13 beacons, then move on to the next one.
        let mut beacons = input[0].clone();

        println!("{:?}", get_offset(&beacons, &input[1]));

        let scanner2 = vec![(-1, -1), (-5, 0), (-2, 1)];

        for x in 0..scanner2.len(){
            for y in &scanner2[(x + 1)..scanner2.len()] {
                println!("{}", manhatten_distance_2d(scanner2[x], *y))
            }
        }
        // inputs.fold(Hashmap::new(), |x, v| {
        //      x.add(v.beakers.relativeTo(x))
        // }).count()
        String::from("Not implemented!")
    }

    fn part2(&self, input: &str) -> String {
        String::from("Not implemented!")
    }
}

