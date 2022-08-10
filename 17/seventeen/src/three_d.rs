use std::collections::HashMap;
use std::collections::HashSet;
use std::iter::zip;

const N_COUNT: usize = 3_usize.pow(3) - 1;

pub type Coord = (i32,i32,i32);
type NeighborLookup = HashMap<Coord, HashSet<Coord>>;

#[derive(PartialEq)]
#[derive(Clone)]
#[derive(Debug)]
pub struct State {
    pub active: HashSet<Coord>,
    pub neighbor_lookup: NeighborLookup,
}

pub fn parse_slice(input: &str) -> State {
    let mut output = State {
        active: HashSet::new(),
        neighbor_lookup: HashMap::new()
    };
    for (row_index, row) in input.trim().lines().rev().enumerate() {
        for (col, chr) in row.char_indices() {
            let coord = (col as i32, row_index as i32,  0);
            output.neighbor_lookup.insert(coord, build_neighbors(&coord));
            if chr == '#' {
                output.active.insert(coord);
            }
        }
    }
    return output;
}

pub fn cycle(cycles: u8, state: &mut State) {
    let mut cycle = cycles.clone();
    while cycle > 0 {
        cycle -= 1;
        next_state(state);
    }
}

pub fn next_actives<'a>(contentions: HashSet<Coord>, state: &mut State) -> HashSet<Coord> {
    let mut new_actives = HashSet::new();
    for cube in contentions {
        let neighs = get_neighbors(&cube, &mut state.neighbor_lookup);
        let cube_active = state.active.contains(&cube);
        let active_ns = neighs.iter().filter(|n| { state.active.contains(&n) }).count();
        if cube_active && (2 <= active_ns && active_ns <= 3){
            new_actives.insert(cube);
        } else if !cube_active && active_ns == 3 {
            new_actives.insert(cube);
        }
    }
    return new_actives;
}

fn coordinate_transforms() -> Vec<Coord> {
    let mut all = Vec::new();
    for a in [-1, 0, 1] {
        for b in [-1, 0, 1] {
            for c in [-1, 0, 1] {
                if a != 0 || b != 0 || c != 0 {
                    all.push((a,b,c));
                }
            }
        }
    }
    return all;
}

pub fn build_neighbors(coord: &Coord) -> HashSet<Coord> {
    let mut neighbors = HashSet::new();

    let base = [coord; N_COUNT];
    let dirs = zip(coordinate_transforms(), base).map(|(a, b)| {(a.0 + b.0, a.1 + b.1, a.2 + b.2)});

    for dir in dirs {
        neighbors.insert(dir);
    }
    return neighbors;
}

fn get_neighbors<'a,'b>(coord: &'b Coord, lookup: &mut NeighborLookup) -> HashSet<Coord> {
    match lookup.get(&coord) {
        Some(neighbors) => { return neighbors.clone() },
        None            => {
            let neighbors = build_neighbors(&coord);
            lookup.insert(*coord, neighbors.clone());
            return neighbors;
        },
    }
}

pub fn next_state(state: &mut State ) {
    let mut in_play = HashSet::new();
    for active in state.active.iter() {
        let neighbors = get_neighbors(active, &mut state.neighbor_lookup);
        for neighbor in neighbors.iter() {
            in_play.insert(*neighbor);
        };
        in_play.insert(*active);
    }
    state.active = next_actives(in_play, state);
}
