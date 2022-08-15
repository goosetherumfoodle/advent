use std::{
    collections::{HashMap, LinkedList},
    iter::Peekable,
    str::Lines,
    mem::swap,
};
use tiles::Tile;

pub mod tiles {
    use Side::*;

    pub enum Side {
        Top,
        Bottom,
        Left,
        Right,
    }

    #[derive(PartialEq, Debug, Clone)]
    pub struct Tile {
        pub id: String,
        pub sides: [u16; 4],
    }

    pub struct Input {
        pub id: String,
        pub top: u16,
        pub bottom: u16,
        pub left: u16,
        pub right: u16,
    }

    impl Input {
        pub fn build(&self) -> Tile {
            Tile {
                id: (*self.id).to_string(),
                sides: [self.top, self.right, self.bottom, self.left],
            }
        }
    }

    impl Tile {
        #[rustfmt::skip]
        pub fn which(&self, side: u16) -> Option<Side> {
            if self.get_top() == side { Some(Top) }
            else if self.get_bottom() == side { Some(Bottom) }
            else if self.get_left() == side { Some(Left) }
            else if self.get_right() == side { Some(Right) }
            else { None }
        }

        pub fn get_top(&self) -> u16 { self.sides[0] }
        pub fn get_bottom(&self) -> u16 { self.sides[2] }
        pub fn get_left(&self) -> u16 { self.sides[3] }
        pub fn get_right(&self) -> u16 { self.sides[1] }
    }

    pub fn rot_90(tile: &mut Tile) {
        tile.sides.swap(1, 0);
        tile.sides.swap(2, 0);
        tile.sides.swap(3, 0);
    }

    pub fn rot_180(tile: &mut Tile) {
        rot_90(tile);
        rot_90(tile);
    }

    pub fn rot_270(tile: &mut Tile) {
        rot_90(tile);
        rot_90(tile);
        rot_90(tile);
    }
}

type SideLookup = HashMap<u16, Vec<String>>;
type TileLookup<'a> = HashMap<String, &'a Tile>;

pub struct State<'a> {
    pub side_lookup: &'a SideLookup,
    pub tile_lookup: &'a TileLookup<'a>,
}

pub fn assemble_input(input: &str) -> Vec<Vec<Tile>> {
    let tiles = parse_all_tiles(input);
    let root_tile = tiles.get(0).unwrap();

    let state = State {
        side_lookup: &build_side_lookup(&tiles),
        tile_lookup: &build_tile_lookup(&tiles),
    };

    println!("building root!");
    let root_row = build_row(&state, root_tile);
    println!("building top!");
    let mut top_seg = build_top_segment(&state, &root_row);
    println!("building bottom!");
    let mut bot_seg = build_bottom_segment(&state, &root_row);
    println!("top: {:?}", top_seg);
    println!("root: {:?}", root_row);
    println!("bott: {:?}", bot_seg);
    let mut square = Vec::new();
    square.append(&mut top_seg);
    square.push(root_row);
    square.append(&mut bot_seg);
    square
}

pub fn build_side_lookup(tiles: &Vec<Tile>) -> SideLookup {
    let mut all_sides = HashMap::new();
    for tile in tiles {
        for side in tile.sides {
            all_sides
                .entry(side)
                .and_modify(|xs: &mut Vec<String>| xs.push(tile.id.clone()))
                .or_insert_with(|| vec![tile.id.clone()]);
        }
    }

    // horse
    println!("raw tiles: {:?}", tiles);
    println!("raw lookup: {:?}", all_sides);

    let mut just_shared_sides = HashMap::new();
    for (side, ids) in all_sides {
        if ids.len() > 2 {
            panic!("Invalid tile inputs. side: {side}; ids: {:?}", ids);
        }
        if ids.len() > 1 {
            just_shared_sides.insert(side, ids);
        }
    }
    println!("lookup: {:?}", just_shared_sides);
    just_shared_sides
}

pub fn build_tile_lookup(tiles: &Vec<Tile>) -> TileLookup {
    let mut out = HashMap::new();
    for tile in tiles {
        out.insert(tile.id.clone(), tile);
    }
    out
}

fn next_is_empty(lines: &mut Peekable<Lines>) -> bool {
    lines.peek().unwrap_or(&"").trim().is_empty()
}

fn consume_empty(lines: &mut Peekable<Lines>) {
    if next_is_empty(lines) {
        lines.next();
    }
}

fn align_upward(tile: &mut Tile, side: u16) {
    match tile.which(side) {
        Some(tiles::Side::Top) => tiles::rot_180(tile),
        Some(tiles::Side::Right) => tiles::rot_90(tile),
        Some(tiles::Side::Left) => tiles::rot_270(tile),
        Some(tiles::Side::Bottom) => {}
        None => {}
    }
}

fn align_downward(tile: &mut Tile, side: u16) {
    match tile.which(side) {
        Some(tiles::Side::Top) => {}
        Some(tiles::Side::Right) => tiles::rot_270(tile),
        Some(tiles::Side::Left) => tiles::rot_90(tile),
        Some(tiles::Side::Bottom) => tiles::rot_180(tile),
        None => {}
    }
}


fn align_rightward(tile: &mut Tile, side: u16) {
    match tile.which(side) {
        Some(tiles::Side::Top) => tiles::rot_270(tile),
        Some(tiles::Side::Right) => tiles::rot_180(tile),
        Some(tiles::Side::Left) => {}
        Some(tiles::Side::Bottom) => tiles::rot_90(tile),
        None => {}
    }
}

fn align_leftward(tile: &mut Tile, side: u16) {
    match tile.which(side) {
        Some(tiles::Side::Top) => tiles::rot_90(tile),
        Some(tiles::Side::Right) => {}
        Some(tiles::Side::Left) => tiles::rot_180(tile),
        Some(tiles::Side::Bottom) => tiles::rot_270(tile),
        None => {}
    }
}

fn build_up<'a>(
    state: &State<'a>,
    list: &'a mut LinkedList<Tile>,
    root_tile: &Tile,
) -> &'a mut LinkedList<Tile> {
    build_towards(
        state,
        LinkedList::push_back,
        list,
        align_upward,
        Tile::get_top,
        root_tile,
    )
}

fn build_down<'a>(
    state: &State<'a>,
    list: &'a mut LinkedList<Tile>,
    root_tile: &Tile,
) -> &'a mut LinkedList<Tile> {
    build_towards(
        state,
        LinkedList::push_front,
        list,
        align_downward,
        Tile::get_bottom,
        root_tile,
    )
}


fn build_right<'a>(
    state: &State<'a>,
    list: &'a mut LinkedList<Tile>,
    root_tile: &Tile,
) -> &'a mut LinkedList<Tile> {
    build_towards(
        state,
        LinkedList::push_back,
        list,
        align_rightward,
        Tile::get_right,
        root_tile,
    )
}

fn build_left<'a>(
    state: &State<'a>,
    list: &'a mut LinkedList<Tile>,
    root_tile: &Tile,
) -> &'a mut LinkedList<Tile> {
    build_towards(
        state,
        LinkedList::push_front,
        list,
        align_leftward,
        Tile::get_left,
        root_tile,
    )
}

fn build_towards<'a>(
    state: &State,
    push: fn(&mut LinkedList<Tile>, Tile),
    list: &'a mut LinkedList<Tile>,
    align: fn(&mut Tile, u16),
    towards: fn(&Tile) -> u16,
    root_tile: &Tile,
) -> &'a mut LinkedList<Tile> {
    let mut next_tile = root_tile.clone();
    let mut next_side = towards(root_tile);

    let mut killer = 0;
    println!("root tile: {:?}", next_tile);
    println!("start loop looking for: {next_side}: {}", state.side_lookup.get(&next_side).is_some());
    while state.side_lookup.get(&next_side).is_some() {
        killer += 1;
        if killer > 5 {
            panic!("infinite loop")
        }
        println!("next tile: {:?}", next_tile);
        println!(
            "looking up {next_side}: {:?}",
            state.side_lookup.get(&next_side)
        );
        let next_tile_ids: &Vec<String> = state
            .side_lookup
            .get(&next_side)
            .expect("all sides in SideLookup");
        let next_tile_id = if next_tile_ids[0] == next_tile.id {
            &next_tile_ids[1]
        } else {
            &next_tile_ids[0]
        };
        println!("next tile id: {next_tile_id}");
        next_tile = (*state
            .tile_lookup
            .get(next_tile_id)
            .expect("all sides in TileLookup"))
        .clone();
        println!("new next tile: {:?}", next_tile);

        align(&mut next_tile, next_side);
        println!("now aligned  : {:?}", next_tile);
        push(list, (next_tile).clone());
        next_side = towards(&next_tile);
        println!("new next side: {next_side}");
    }
    println!("building ended! not found: {next_side}: {}", state.side_lookup.get(&next_side).is_some());
    println!();
    list
}

pub fn build_row(state: &State, root_tile: &Tile) -> Vec<Tile> {
    let mut row = LinkedList::new();
    row.push_front(root_tile.clone());
    println!("building row right!");
    build_right(state, &mut row, root_tile);
    println!("building row left!");
    build_left(state, &mut row, root_tile);

    let mut out = Vec::new();
    for a in row {
        out.push(a);
    }
    out
}

pub fn build_top_segment(state: &State, bottom_row: &[Tile]) -> Vec<Vec<Tile>> {
    let root = &bottom_row[0];

    let mut row = LinkedList::new();
    build_up(state, &mut row, root);


    let mut left_col = LinkedList::new();
    for tile in &row {
        let mut new_row = LinkedList::new();
        new_row.push_back((*tile).clone());
        build_right(state, &mut new_row, tile);
        left_col.push_front(new_row);
    }

    let mut out = Vec::new();
    for row in left_col {
        let mut row_vec = Vec::new();
        for tile in row {
            row_vec.push(tile);
        }
        out.push(row_vec);
    }
    out
}

fn build_bottom_segment(state: &State, top_row: &[Tile]) -> Vec<Vec<Tile>> {
    let root = &top_row[0];

    let mut row = LinkedList::new();
    println!("GOING DOWN!");
    build_down(state, &mut row, root);


    println!("down row: {:?}", row);
    let mut left_col = Vec::new();
    for tile in &row {
        let mut new_row = LinkedList::new();
        new_row.push_back((*tile).clone());
        build_right(state, &mut new_row, tile);
        left_col.push(new_row);
    }

    // println!("left_col: {:?}", left_col);
    let mut out = Vec::new();
    for row in left_col {
        let mut row_vec = Vec::new();
        for tile in row {
            row_vec.push(tile);
        }
        out.push(row_vec);
    }
    out
}

pub fn parse_all_tiles(input: &str) -> Vec<Tile> {
    let mut lines = input.trim().lines().peekable();
    let mut out = Vec::new();
    while lines.peek().is_some() {
        let mut panel = Vec::new();
        consume_empty(&mut lines);
        while !next_is_empty(&mut lines) {
            panel.push(lines.next().unwrap());
        }
        let parsed = parse_tile(&panel.join("\n"));
        out.push(parsed);
    }
    out
}

pub fn parse_tile(input: &str) -> Tile {
    let input = input.trim();
    let mut lines = input.lines().clone();
    let title = lines.next().unwrap();
    let top = lines.next().unwrap();
    let bottom = lines.last().unwrap();

    let mut lines = input.lines();
    lines.next();
    let mut left = Vec::new();
    let mut right = Vec::new();
    for l in lines {
        let mut chars = l.chars();
        let first = chars.next().unwrap();
        left.push(first);
        right.push(chars.last().unwrap());
    }
    tiles::Input {
        id: title.to_string(),
        top: parse_side(top.chars().collect()),
        bottom: parse_side(bottom.chars().collect()),
        left: parse_side(left),
        right: parse_side(right),
    }
    .build()
}

pub fn parse_all_sides(input: &str) -> Vec<(String, u16)> {
    let mut lines = input.trim().lines().peekable();
    let mut out = Vec::new();
    while lines.peek().is_some() {
        let mut panel = Vec::new();
        consume_empty(&mut lines);
        while !next_is_empty(&mut lines) {
            panel.push(lines.next().unwrap());
        }
        let parsed = parse_sides(&panel.join("\n"));
        out.push(parsed);
    }
    out.concat()
}

pub fn parse_sides(input: &str) -> Vec<(String, u16)> {
    let input = input.trim();
    let mut lines = input.lines().clone();
    let title = lines.next().unwrap();
    let top = lines.next().unwrap();
    let bottom = lines.last().unwrap();

    let mut ls = input.lines();
    ls.next();
    let mut left = Vec::new();
    let mut right = Vec::new();
    for l in ls {
        let mut chars = l.chars();
        let first = chars.next().unwrap();
        left.push(first);
        right.push(chars.last().unwrap());
    }
    vec![
        (title.to_string(), parse_side(top.chars().collect())),
        (title.to_string(), parse_side(bottom.chars().collect())),
        (title.to_string(), parse_side(left)),
        (title.to_string(), parse_side(right)),
    ]
}

pub fn parse_side(side: Vec<char>) -> u16 {
    let mut first = 0;
    for (i, chr) in side.iter().enumerate() {
        if *chr == '#' {
            first += 2_u16.pow((i).try_into().unwrap());
        }
    }

    let mut second = 0;
    for (i, chr) in side.iter().rev().enumerate() {
        if *chr == '#' {
            second += 2_u16.pow((i).try_into().unwrap());
        }
    }

    cantor_pair(first, second)
}

pub fn cantor_pair(x: u16, y: u16) -> u16 {
    let mut x = x as u32;
    let mut y = y as u32;
    if x <= y { swap(&mut x, &mut y) }

    (((x + y) % u16::MAX as u32) * ((x + y + 1) % u16::MAX as u32) / (2 + x)) as u16
}
