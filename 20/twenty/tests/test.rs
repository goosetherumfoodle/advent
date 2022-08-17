use twenty;
use twenty::tiles;
use std::collections::HashMap;

#[test]
fn test_rot_90() {
    let mut tile = tiles::Input { id: 1,  top: 0, right: 1, bottom: 2, left: 3}.build();

    tiles::rot_90(&mut tile);

    assert_eq!(tile.sides, [3,0,1,2]);
}

#[test]
fn test_rot_180() {
    let mut tile = tiles::Input { id: 1,  top: 0, right: 1, bottom: 2, left: 3}.build();

    tiles::rot_180(&mut tile);

    assert_eq!(tile.sides, [2,3,0,1]);
}

#[test]
fn test_rot_270() {
    let mut tile = tiles::Input { id: 1,  top: 0, right: 1, bottom: 2, left: 3}.build();

    tiles::rot_270(&mut tile);

    assert_eq!(tile.sides, [1,2,3,0]);
}

#[test]
fn test_build_top_segment() {
    let root_tile = tiles::Input { id: 1,  top: 10, right: 1010, bottom: 2020, left: 1 }.build();
    let root_row = vec![
        root_tile.clone(),
        tiles::Input { id: 2, top: 777, right: 1, bottom: 888, left: 2 }.build(),
        tiles::Input { id: 3,  top: 222, right: 2, bottom: 333, left: 111 }.build(),
        tiles::Input { id: 4, top: 11,  right: 20, bottom: 10, left: 3032 }.build(),
        tiles::Input { id: 5,  top: 119, right: 988, bottom: 759, left: 20 }.build(),
        tiles::Input { id: 6,  top: 132, right: 30, bottom: 11, left: 659 }.build(),
        tiles::Input { id: 7,  top: 4827, right: 1847, bottom: 9938, left: 30 }.build(),

    ];
    let state = twenty::State {
        tile_lookup: &twenty::build_tile_lookup(&root_row),
        side_lookup: &twenty::build_side_lookup(&root_row),
    };

    let result = twenty::build_top_segment(&state, &root_row);

    let expected = [
        [
            tiles::Tile { id: 6, sides: [132, 30, 11, 659] },
            tiles::Tile { id: 7, sides: [4827, 1847, 9938, 30] }
        ], [
            tiles::Tile { id: 4, sides: [11, 20, 10, 3032] },
            tiles::Tile { id: 5, sides: [119, 988, 759, 20] }
        ]
    ];
    assert_eq!(result, expected);
}

#[test]
fn test_build_row() {
    let root_tile = tiles::Input { id: 2,  top: 20, right: 1, bottom: 8989, left: 2}.build();
    let tiles = vec![
        root_tile.clone(),
        tiles::Input { id: 777,  top: 20, right: 555, bottom: 777, left: 777}.build(),
        tiles::Input { id: 1,    top: 111, right: 222, bottom: 2,   left: 333}.build(),
        tiles::Input { id: 3,    top: 1,   right: 999, bottom: 1010, left: 2020}.build(),
    ];
    let state = twenty::State {
        tile_lookup: &twenty::build_tile_lookup(&tiles),
        side_lookup: &twenty::build_side_lookup(&tiles),
    };

    let result: Vec<u16> = twenty::build_row(&state, &root_tile)
        .iter()
        .map(|t| t.id.clone() )
        .collect();

    let expected = vec![1,2,3];
    assert_eq!(result, expected);
}

#[test]
fn test_build_side_lookup() {
    let input = vec![
        tiles::Input { id: 1,  top: 1, bottom: 2, left: 3, right: 4 }.build(),
        tiles::Input { id: 2, top: 4, bottom: 5, left: 6, right: 7 }.build(),
        tiles::Input { id: 3,  top: 7, bottom: 8, left: 9, right: 10 }.build(),
    ];

    let result = twenty::build_side_lookup(&input);

    let mut expected = HashMap::new();
    expected.insert(4, vec![1, 2]);
    expected.insert(7, vec![2, 3]);
    assert_eq!(result, expected);
}

#[test]
#[ignore]
fn test_parse_all_tiles() {
    let input = "
Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..
";

    let result = twenty::parse_all_tiles(input);

    let expected = [
        tiles::Input { id: 2311, top: 300, bottom: 924, left: 318, right: 616 }.build(),
        tiles::Input { id: 1951, top: 397, bottom: 177, left: 587, right: 318 }.build(),
    ];
    assert_eq!(result, expected)
}

#[test]
#[ignore]
fn test_parse_all_sides() {
    let input = "
Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...
";

    let result = twenty::parse_all_sides(input);

    let expected = vec![
        (2311, 300),
        (2311, 231),
        (2311, 498),
        (2311, 616),
        (1951, 397),
        (1951, 564),
        (1951, 841),
        (1951, 318)
    ];
    assert_eq!(result, expected)
}

#[test]
#[ignore]
fn test_parse_tile() {
    let input = "
Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###
";

    let result = twenty::parse_tile(input);

    let expected = tiles::Input { id: 2311, top: 300, bottom: 924, left: 318, right: 616 }.build();
    assert_eq!(result, expected)
}

#[test]
#[ignore]
fn test_parse_sides() {
    let input = "
Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###
";

    let result = twenty::parse_sides(input);

    let expected = vec![
        (2311, 300),
        (2311, 231),
        (2311, 498),
        (2311, 616)
    ];
    assert_eq!(result, expected)
}

// #[test]
// #[ignore]
// fn test_parse_sides_2() {
//     let input = "
// Tile 3079:
// #.#.#####.
// .--------#
// .--------.
// #--------.
// #--------.
// .--------.
// #--------#
// .--------.
// .--------.
// ..#.###...
// ";

//     let result = twenty::parse_sides_2(input);

//     let expected = vec![vec!['a']];
//     assert_eq!(result, expected)
// }

#[test]
#[ignore]
fn test_parse_side() {
    let side = vec!['#','.','#','#'];

    let result = twenty::parse_side(side);

    assert_eq![result, 13];
}


#[test]
fn test_parse_side_flipee() {
    let side1 = vec!['#', '.', '.', '#', '#', '.', '#', '.', '.', '.'];
    let side2 = side1.clone().into_iter().rev().collect();

    let result1 = twenty::parse_side(side1);
    let result2 = twenty::parse_side(side2);

    assert_eq![result1, result2];
}


#[test]
fn test_assemble_input() {
    let input = "
Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
v####..#...
.....##...

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...
";

    let result = twenty::assemble_input(input);

    let expected = vec![
        [
            tiles::Tile { id: 2971, sides: [900, 2277, 859, 623] },
            tiles::Tile { id: 1489, sides: [935, 323, 1347, 2277] },
            tiles::Tile { id: 1171, sides: [148, 1188, 1926, 323] }
        ], [
            tiles::Tile { id: 2729, sides: [859, 593, 1722, 1578] },
            tiles::Tile { id: 1427, sides: [1347, 969, 862, 593] },
            tiles::Tile { id: 2473, sides: [1926, 1925, 485, 969] }],
        [
            tiles::Tile { id: 1951, sides: [1722, 1333, 971, 2420] },
            tiles::Tile { id: 2311, sides: [862, 805, 1441, 1333] },
            tiles::Tile { id: 3079, sides: [2057, 410, 485, 805] }
        ]
    ];
    assert_eq!(result, expected);
}

#[test]
fn test_cantor_pair_commutativity() {
    let x = 255;
    let y = 524;

    let result1 = twenty::cantor_pair(x,y);
    let result2 = twenty::cantor_pair(y,x);

    assert_eq!(result1, result2);
}
// left: `[[Tile { id: "2971:", sides: [900, 2277, 859, 623] }, Tile { id: "1489:", sides: [935, 323, 1347, 2277] }, Tile { id: "Tile 1171:", sides: [148, 1188, 1926, 323] }],
//         [Tile { id: "Tile 2729:", sides: [859, 593, 1722, 1578] }, Tile { id: "Tile 1427:", sides: [1347, 969, 862, 593] }, Tile { id: "Tile 2473:", sides: [1926, 1925, 485, 969] }],
//         [Tile { id: "Tile 1951:", sides: [1722, 1333, 971, 2420] }, Tile { id: "Tile 2311:", sides: [862, 805, 1441, 1333] }, Tile { id: "Tile 3079:", sides: [2057, 410, 485, 805] }]]`

// top: [[Tile { id: "Tile 2971:", sides: [900, 2277, 859, 623] }, Tile { id: "Tile 1489:", sides: [935, 323, 1347, 2277] }, Tile { id: "Tile 1171:", sides: [148, 1188, 1926, 323] }]]
// root: [Tile { id: "Tile 2729:", sides: [859, 593, 1722, 1578] }, Tile { id: "Tile 1427:", sides: [1347, 969, 862, 593] }, Tile { id: "Tile 2473:", sides: [1926, 1925, 485, 969] }]
// bott: [[Tile { id: "Tile 1951:", sides: [1722, 1333, 971, 2420] }, Tile { id: "Tile 2311:", sides: [862, 805, 1441, 1333] }, Tile { id: "Tile 3079:", sides: [2057, 410, 485, 805] }]]

// top: [[Tile { id: "Tile 2971:", sides: [900, 2277, 859, 623] },
//        Tile { id: "Tile 1489:", sides: [935, 323, 1347, 2277] },
//        Tile { id: "Tile 1171:", sides: [148, 1188, 1926, 323] }],
//       [Tile { id: "Tile 2729:", sides: [859, 593, 1722, 1578] },
//        Tile { id: "Tile 1427:", sides: [1347, 969, 862, 593] },
//        Tile { id: "Tile 2473:", sides: [1926, 1925, 485, 969] }]]
//     root: [Tile { id: "Tile 1951:", sides: [1722, 1333, 971, 2420] },
//            Tile { id: "Tile 2311:", sides: [862, 805, 1441, 1333] },
//            Tile { id: "Tile 3079:", sides: [2057, 410, 485, 805] }]

// OLD

// [
//     [Tile { id: "Tile 2729:", sides: [680, 9, 397, 962] }, Tile { id: "Tile 1427:", sides: [183, 348, 300, 9] }, Tile { id: "Tile 2473:", sides: [399, 481, 184, 348] }],
//     [Tile { id: "Tile 2971:", sides: [532, 689, 680, 78] }, Tile { id: "Tile 1489:", sides: [43, 288, 183, 689] }],
//     [Tile { id: "Tile 1951:", sides: [397, 318, 177, 587] }, Tile { id: "Tile 2311:", sides: [300, 616, 924, 318] }]]


// top:
// [[Tile { id: "Tile 2729:", sides: [680, 9, 397, 962] }, Tile { id: "Tile 1427:", sides: [183, 348, 300, 9] }, Tile { id: "Tile 2473:", sides: [399, 481, 184, 348] }]
//  [Tile { id: "Tile 2971:", sides: [532, 689, 680, 78] }, Tile { id: "Tile 1489:", sides: [43, 288, 183, 689] }]]
// [[Tile { id: "Tile 1951:", sides: [397, 318, 177, 587] }, Tile { id: "Tile 2311:", sides: [300, 616, 924, 318] }]]


// top: [[Tile { id: "Tile 2971:", sides: [532, 689, 680, 78] }, Tile { id: "Tile 1489:", sides: [43, 288, 183, 689] }],
//       [Tile { id: "Tile 2729:", sides: [680, 9, 397, 962] }, Tile { id: "Tile 1427:", sides: [183, 348, 300, 9] }, Tile { id: "Tile 2473:", sides: [399, 481, 184, 348] }]]
// root: [Tile { id: "Tile 1951:", sides: [397, 318, 177, 587] }, Tile { id: "Tile 2311:", sides: [300, 616, 924, 318] }]


// NEW

// [[Tile { id: "Tile 2971:", sides: [532, 689, 680, 78] },
//   Tile { id: "Tile 1489:", sides: [43, 288, 183, 689] }],
//  [Tile { id: "Tile 2729:", sides: [680, 9, 397, 962] },
//   Tile { id: "Tile 1427:", sides: [183, 348, 300, 9] },
//   Tile { id: "Tile 2473:", sides: [399, 481, 184, 348] }],
//  [Tile { id: "Tile 1951:", sides: [397, 318, 177, 587] },
//   Tile { id: "Tile 2311:", sides: [300, 616, 924, 318] }]]
