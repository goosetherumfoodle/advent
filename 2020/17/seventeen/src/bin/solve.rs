use seventeen::three_d as three_d;
use seventeen::four_d  as four_d;

const CYCLES: u8 = 6;

fn main() {
    let input = "
##.#...#
#..##...
....#..#
....####
#.#....#
###.#.#.
.#.#.#..
.#.....#
";

    let mut state_3d = three_d::parse_slice(input);
    let mut state_4d = four_d::parse_slice(input);

    three_d::cycle(CYCLES, &mut state_3d);
    four_d::cycle(CYCLES, &mut state_4d);

    println!("3D: after {} cycles, the count of active cubes is {}", CYCLES, state_3d.active.len());
    println!("4D: after {} cycles, the count of active cubes is {}", CYCLES, state_4d.active.len());
}
