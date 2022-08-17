require_relative '../solve'

RSpec.describe Solve do
  describe '::count_containers' do
    it 'correct example output' do
      input = <<-INPUT
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
INPUT

      result = Solve.count_containers(:shiny_gold, input)

      expect(result).to eq(4)
    end
  end

  describe '::count_contents' do
    it 'correct example output' do
      input = <<-INPUT
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
INPUT

      result = Solve.count_contents(:shiny_gold, input)

      expect(result).to eq(32)
    end
  end


  describe '::parse_line' do
    it 'parses one contained bag' do
      line = 'light red bags contain 1 bright white bag.'

      result = Solve.parse_line(line)

      expected = {
        light_red: {
          contains: [
            {
              count: 1,
              name: :bright_white
            }
          ],
        },
        bright_white: {
          contained_by: [:light_red],
        },
      }
      expect(result).to eq(expected)
    end

    it 'parses two contained bags' do
      line = 'light red bags contain 1 bright white bag, 2 muted yellow bags.'

      result = Solve.parse_line(line)

      expected = {
        light_red: {
          contains: [
            {count: 1, name: :bright_white},
            {count: 2, name: :muted_yellow},
          ]
        },
        bright_white: {
          contained_by: [:light_red]
        },
        muted_yellow: {
          contained_by: [:light_red],
        }
      }
      expect(result).to eq(expected)
    end
  end
end
