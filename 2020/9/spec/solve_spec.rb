require_relative '../solve'

RSpec.describe Solve do
  it '' do
    input = <<-INPUT
35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576
INPUT

    result = Solve.new(input).get_init_number(5)

    expect(result).to eq(127)
  end

  describe '#range_min_max_sum' do
    describe 'with an init number of 127' do
      it 'finds a contiguous range of at least 2 that sums to 127, and sums the min/max' do
        input = <<-INPUT
35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576
INPUT

        result = Solve.new(input).range_min_max_sum(127)

        expect(result).to eq(62)
      end
    end
  end
end
