require_relative '../solve'

RSpec.describe Solve do
  describe '#get_combinations' do
    it 'lists jump counts' do
      input = <<-INPUT
28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3
INPUT

      jumps = Solve.new(input).get_combinations

      expect(jumps).to eq(19208)
    end
  end

  describe '#get_jumps' do
    it 'lists jump counts' do
      input = <<-INPUT
28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3
INPUT

      jumps = Solve.new(input).get_jumps

      expect(jumps).to eq({1 => 22, 3 => 10})
    end
  end
end
