require_relative '../solve'

RSpec.describe Solve do
  describe '::sum_of_disjunct_counts' do
    it 'sums every unique letter in groups' do
      input = <<-INPUT
abc

a
b
c

ab
ac

a
a
a
a

b
INPUT

      result = Solve.sum_of_disjunct_counts(input)

      expect(result).to eq(11)
    end
  end

  describe '::sum_of_disjunct_counts' do
    it 'sums universal letter in groups' do
      input = <<-INPUT
abc

a
b
c

ab
ac

a
a
a
a

b
INPUT

      result = Solve.sum_of_conjunct_counts(input)

      expect(result).to eq(6)
    end
  end
end
