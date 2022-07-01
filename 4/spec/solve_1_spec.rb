require 'pry'
require_relative '../solve_1'

RSpec.describe Solve_1 do
  describe '#count_valid' do
    it 'gives the count of valid ids' do
      input = <<-INPUT
ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
    INPUT

      result = Solve_1.count_valid(input.lines)

      expect(result).to eq(2)
    end
  end

  describe '#group_id_text' do
    it 'groups id texts, removing newlines' do
      input = <<-INPUT
ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929
INPUT

      result = Solve_1.group_id_text(input.lines)

      expected = [
        'ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm',
        'iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929'
      ]
      expect(result).to eq(expected)
    end
  end

  describe '#validate' do
    describe 'with all required fields' do
      it 'is valid' do
        required_fields = %w{first second}
        input = 'blah:blargh second:horse first:blah horse:horse'

        expect(Solve_1.valid?(input, required_fields: required_fields)).to eq(true)
      end
    end

    describe 'with missing fields' do
      it 'is invalid' do
        required_fields = %w{first second}
        input = 'second:horse horse:horse'

        expect(Solve_1.valid?(input, required_fields: required_fields)).to eq(false)
      end
    end
  end
end
