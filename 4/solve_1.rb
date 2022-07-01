require 'pry'

class Solve_1
  REQUIRED_DEFAULT = %w{byr iyr eyr hgt hcl ecl pid}

  def self.run
    File.open('./input') do |f|
      input = f.readlines
      puts count_valid(input)
    end
  end

  def self.count_valid(input)
    group_id_text(input)
      .filter { |id| valid?(id) }
      .count
  end

  def self.valid?(id, required_fields: REQUIRED_DEFAULT)
    required_fields.all? { |req| id.match?(/#{req}:./) }
  end

  def self.group_id_text(text)
    text.reduce(['']) do |accum, line|
      if line == "\n"
        accum.push('')
      else
        accum[-1] += line
      end
      accum
    end
      .map { |line| line.strip }
      .map { |line| line.gsub(/\n/, ' ') }
  end
end

if __FILE__ == $0
    Solve_1.run
end
