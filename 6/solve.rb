require 'pry'

class Solve
  def self.run
    File.open('./input') do |f|
      input = f.read
      puts "sum of disjunct counts: #{sum_of_disjunct_counts(input)}"
      puts "sum of conjunct counts: #{sum_of_conjunct_counts(input)}"
    end
  end

  def self.sum_of_conjunct_counts(input)
    group(input)
      .map { |answers| answer_intersection(answers) }
      .map { |set| set.size }
      .reduce(&:+)
  end

  def self.answer_intersection(answers)
    first_seen = answers.first.chars.to_set
    answers.reduce(first_seen) do |already_seen, answer|
      already_seen.intersection(answer.chars.to_set)
    end
  end

  def self.sum_of_disjunct_counts(input)
    group(input)
      .map(&:join)
      .map { |group| group.chars.to_set.size }
      .reduce(:+)
  end

  def self.group(input)
    input.lines
      .map(&:chomp)
      .reduce([[]]) do |accum, ansr|
        if ansr.empty?
          accum << []
        else
          accum.last << ansr
        end
        accum
      end
  end
end

if __FILE__ == $0
  Solve.run
end
