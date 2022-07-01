require 'pry'

class Solve

  PREAMBLE = 25

  def initialize(input)
    @input = input.lines.map(&:chomp)
  end

  def run
    init = get_init_number
    sum = range_min_max_sum(init)
    puts "init:\t#{init}\nsum:\t#{sum}"
  end

  def get_init_number(preamble_length=PREAMBLE)
    lines = @input.map { |l| l.chomp.to_i }
    preamble = lines[0..preamble_length-1]
    lines = lines[preamble_length..-1]
    found = nil
    lines.reduce(preamble) do |check, line|
      if found
        nil
      else
        if !check.permutation(2).to_a.any? { |a, b| a + b == line }
          found = line
        end
        check.shift
        check << line
      end
    end
    found
  end

  def range_min_max_sum(sums_to)
    init = {
      solved: false,
      prev: [],
    }

    result = @input.reduce(init) do |accum, line|
      if accum[:solved]
        accum
      else
        num = line.to_i
        accum[:prev].unshift(num)
        sum = 0
        i = 0
        while sum < sums_to && i < accum[:prev].size - 1 do
          sum += accum[:prev][i]
          i += 1
        end

        if sum == sums_to && accum[:prev].size > 1
          accum[:solved] = accum[:prev][0..i].min + accum[:prev][0..i].max
          binding.pry
        end

        accum
      end
    end

    return result[:solved]
  end
end

if __FILE__ == $0
  puts Solve.new(File.open('./input').read).run
end
