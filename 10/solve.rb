require 'pry'

class Solve

  def initialize(input)
    @array = build_sort_array(input)
  end

  def build_sort_array(raw_input)
    input = raw_input.lines.map(&:chomp).map(&:to_i)
    a = Array.new(input.size)
    a[0] = 0 # represents power origin
    input.each { |x| a[x] = x }
    a
  end

  def get_combinations
    skippable = []
    @array.each.with_index do |val, i|
      x = i >= 1 && val && @array[i - 1] && @array[i + 1]
      if x
        skippable << val
      end
    end
    [skippable.size, 2**skippable.size, skippable]
  end

  def run
    jumps = get_jumps
    # puts "jumps: #{jumps}"
    puts "1 jolt by 3 jolts: #{jumps[1] * jumps[3]}"
    puts "combos: #{(2 ** jumps[1].size) + (4 ** jumps[3].size)}"
  end

  def get_jumps
    jumps = @array.reduce({prev: 0}) do |accum, jolts|
      if jolts
        jump = jolts - accum[:prev]
        if accum[jump]
          accum[jump] += 1
        else
          accum[jump] = 1
        end
        accum[:prev] = jolts
      end
      accum
    end
    jumps.delete(:prev)
    jumps[3] += 1 # represents computer
    jumps
  end
end

if __FILE__ == $0
  puts Solve.new(File.open('./input').read).run
end
