require 'pry'

class Main
  def self.start(part)
    File.open('./input') do |f|
      inputs = lines = f.readlines.map(&:chomp)
      slope = inputs.map { |line| Layer.new(line) }
      if part == '1'
        part_1(slope)
      elsif part == '2'
        part_2(slope)
      end
    end
  end

  def self.part_1(slope)
      hits = slope.filter.with_index { |layer, i| layer.hit?(i*3) }.count
      puts hits
  end

  def self.part_2(slope)
    angles = [
      {r: 1, d: 1},
      {r: 3, d: 1},
      {r: 5, d: 1},
      {r: 7, d: 1},
      {r: 1, d: 2},
    ]

    hit_per_angle = angles.map do |angle|
      angle in {d:, r:}

      slope
        .filter.with_index { |layer, i| i.remainder(d) == 0 }
        .filter.with_index { |layer, i| layer.hit?(i * r) }
        .count
    end
    puts "hits per angle: #{hit_per_angle}"
    puts "product: #{hit_per_angle.reduce(:*)}"
  end

  def self.read_layer(terrain)
    return Layer.new(terrain)
  end

  class Layer
    TREE = '#'

    def initialize(terrain)
      @terrain = terrain.chars
    end

    def hit?(x_axis)
      if extrapolate(x_axis) == TREE
        true
      else
        false
      end
    end

    def extrapolate(x)
      i = x.modulo(@terrain.count)
      @terrain[i]
    end
  end
end


if __FILE__== $0
  Main.start(ARGV[0])
end
