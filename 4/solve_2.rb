require 'pry'
require_relative './solve_1'

class Solve_2

  def initialize(validators=nil)
    @validators = validators || default_validators
  end

  def run
    File.open('./input') do |f|
      input = f.readlines
      puts count_valid(input)
    end
  end

  def count_valid(input)
    group_id_text(input)
      .filter { |id| valid?(id) }
      .count
  end

  def group_id_text(text)
    Solve_1.group_id_text(text)
  end

  def valid?(id)
    @validators.to_a.all? do |field, field_validators|
      field_validators.all? do |validator|
        found_field = fetch_field(field, id)
        found_field && validator.call(found_field)
      end
    end
  end

  def fetch_field(field, text)
    text.match(/#{field}:(#?\w+)/)
      &.captures
      &.first
  end

  def default_validators
    {
      byr: [size_4, at_least_1920, at_most_2002],
      iyr: [size_4, at_least_2010, at_most_2020],
      eyr: [size_4, at_least_2020, at_most_2030],
      hgt: [in_or_cm, at_least_150cm, at_most_193cm, at_least_59in, at_most_76in],
      hcl: [rgb_color],
      ecl: [eye_color],
      pid: [size_9, only_digits],
    }
  end

  def size_4;         -> (x) { x.size == 4 }                               end
  def size_9;         -> (x) { x.size == 9 }                               end
  def at_least_1920;  -> (x) { 1920 <= x.to_i }                            end
  def at_least_2010;  -> (x) { 2010 <= x.to_i }                            end
  def at_least_2020;  -> (x) { 2020 <= x.to_i }                            end
  def at_most_2002;   -> (x) { x.to_i <= 2002 }                            end
  def at_most_2020;   -> (x) { x.to_i <= 2020 }                            end
  def at_most_2030;   -> (x) { x.to_i <= 2030 }                            end
  def at_least_150cm; -> (x) { inches.call(x) || 150 <= cm.call(x)  }      end
  def at_most_193cm;  -> (x) { inches.call(x) || cm.call(x) <= 193 }       end
  def at_least_59in;  -> (x) { cm.call(x)     || 59 <= inches.call(x)  }   end
  def at_most_76in;   -> (x) { cm.call(x)     || inches.call(x) <= 76 }    end
  def in_or_cm;       -> (x) { x.match?(/\d+(?:cm|in)/) }                  end
  def rgb_color;      -> (x) { x.match?(/#[a-f0-9]{6}/) }                  end
  def eye_color;      -> (x) { x.match?(/amb|blu|brn|gry|grn|hzl|oth/) }   end
  def only_digits;    -> (x) { x.match(/\A\d+\z/) }                        end
  def cm;             -> (x) { x.match(/(\d+)cm/)&.captures&.first&.to_i } end
  def inches;         -> (x) { x.match(/(\d+)in/)&.captures&.first&.to_i } end
end

if __FILE__ == $0
  Solve_2.new.run
end
