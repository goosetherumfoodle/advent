MATCHER = /(?<min>\d{1,2})-(?<max>\d{1,2})\s(?<char>\w):\s(?<pwd>\w+)/

File.open('./input') do |f|
  inputs = lines = f.readlines.map(&:chomp)
  valids = inputs&.filter do |input|
    matches = input.match(MATCHER)
    return nil if !matches
    _, min, max, char, pwd = matches.to_a
    min_i = min.to_i - 1
    max_i = max.to_i - 1
    min_match = pwd[min_i] == char
    max_match = pwd[max_i] == char
    min_match ^ max_match
  end

  puts "total: #{inputs.count}\tvalid: #{valids.count}"
end
