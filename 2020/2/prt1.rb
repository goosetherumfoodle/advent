require 'pry'

MATCHER = /(?<min>\d{1,2})-(?<max>\d{1,2})\s(?<char>\w):\s(?<pwd>\w+)/

File.open('./input') do |f|
  inputs = lines = f.readlines.map(&:chomp)
  valids = inputs.filter do |input|
    matches = input.match(MATCHER)
    return nil if !matches
    char_count = matches[:pwd].chars.find_all { |c| c == matches[:char] }.count
    matches[:min].to_i <= char_count && char_count <= matches[:max].to_i
  end

  puts "total: #{inputs.count}\tvalid: #{valids.count}"
end
