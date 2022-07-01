require 'pry'

class Solve

  def self.run(start)
    start ||= 'shiny gold'
    input = File.read('./input')
    puts "possible containers: #{count_containers(clean(start), input)}"
    puts "required contents: #{count_contents(clean(start), input)}"
  end

  def self.count_containers(start, input)
    accum_containers(start, graph(input)).count
  end

  def self.count_contents(start, input)
    accum_contents(start, graph(input))
      .map { |content| content[:count] }
      .reduce(&:+) || 0
  end

  def self.accum_contents(start, graph, accum = [])
    if contents(graph, start)
      contents = contents(graph, start)
      contents.each { |content| accum << content }
      contents.each do |content|
        (1..content[:count]).each do
          accum_contents(content[:name], graph, accum)
        end
      end
    end
    accum
  end

  def self.accum_containers(start, graph, accum = [].to_set)
    if contained_by(graph, start)
      nodes = contained_by(graph, start)
      nodes.each { |n| accum << n }
      nodes.each { |n| accum_containers(n, graph, accum) }
    end
    accum
  end

  def self.graph(input)
    input.lines
      .map    { |line| parse_line(line) }
      .reduce do |accum, elem|
        accum.merge(elem) do |_, val_1, val_2|
          merged = {}
          join_merge(:contained_by, val_1, val_2, merged)
          join_merge(:contains, val_1, val_2, merged)
          merged
        end
      end
  end

  def self.join_merge(key, val_1, val_2, merging)
    merging[key] = val_1.fetch(key, []) + val_2.fetch(key, []) if val_1[key] || val_2[key]
    merging
  end

  def self.parse_line(line)
    container_match = /((?:\w|\s)+) bags contain/.match(line)&.captures&.first
    contain_matches = line.scan(/(?:(?:(?<count>\d) (?<name>[ a-z]*) bags?))+/)
    container = clean(container_match)
    contents = contain_matches.map { |count, name| {count: count.to_i, name: clean(name) }}

    graph = contents.reduce({}) { |g, content| set_contained_by(g, content[:name], container) }
    set_contents(graph, container, contents)
  end

  def self.contents(graph, key)
    graph.fetch(key, nil)&.fetch(:contains, nil)
  end

  def self.set_contents(graph, container, contents)
    if graph[container] && graph[container][:contains]
      graph[container][:contains] += contents
    else
      graph[container] = {contains: contents}
    end
    graph
  end

  def self.contained_by(graph, key)
    graph.fetch(key, nil)&.fetch(:contained_by, nil)
  end

  def self.set_contained_by(graph, content, container)
    if graph[content] && graph[content][:contained_by]
      graph[content][:contained_by] += [container]
    else
      graph[content] = {contained_by: [container]}
    end
    graph
  end

  def self.clean(string)
    string.gsub(' ', '_').intern
  end
end

if __FILE__ == $0
  Solve.run(ARGV[0])
end
