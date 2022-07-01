require 'pry'

class Main
  ALL_ROWS = (0..127).to_a
  ALL_COLUMNS = (0..7).to_a

  def self.run
    File.open('./input') do |f|
      input = f.readlines.map(&:chomp)
      seat_ids = get_seat_ids(input)
      puts "higest seat id: #{seat_ids.max}"
      puts "missing: #{missing_ids(seat_ids)}"
    end
  end

  def self.missing_ids(ids)
    ids.sort.reduce([[], nil]) do |(missing, last), id|
      if last && id - last > 1
        missing << ((last + 1)..(id - 1)).to_a
      end
      [missing.flatten, id]
    end
      .first
  end

  def self.get_seat_ids(tickets)
    tickets
      .map { |tix| translate_ticket(tix) }
      .map do |row_finders, col_finders|
        row = row_finders.reduce(ALL_ROWS)    { |seats, finder| finder.call(seats) }.first
        col = col_finders.reduce(ALL_COLUMNS) { |seats, finder| finder.call(seats) }.first
        [row, col]
      end
      .map { |row, col| seat_id(row, col) }
  end

  def self.seat_id(row, column)
    row * 8 + column
  end

  def self.lower(rows)
    rows[0..(rows.size/2-1)]
  end

  def self.upper(rows)
    rows[(rows.size/2)..rows.size-1]
  end

  def self.translate_ticket(ticket)
    row_wise = ticket[0..6]
    column_wise = ticket[7..9]
    [row_wise, column_wise].map do |subset|
      translate_ticket_subset(subset)
    end
  end

  def self.translate_ticket_subset(subset)
    subset.chars.map do |char|
      case char
      when 'F'
        self.method(:lower)
      when 'B'
        self.method(:upper)
      when 'L'
        self.method(:lower)
      when 'R'
        self.method(:upper)
      else
        raise "#{char} is an invalid character in the ticket #{ticket}"
      end
    end
  end
end

if __FILE__ == $0
  Main.run
end
