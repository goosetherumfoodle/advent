require_relative '../solve'

RSpec.describe Main do
  describe '#lower' do
    it 'selects lower half' do
      rows = (0..127).to_a

      result = Main.lower(rows)

      expect(result).to eq((0..63).to_a)
    end

    it 'is a closure' do
      rows = (0..127).to_a

      result = Main.lower(Main.lower(rows))

      expect(result).to eq((0..31).to_a)
    end
  end

  describe '#upper' do
    it 'selects upper half' do
      rows = (0..127).to_a

      result = Main.upper(rows)

      expect(result).to eq((64..127).to_a)
    end
  end

  describe '#translate_ticket_subset' do
    it 'translates F to lower' do
      ticket = 'F'

      result = Main.translate_ticket_subset(ticket)

      expect(result).to eq([Main.method(:lower)])
    end

    it 'translates L to lower' do
      ticket = 'L'

      result = Main.translate_ticket_subset(ticket)

      expect(result).to eq([Main.method(:lower)])
    end

    it 'translates B to upper' do
      ticket = 'B'

      result = Main.translate_ticket_subset(ticket)

      expect(result).to eq([Main.method(:upper)])
    end

    it 'translates R to upper' do
      ticket = 'R'

      result = Main.translate_ticket_subset(ticket)

      expect(result).to eq([Main.method(:upper)])
    end

    it 'translates multiple' do
      ticket = 'BFLR'

      result = Main.translate_ticket_subset(ticket)

      expected = [
        Main.method(:upper),
        Main.method(:lower),
        Main.method(:lower),
        Main.method(:upper),
      ]
      expect(result).to eq(expected)
    end
  end

  describe '#translate_ticket' do
    it 'works' do
      ticket = 'FBFBBFFRLR'

      result = Main.translate_ticket(ticket)

      expected = [
        [
          Main.method(:lower),
          Main.method(:upper),
          Main.method(:lower),
          Main.method(:upper),
          Main.method(:upper),
          Main.method(:lower),
          Main.method(:lower),
        ],
        [
          Main.method(:upper),
          Main.method(:lower),
          Main.method(:upper),
        ]
      ]
      expect(result).to eq(expected)
    end
  end
end
