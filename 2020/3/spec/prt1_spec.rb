require_relative '../prt1'

RSpec.describe Main do
  describe '#read_layer' do
    describe 'without extrapolation' do
      it 'detects tree hits' do
        layer = Main.read_layer('..#')

        expect(layer.hit?(2)).to eq(true)
      end

      it 'detects tree misses' do
        layer = Main.read_layer('..#')

        expect(layer.hit?(1)).to eq(false)
      end
    end

    describe 'with extrapolation' do
      it 'detects tree hits' do
        layer = Main.read_layer('..#')

        expect(layer.hit?(5)).to eq(true)
      end

      it 'detects tree misses' do
        layer = Main.read_layer('..#')

        expect(layer.hit?(6)).to eq(false)
      end
    end
  end
end
