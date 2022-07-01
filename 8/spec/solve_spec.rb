require_relative '../solve'

RSpec.describe Solve do
  describe '::evaluate' do
    describe 'with instructions that terminate' do
      it 'reports the final state of the evaluator as terminated' do
        instructions = <<-INPUT
nop +0
    INPUT

        result = Solve.new(instructions).evaluate

        expect(result[:state]).to eq(:terminated)
      end
    end

    describe 'in normal mode' do
      it 'reports the final value of the accumulator' do
        instructions = <<-INPUT
0 nop +0
1 acc +1
2 jmp +4
3 acc +3
4 jmp -3
5 acc -99
6 acc +1
7 jmp -4
8 acc +6
    INPUT

        result = Solve.new(instructions).evaluate

        expect(result[:accumulator]).to eq(5)
      end

      describe 'with instructions that loop' do
        it 'reports the final state of the evaluator as a loop' do
          instructions = <<-INPUT
nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
    INPUT

          result = Solve.new(instructions).evaluate

          expect(result[:state]).to eq(:loop)
        end
      end
    end

    describe 'in fix mode' do
      describe 'with a fixable loop' do
        it 'fixes the loop and terminates' do
          instructions = <<-INPUT
nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
    INPUT

          result = Solve.new(instructions, fix: true).evaluate

          expect(result[:state]).to eq(:terminated)
          expect(result[:accumulator]).to eq(8)
        end
      end
    end
  end
end
