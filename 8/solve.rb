require 'set'

class Solve
  INITIAL_STATE = {
    accumulator: 0,
    pointer: 0,
  }

  def initialize(instructions, fix: false)
    @seen = Set.new
    @instructions = parse_instructions(instructions)
    @fixable_states = []
    @block_fixable_states = !fix
  end

  def evaluate(state=INITIAL_STATE.dup)
    if terminated?(state)
      state.merge({state: :terminated})
    elsif looping?(state)
      evaluate_hack(state)
    else
      @seen << state[:pointer]
      evaluate(new_state(state))
    end
  end

  def evaluate_hack(state)
    @block_fixable_states = true
    if @fixable_states.empty?
      state.merge({state: :loop})
    else
      evaluate(new_state_hack)
    end
  end

  def parse_instructions(input)
    input.lines.map do |l|
      match = l.match(/(?<cmd>\w+) (?<int>[+-]\d+)/)
      {int: match[:int].to_i, cmd: match[:cmd].intern}
    end
  end

  def increment_pointer(state, inc = 1)
    state[:pointer] += inc
  end

  def increment_accumulator(state, inc = 1)
    state[:accumulator] += inc
  end

  def nop_action(state)
    increment_pointer(state)
  end

  def jump_action(state, jump_inc)
    increment_pointer(state, jump_inc)
  end

  def acc_action(state, acc_inc)
    increment_accumulator(state, acc_inc)
    increment_pointer(state)
  end

  def new_state_hack
    state = last_fixable_state
    chop_fixable_states
    curr = @instructions[state[:pointer]]
    case curr[:cmd]
    when :jmp
      nop_action(state)
    when :acc
      acc_action(state, curr[:int])
    when :nop
      jump_action(state, curr[:int])
    else
      raise "unrecognized instruction: #{curr[:cmd]}"
    end
    state
  end

  def new_state(state)
    curr = @instructions[state[:pointer]]
    case curr[:cmd]
    when :jmp
      push_fixable_state(state)
      jump_action(state, curr[:int])
    when :acc
      acc_action(state, curr[:int])
    when :nop
      push_fixable_state(state)
      nop_action(state)
    else
      raise "unrecognized instruction: #{curr[:cmd]}"
    end
    state
  end

  def push_fixable_state(state)
    return nil if @block_fixable_states

    @fixable_states << state.dup
  end

  def last_fixable_state
    @fixable_states.last
  end

  def chop_fixable_states
    if @fixable_states.size > 1
      @fixable_states = @fixable_states[0..@fixable_states.size-2]
    else
      @fixable_states = []
    end
  end

  def terminated?(state)
    state[:pointer] > @instructions.size - 1
  end

  def looping?(state)
    if @seen.include? state[:pointer]
      return true
    else
      false
    end
  end
end

if __FILE__ == $0
  puts Solve.new(File.open('./input').read, fix: ARGV[0] == 'fix').evaluate
end
