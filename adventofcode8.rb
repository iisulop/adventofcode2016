#!/usr/bin/env ruby

X=50
Y=6

class Screen
  def initialize(cols, rows)
    @cols = cols
    @rows = rows
    @screen = Array.new(@rows) { Array.new(@cols) }
    @transposed = false
  end

  def rect(x, y)
    @transposed ? transpose : nil
    @screen = @screen.each_with_index.map do |row, j|
      if j < y
        row = row.each_with_index.map do |column, i|
          if i < x
            column = true
          end
          column
        end
      else
        row
      end
    end
  end

  def transpose()
    @transposed = !@transposed
    @screen = @screen.transpose
  end

  def rotate(direction, row, amount)
    if direction == :row
      @transposed ? transpose : nil
    elsif direction == :column
      @transposed ? nil : transpose
    end

    @screen[row].rotate!(-amount)
  end

  def litPixels()
    @screen.inject(0) do |litCol, column|
      litCol = litCol + column.inject(0) do |litPx, pixel|
        if pixel == true
          litPx = litPx + 1
        else
          litPx
        end
      end
    end
  end

  def to_s()
    @transposed ? transpose : nil

    @screen.inject("") do |screen, row|
      screen << row.inject("") do |column, pixel|
        column << (pixel == true ? "#" : ".")
      end
      screen << "\n"
    end
  end
end

screen = Screen.new(X, Y)

while input = gets do
  input.match(/rect (?<x>\d+)x(?<y>\d+)/) do |cmd|
    screen.rect(cmd[:x].to_i, cmd[:y].to_i)
  end

  input.match(/rotate (?<direction>\w+) [xy]=(?<row>\d+) by (?<amount>\d+)/) do |cmd|
    screen.rotate(cmd[:direction].to_sym, cmd[:row].to_i, cmd[:amount].to_i)
  end
end

puts "Lit pixels: #{screen.litPixels}"
puts screen
