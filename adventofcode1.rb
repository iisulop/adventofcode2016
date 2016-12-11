#!/usr/bin/env ruby

DELTAS = [
  {:x=>1, :y=>0}, # North
  {:x=>0, :y=>1}, # East
  {:x=>-1, :y=>0}, # South
  {:x=>0, :y=>-1} # West
]

# How R/L affects DELTA
DIRECTIONS = {
  'L'=>-1,
  'R'=>1
}


path = []
File.open(ARGV[0]) do |f|
  path = f.read.split(', ')
end

x,y = 0,0
direction = 0 # North

visited = [[x,y]]
visited_twice = false

path.each do |move|
  # Get new heading
  direction = (direction + DIRECTIONS[move[0]]) % 4

  steps = move[1..-1]
  steps.to_i.times do |i|
    x = x + DELTAS[direction][:x]
    y = y + DELTAS[direction][:y]

    # Drop breadcrumbs until we stumble on to old breadcrumbs
    if not visited_twice
      coordinates = [x,y]
      if visited.include?(coordinates)
        visited_twice = true
      end
      visited.push(coordinates)
    end
  end

end

distance = x.abs + y.abs
puts "Ended up at (#{x},#{y}) - #{distance} blocks away from the start."

xx, yy = visited.last
distance = xx.abs + yy.abs
puts "First place visited twice at (#{xx},#{yy}) - #{distance} blocks away from the start"
