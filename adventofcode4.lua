#!/usr/bin/env lua5.3

function parseRoom(roomname)
  --io.write(string.format("roomname: %s\n", roomname))

  local checksum = string.match(roomname, "%[%a+%]")
  local sectorId = string.match(roomname, "-%d+%[")

  roomname = string.sub(roomname, 1, string.len(roomname) + 1 -
                        string.len(sectorId) - string.len(checksum))

  checksum = string.sub(checksum, 2, string.len(checksum) - 1)
  sectorId  = string.sub(sectorId, 2, string.len(sectorId) - 1)

  --io.write(string.format("checksum: %s\nsector id: %s\n\n",
                           --checksum, sectorId))

  characters = roomname:gsub("[-]", "")
  --io.write(string.format("encrypted name: %s\n\n", roomname))

  local letters = {}
  for c in characters:gmatch(".") do
    if not letters[c] then
      letters[c] = 0
    end
    letters[c] = letters[c] + 1
  end

  return letters, checksum, sectorId, roomname
end

function calculateChecksum(letters)
  --[[io.write("\nNot sorted:\n")
  for key, value in pairs(letters) do
    io.write(string.format("%s: %d\n", key, value))
  end
  --]]

  local arr = {}
  for key, value in pairs(letters) do
    table.insert(arr, {key, value})
  end

  table.sort(arr, function (a, b)
             return a[2] > b[2] or (a[2] == b[2] and a[1] < b[1])
             end
  )

  local i = 0
  local checksum = ""
  --io.write("\nSorted:\n")
  for key, value in pairs(arr) do
    --io.write(string.format("%s: %d\n", value[1], value[2]))
    if i < 5 then
      checksum = checksum .. value[1]
      i = i + 1
    end
  end
  --io.write(string.format("Checksum: %s\n", checksum))

  return checksum
end

function decryptRoomname(roomname, sectorId)
  --io.write(string.format("%s rotating %d rounds\n", roomname, sectorId))
  local decrypted = ""
  for c in roomname:gmatch(".") do
    if c == "-" then
      decrypted = decrypted .. " "
    else
      --io.write(string.format("%s\n", c))
        --rotate
      decrypted = decrypted ..
        string.char(((string.byte(c) - string.byte("a") + sectorId) %
          (string.byte("z") - string.byte("a") + 1)) + string.byte("a"))
    end
  end
  return decrypted
end

local sumOfIds = 0
repeat
  local roomname = io.read()
  local letters, checksum, sectorId, roomname = parseRoom(roomname)
  local calculatedChecksum = calculateChecksum(letters)
  if checksum == calculatedChecksum then
    --io.write("Real room!\n\n")
    sumOfIds = sumOfIds + sectorId
    local decrypted = decryptRoomname(roomname, sectorId)
    io.write(string.format("%s: %d\n", decrypted, sectorId))
  else
    --io.write(string.format("Not a real room! (%s - %s)\n\n",
                           --checksum, calculatedChecksum))
  end
until io.read(0) == nil

io.write(string.format("Sum of sector IDs: %d\n", sumOfIds))
--io.write("Bye\n")
