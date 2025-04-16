local args = {...}

local outputFormats = {
  bin = { suffix = ".bin", format = function(program)
    local str = ""
    for _, byte in ipairs(program) do
      str = str .. string.char(byte)
    end
    return str
  end },
  hex = { suffix = ".hex", format = function(program)
    local str = ""
    for _, byte in ipairs(program) do
      str = str .. string.format("%02X\n", byte)
    end
    return str
  end }
}

local function newLineParser(lineNum, line)
  return {
    lineNum = lineNum,
    line = line,
    pos = 1,
    eat = function(self, num)
      local str = self:peek(num)
      self.pos = self.pos + num
      return str
    end,
    peek = function(self, num)
      return self.line:sub(self.pos, self.pos + num - 1)
    end,
    skipWhitespace = function(self)
      while self.pos <= #self.line and self.line:sub(self.pos, self.pos) == " " do
        self:eat(1)
      end
    end,
    expect = function(self, str)
      if self.line:sub(self.pos, self.pos + #str - 1) == str then
        self:eat(#str)
        return true
      end
      error("Expected " .. str .. " at line " .. self.lineNum .. ", pos " .. self.pos)
    end,
    isEmpty = function(self)
      return self.pos > #self.line
    end,
  }
end

local function parseWord(lineParser, regex)
  lineParser:skipWhitespace()
  local word = {}

  while lineParser.pos <= #lineParser.line and lineParser:peek(1):match(regex) do
    word[#word + 1] = lineParser:peek(1)
    lineParser:eat(1)
  end

  local result = table.concat(word, "")
  if result == "" then
    error("Invalid word at line " .. lineParser.lineNum .. ", pos " .. lineParser.pos)
  end

  return result
end

local function parseReg(lineParser)
  lineParser:skipWhitespace()
  local reg = lineParser:eat(1)
  local regNum = string.byte(reg) - string.byte("a")
  if regNum < 0 or regNum > 7 then
    error("Invalid register at line " .. lineParser.lineNum .. ", pos " .. lineParser.pos)
  end

  return regNum
end

local function parseRegexNumber(lineParser, regex, requireDigit, base)
  local numStr = ""
  if requireDigit and not lineParser:peek(1):match(regex) then
    error("Invalid number at line " .. lineParser.lineNum .. ", pos " .. lineParser.pos)
  end
  while lineParser.pos <= #lineParser.line and lineParser:peek(1):match(regex) do
    numStr = numStr .. lineParser:peek(1)
    lineParser:eat(1)
  end
  return tonumber(numStr, base or 10) or 0
end

local function parseNumber(lineParser)
  lineParser:skipWhitespace()
  local multiplier = 1
  if lineParser:peek(1) == "-" then
    multiplier = -1
    lineParser:eat(1)
  end
  if lineParser:peek(1) == "0" then
    lineParser:eat(1)
    if lineParser:peek(1) == "x" then
      lineParser:eat(1)
      return parseRegexNumber(lineParser, "[0-9a-fA-F]", true, 16) * multiplier
    elseif lineParser:peek(1) == "b" then
      lineParser:eat(1)
      return parseRegexNumber(lineParser, "[01]", true, 2) * multiplier
    elseif lineParser:peek(1) == "o" then
      lineParser:eat(1)
      return parseRegexNumber(lineParser, "[0-7]", true, 8) * multiplier
    else
      return parseRegexNumber(lineParser, "[0-9]", false, 10) * multiplier
    end
  end
  return parseRegexNumber(lineParser, "[0-9]", true, 10) * multiplier
end

local operators = {
  ["+"] = {prec = 1, assoc = "left", fn = function(a, b) return a + b end},
  ["-"] = {prec = 1, assoc = "left", fn = function(a, b) return a - b end},
  ["*"] = {prec = 2, assoc = "left", fn = function(a, b) return a * b end},
  ["/"] = {prec = 2, assoc = "left", fn = function(a, b) return a / b end},
}

local parseMath
local function parseExpression(lineParser, ctx)
  lineParser:skipWhitespace()
  if lineParser:peek(1) == "(" then
    lineParser:eat(1)
    local result = parseMath(lineParser, ctx)
    lineParser:expect(")")
    return result
  elseif lineParser:peek(1) == "@" then
    lineParser:eat(1)
    return ctx.relOffset
  else
    return parseNumber(lineParser)
  end
end

parseMath = function(lineParser, ctx)
  local values = {}
  local ops = {}
  local function applyOp()
    local op = table.remove(ops)
    local b = table.remove(values)
    local a = table.remove(values)
    table.insert(values, operators[op].fn(a, b))
  end
  table.insert(values, parseExpression(lineParser, ctx))
  while true do
    lineParser:skipWhitespace()
    local op = lineParser:peek(1)
    if not operators[op] then break end
    lineParser:eat(1)
    local o1 = operators[op]
    while true do
      local top = ops[#ops]
      if operators[top] and ((o1.assoc == "left" and o1.prec <= operators[top].prec) or (o1.assoc == "right" and o1.prec <  operators[top].prec)) then
        applyOp()
      else break end
    end
    table.insert(ops, op)
    table.insert(values, parseExpression(lineParser, ctx))
  end
  while #ops > 0 do
    applyOp()
  end
  return values[1]
end


local function parseAddress(lineParser, ctx)
  lineParser:skipWhitespace()
  lineParser:expect("[")
  local addr = parseMath(lineParser, ctx)
  lineParser:expect("]")
  if addr < 0 or addr > 2 ^ 31 then
    error("Invalid address at line " .. lineParser.lineNum .. ", pos " .. lineParser.pos)
  end
  return
    addr & 0xFF,
    (addr >> 8) & 0xFF,
    (addr >> 16) & 0xFF,
    (addr >> 24) & 0xFF
end

local function parseComma(lineParser)
  lineParser:skipWhitespace()
  lineParser:expect(",")
end

local function oprNone()
  return {}
end

local function oprRegReg(lineParser)
  local reg1 = parseReg(lineParser)
  parseComma(lineParser)
  local reg2 = parseReg(lineParser)
  return { (reg1 << 4) | reg2 }
end

local function oprRegImm(lineParser, ctx)
  local reg = parseReg(lineParser)
  parseComma(lineParser)
  local imm = parseMath(lineParser, ctx)
  return { reg << 4, imm }
end

local function oprRegAdr(lineParser)
  local reg = parseReg(lineParser)
  parseComma(lineParser)
  local a1, a2, a3, a4 = parseAddress(lineParser)
  return { (reg << 4) | 0x0F, a1, a2, a3, a4 }
end

local opcodes = {
  nop = { op = 0x00, operands = oprNone },
  hlt = { op = 0x03, operands = oprNone },
  add = { op = 0x80, operands = oprRegReg },
  sub = { op = 0x81, operands = oprRegReg },
  mul = { op = 0x82, operands = oprRegReg },
  -- div = { op = 0x83, operands = oprRegReg },
  ["and"] = { op = 0x84, operands = oprRegReg },
  ["or"] = { op = 0x85, operands = oprRegReg },
  xor = { op = 0x87, operands = oprRegReg },
  shl = { op = 0x88, operands = oprRegReg },
  shr = { op = 0x89, operands = oprRegReg },
  shra = { op = 0x8A, operands = oprRegReg },
  mod = { op = 0x8C, operands = oprRegReg },
  cpy = { op = 0x91, operands = oprRegReg },
  cpyi = { op = 0xA2, operands = oprRegImm },
  stoa = { op = 0xD0, operands = oprRegAdr },
  loda = { op = 0xD1, operands = oprRegAdr },
}

local function maybeParseOpcode(lineParser, ctx)
  lineParser:skipWhitespace()
  local opcodeStr = parseWord(lineParser, "[a-zA-Z]")
  lineParser:skipWhitespace()
  if opcodes[opcodeStr] then
    local opcode = opcodes[opcodeStr]
    local operands = opcode.operands(lineParser, ctx)
    return { opcode.op, table.unpack(operands) }
  end

  return opcodeStr
end

local psuedoOps = {
  ["repeat"] = function(lineParser, ctx)
    local times = parseMath(lineParser, ctx)
    -- TODO: Should we make sure ctx has the correct relOffset?
    local oneResult = maybeParseOpcode(lineParser, ctx)
    if not oneResult then
      error("Invalid repeat instruction at line " .. lineParser.lineNum .. ", pos " .. lineParser.pos)
    end

    local result = {}
    for i = 1, times do
      for j = 1, #oneResult do
        result[#result + 1] = oneResult[j]
      end
    end

    return result
  end
}

local function parseLine(line, lineNum, ctx)
  local lineParser = newLineParser(lineNum, line)
  lineParser:skipWhitespace()
  if lineParser:isEmpty() or lineParser:peek(1) == ";" then
    return {}
  end

  local result = maybeParseOpcode(lineParser, ctx)
  if type(result) == "string" and psuedoOps[result] then
    result = psuedoOps[result](lineParser, ctx)
  end

  if not result then
    error("Unknown instruction at line " .. lineNum .. ", pos " .. lineParser.pos)
  end

  lineParser:skipWhitespace()
  if not (lineParser:isEmpty() or lineParser:peek(1) == ";") or type(result) ~= "table" then
    error("Unexpected token at line " .. lineNum .. ", pos " .. lineParser.pos)
  end

  return result
end

local function assembleProgram(lines)
  local ctx = { relOffset = 0 }

  local program = {}
  for lineNum, line in ipairs(lines) do
    local bytes = parseLine(line, lineNum, ctx)
    for _, byte in ipairs(bytes) do
      program[#program + 1] = byte
      ctx.relOffset = ctx.relOffset + 1
    end
  end
  return program
end

local function saveProgram(program, format, outputFile)
  local outputFormat = outputFormats[format]
  if not outputFormat then
    error("Unknown output format: " .. format)
  end

  local file = io.open(outputFile, "wb")
  if not file then
    error("Could not open file for writing: " .. outputFile)
  end

  local data = outputFormat.format(program)
  file:write(data)
  file:close()
end

local function main()
  if #args < 2 then
    print("Usage: pisasm <input file> [<output file>] <output format>")
    return
  end

  local inputFile = args[1]
  local outputFile = args[3] and args[2] or nil
  local outputFormat = args[3] and args[3] or args[2]

  local filenameBeforeExt = inputFile:match("(.+)%..+") or inputFile
  if not outputFormats[outputFormat] then
    error("Unknown output format: " .. outputFormat)
  end
  outputFile = outputFile or (filenameBeforeExt .. outputFormats[outputFormat].suffix)

  local file = io.open(inputFile, "r")
  if not file then
    error("Could not open input file: " .. inputFile)
  end

  local lines = {}
  for line in file:lines() do
    lines[#lines + 1] = line
  end
  file:close()

  local program = assembleProgram(lines)
  saveProgram(program, outputFormat, outputFile)
end
main()