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

local function startsWord(lineParser, regex)
  return lineParser:peek(1):match(regex)
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
  ["/"] = {prec = 2, assoc = "left", fn = function(a, b) return a // b end},
}

local parseMath
local function parseExpression(lineParser)
  lineParser:skipWhitespace()
  if lineParser:peek(1) == "(" then
    lineParser:eat(1)
    local result = parseMath(lineParser)
    lineParser:expect(")")
    return result
  elseif lineParser:peek(1) == "@" then
    lineParser:eat(1)
    return function(ctx) return ctx.relOffset end
  elseif startsWord(lineParser, "[a-zA-Z]") then
    local word = parseWord(lineParser, "[a-zA-Z]")
    return function(ctx)
      local label = ctx.labels[word]
      if not label and not ctx.firstParse then
        error("Unknown label " .. word .. " at line " .. lineParser.lineNum .. ", pos " .. lineParser.pos)
      end
      if label and ctx.relAddr then
        return label - ctx.relAddr
      end
      return label or 0
    end
  else
    return parseNumber(lineParser)
  end
end

parseMath = function(lineParser)
  local values = {}
  local ops = {}
  local function applyOp()
    local op = table.remove(ops)
    local b = table.remove(values)
    local a = table.remove(values)
    table.insert(values, { fn = operators[op].fn, a = a, b = b})
  end
  table.insert(values, parseExpression(lineParser))
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
    table.insert(values, parseExpression(lineParser))
  end
  while #ops > 0 do
    applyOp()
  end
  return values[1]
end


local function parseAddress(lineParser)
  lineParser:skipWhitespace()
  lineParser:expect("[")
  local addr = parseMath(lineParser)
  lineParser:expect("]")
  return addr
end

local function parseComma(lineParser)
  lineParser:skipWhitespace()
  lineParser:expect(",")
end

local function oprNone()
  return {}
end

local function oprImm(lineParser)
  local imm = parseMath(lineParser)
  return { imm = imm }
end

local function oprRegReg(lineParser)
  local reg1 = parseReg(lineParser)
  parseComma(lineParser)
  local reg2 = parseReg(lineParser)
  return { regreg = (reg1 << 4) | reg2 }
end

local function oprRegImm(lineParser)
  local reg = parseReg(lineParser)
  parseComma(lineParser)
  local imm = parseMath(lineParser)
  return {regreg = reg << 4, imm = imm }
end

local function oprRegAddr(lineParser)
  local reg = parseReg(lineParser)
  parseComma(lineParser)
  local addr = parseAddress(lineParser)
  return { regreg = (reg << 4) | 0x0F, addr = addr }
end

local function oprAddrVReg(lineParser)
  local addr = parseAddress(lineParser)
  parseComma(lineParser)
  local reg = parseReg(lineParser)
  return { regreg = (reg << 4) | 0x0F, addr = addr }
end

local function oprAddr(lineParser)
  local addr = parseMath(lineParser)
  return { addr = addr }
end

local opcodes = {
  nop = { op = 0x00, operands = oprNone },
  hlt = { op = 0x03, operands = oprNone },
  jmpr = { op = 0x20, operands = oprImm },
  jer = { op = 0x22, operands = oprImm },
  jner = { op = 0x23, operands = oprImm },
  jl = { op = 0x24, operands = oprImm },
  jler = { op = 0x25, operands = oprImm },
  jmpa = { op = 0x40, operands = oprAddr },
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
  --mod = { op = 0x8C, operands = oprRegReg },
  cpy = { op = 0x90, operands = oprRegReg },
  addi = { op = 0xA0, operands = oprRegImm },
  subi = { op = 0xA1, operands = oprRegImm },
  cpyi = { op = 0xB0, operands = oprRegImm },
  stoa = { op = 0xD0, operands = oprAddrVReg },
  loda = { op = 0xD1, operands = oprRegAddr },
}

local function maybeParseOpcode(lineParser)
  lineParser:skipWhitespace()
  local opcodeStr = parseWord(lineParser, "[a-zA-Z]")
  lineParser:skipWhitespace()
  if opcodes[opcodeStr] then
    local opcode = opcodes[opcodeStr]
    local virtOp = opcode.operands(lineParser)
    virtOp.op = opcode.op
    return { virtOp }
  end

  return opcodeStr
end

local psuedoOps = {
  ["repeat"] = function(lineParser)
    local times = parseMath(lineParser)
    local oneResult = maybeParseOpcode(lineParser)
    if not oneResult then
      error("Invalid repeat instruction at line " .. lineParser.lineNum .. ", pos " .. lineParser.pos)
    end

    return {{
      special = "repeat",
      times = times,
      virtOp = oneResult,
    }}
  end
}

local function parseLine(line, lineNum)
  local lineParser = newLineParser(lineNum, line)
  lineParser:skipWhitespace()
  if lineParser:isEmpty() or lineParser:peek(1) == ";" then
    return {}
  end

  local result = maybeParseOpcode(lineParser)
  if type(result) == "string" and lineParser:peek(1) == ":" then
    lineParser:eat(1)
    lineParser:skipWhitespace()
    if lineParser:isEmpty() or lineParser:peek(1) == ";" then
      return { { special = "label", label = result } }
    end
  end
  if type(result) == "string" and psuedoOps[result] then
    result = psuedoOps[result](lineParser)
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

local function parseInstructions(lines)
  local program = {}
  for lineNum, line in ipairs(lines) do
    local virtOps = parseLine(line, lineNum)
    for _, virtOp in ipairs(virtOps) do
      program[#program + 1] = virtOp
    end
  end
  return program
end

local function evaluateExpression(expr, ctx)
  if type(expr) == "number" then
    return expr
  elseif type(expr) == "function" then
    return expr(ctx)
  elseif type(expr) == "table" and expr.fn then
    local a = evaluateExpression(expr.a, ctx)
    local b = evaluateExpression(expr.b, ctx)
    return expr.fn(a, b)
  end
  error("Invalid expression")
end

local function addAssembledBytes(assembled, virtOps, ctx)
  for _, virtOp in ipairs(virtOps) do
    if virtOp.special == "repeat" then
      local times = evaluateExpression(virtOp.times, ctx)
      for i = 1, times do
        addAssembledBytes(assembled, virtOp.virtOp, ctx)
      end
    elseif virtOp.special == "label" then
      ctx.labels[virtOp.label] = ctx.relOffset
    elseif virtOp.op then
      -- TODO: Bounds check evaluated values
      local postRelOffset = ctx.relOffset + 1
        + (virtOp.imm and 1 or 0)
        + (virtOp.regreg and 1 or 0)
        + (virtOp.addr and 4 or 0)
      assembled[#assembled + 1] = virtOp.op
      if virtOp.imm then
        local tCtx = setmetatable({ relAddr = postRelOffset }, { __index = ctx })
        assembled[#assembled + 1] = evaluateExpression(virtOp.imm, tCtx) % 256
      end
      if virtOp.regreg then
        assembled[#assembled + 1] = virtOp.regreg
      end
      if virtOp.addr then
        local addr = evaluateExpression(virtOp.addr, ctx)
        assembled[#assembled + 1] = addr & 0xFF
        assembled[#assembled + 1] = (addr >> 8) & 0xFF
        assembled[#assembled + 1] = (addr >> 16) & 0xFF
        assembled[#assembled + 1] = (addr >> 24) & 0xFF
      end
      ctx.relOffset = postRelOffset
    else
      error("Encountered unknown instruction")
    end
  end
end

local function resetCtx(ctx)
  ctx.relOffset = 0
  ctx.firstParse = false
end

local function bytesEqual(a, b)
  if #a ~= #b then return false end
  for i = 1, #a do
    if a[i] ~= b[i] then return false end
  end
  return true
end

local function assembleProgram(lines)
  local ctx = {
    relOffset = 0,
    firstParse = true,
    labels = {}
  }
  local virtOps = parseInstructions(lines)
  local assembled, prevAssembled = {}, {}
  addAssembledBytes(assembled, virtOps, ctx)
  resetCtx(ctx)
  while not bytesEqual(assembled, prevAssembled) do
    prevAssembled = assembled
    assembled = {}
    addAssembledBytes(assembled, virtOps, ctx)
    resetCtx(ctx)
  end

  return assembled
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