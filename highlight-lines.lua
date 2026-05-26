-- Lua filter: wraps specific lines in code blocks with a <span class="hl-add">
-- after Pandoc syntax-highlights them, preserving all colour spans.
-- Usage in chunk: #| highlight-lines: "4-5,13"

local function parse_ranges(s)
  local lines = {}
  for part in s:gmatch("[^,]+") do
    local a, b = part:match("^%s*(%d+)%s*-%s*(%d+)%s*$")
    if a then
      for i = tonumber(a), tonumber(b) do lines[i] = true end
    else
      local n = part:match("^%s*(%d+)%s*$")
      if n then lines[tonumber(n)] = true end
    end
  end
  return lines
end

-- Split highlighted HTML (inside <code>) by newlines, wrapping marked lines.
-- The HTML inside <code> uses \n as line separators between spans.
local function wrap_lines(html, marked)
  local out = {}
  local i = 1
  -- Split on literal newlines
  for line in (html .. "\n"):gmatch("([^\n]*)\n") do
    if marked[i] then
      table.insert(out, '<span class="hl-add">' .. line .. '</span>')
    else
      table.insert(out, line)
    end
    i = i + 1
  end
  -- Remove the extra empty entry from the trailing \n we added
  if out[#out] == "" then table.remove(out) end
  return table.concat(out, "\n")
end

function CodeBlock(el)
  local hl = el.attributes["highlight-lines"]
  if not hl then return nil end

  local marked = parse_ranges(hl)
  el.attributes["highlight-lines"] = nil

  -- Let Pandoc highlight the block normally first by converting to HTML
  local tmp = pandoc.write(pandoc.Pandoc({el}), "html")

  -- Extract the content inside <code ...>...</code>
  local inner = tmp:match("<code[^>]*>(.-)</code>")
  if not inner then return nil end

  local wrapped = wrap_lines(inner, marked)

  -- Rebuild the full block, preserving the outer <div class="sourceCode"><pre ...>
  local rebuilt = tmp:gsub("<code([^>]*)>(.-)</code>", function(attrs, _)
    return "<code" .. attrs .. ">" .. wrapped .. "</code>"
  end, 1)

  return pandoc.RawBlock("html", rebuilt)
end
