#!/usr/bin/env node
/**
 * Limn MCP Server - Node.js JSON-RPC wrapper
 * =========================================================
 * fab mcp | nod jso | prl lim
 * The bridge speaks JSON. The core speaks Limn.
 */

const { spawn } = require('child_process');
const path = require('path');
const readline = require('readline');

const LINTER_DIR = path.join(__dirname, '..', 'linter');

// Tool definitions
const TOOLS = [
  {
    name: "limn_interpret",
    description: "Interpret a Limn sentence. Words define REGIONS of meaning; sentences combine by INTERSECTION. Returns diverse interpretations, or collapsed meanings if a key is provided.",
    inputSchema: {
      type: "object",
      properties: {
        sentence: { type: "string", description: "The Limn sentence (e.g., 'sol liq tra')" },
        key: { type: "string", description: "Optional context key to collapse ambiguity (e.g., 'chemistry', 'emotions')" }
      },
      required: ["sentence"]
    }
  },
  {
    name: "limn_compose",
    description: "Create Limn sentences from English concepts. Returns multiple valid translations with explanations.",
    inputSchema: {
      type: "object",
      properties: {
        concept: { type: "string", description: "English concept to express (e.g., 'hidden truth slowly revealed')" }
      },
      required: ["concept"]
    }
  },
  {
    name: "limn_validate",
    description: "Check if a Limn sentence contains valid vocabulary words.",
    inputSchema: {
      type: "object",
      properties: {
        sentence: { type: "string", description: "The Limn sentence to validate" }
      },
      required: ["sentence"]
    }
  },
  {
    name: "limn_teach",
    description: "Get an interactive Limn teaching lesson.",
    inputSchema: {
      type: "object",
      properties: {
        level: { type: "integer", description: "Lesson level 1-3", default: 1 }
      }
    }
  },
  {
    name: "limn_poetry",
    description: "Generate Limn poetry on a given theme.",
    inputSchema: {
      type: "object",
      properties: {
        theme: { type: "string", description: "Theme for poetry (e.g., 'grief', 'transformation')" }
      },
      required: ["theme"]
    }
  },
  {
    name: "limn_vocabulary",
    description: "Look up Limn words or browse vocabulary by domain.",
    inputSchema: {
      type: "object",
      properties: {
        word: { type: "string", description: "Specific word to look up" },
        domain: { type: "string", description: "Domain to browse (physical, mind, change, etc.)" }
      }
    }
  }
];

// Run Prolog and get result
function runProlog(goal) {
  return new Promise((resolve, reject) => {
    const proc = spawn('scryer-prolog', [
      '-g', `consult('${LINTER_DIR}/limn-vocab.pl'), consult('limn-mcp.pl'), ${goal}, halt`
    ], { cwd: __dirname });

    let stdout = '';
    let stderr = '';

    proc.stdout.on('data', d => stdout += d);
    proc.stderr.on('data', d => stderr += d);
    proc.on('close', code => {
      if (code === 0) resolve(stdout);
      else reject(new Error(stderr || `Exit code ${code}`));
    });
    proc.on('error', reject);
  });
}

// Validate using Prolog linter
async function validateSentence(sentence) {
  try {
    const result = await runProlog(`handle_validate('${sentence.replace(/'/g, "''")}')`);
    const validMatch = result.match(/VALID_COUNT:(\d+)/);
    const invalidMatch = result.match(/INVALID_WORDS:(\[.*?\])/);

    const validCount = validMatch ? parseInt(validMatch[1]) : 0;
    const invalidWords = invalidMatch ? invalidMatch[1] : '[]';

    if (invalidWords === '[]') {
      return `✓ Valid Limn sentence. All ${validCount} words recognized.`;
    } else {
      return `⚠ Unknown words: ${invalidWords}\n\nCheck vocabulary or use similar concepts.`;
    }
  } catch (e) {
    return `Error validating: ${e.message}`;
  }
}

// Handle tool calls
async function handleTool(name, args) {
  switch (name) {
    case 'limn_validate':
      return await validateSentence(args.sentence || '');

    case 'limn_interpret': {
      const sentence = args.sentence || '';
      const key = args.key || '';
      let prompt = `Interpret this Limn sentence: "${sentence}"

Limn is a constructed language where:
- Words define REGIONS of meaning, not single referents
- Sentences combine words by INTERSECTION (where all meanings overlap)
- Word ORDER does not matter (commutative)
- Ambiguity is intentional until collapsed by a KEY

`;
      if (key) {
        prompt += `KEY PROVIDED: "${key}"

Collapse the ambiguity using this key. Provide 2-3 specific interpretations that fit this domain.`;
      } else {
        prompt += `NO KEY PROVIDED - Generate 5 diverse interpretations across different domains.

For each interpretation:
- Name the implicit key/domain
- Give the specific meaning
- Be creative but valid`;
      }
      return prompt;
    }

    case 'limn_compose': {
      const concept = args.concept || '';
      return `Compose a Limn sentence expressing: "${concept}"

Use ONLY valid Limn vocabulary (3-letter words):

PHYSICAL: sol (solid), liq (liquid), gas, hot, col (cold), bri (bright), dim
SPATIAL: abo (above), bel (below), ins (inside), out (outside), nea (near), far
TEMPORAL: now, pas (past), fut (future), beg (begin), end, cyc (cycle)
CHANGE: gro (growth), dec (decay), tra (transform), mov (movement), flo (flow)
LIFE: lif (life), dea (death), hea (health), you (young), old
MIND: thi (think), fee (feel), kno (know), dre (dream), cur (curious)
OPERATORS: nu (not), ve (very), so (somewhat), | (scope boundary)

Rules:
- Use 3-6 words typically
- Use | to separate contrasting concepts
- Be evocative, not explicit
- Provide 2-3 options with explanations`;
    }

    case 'limn_teach': {
      const level = args.level || 1;
      const lessons = {
        1: `## Limn Lesson 1: Basic Intersection

In Limn, words don't have single meanings - they define REGIONS.
When you combine words, their meanings INTERSECT.

**Exercise:** What could \`hot col\` mean?

This seems contradictory - hot AND cold. But think about what exists in that intersection...

Possible answers:
- Lukewarm (the temperature between)
- Thermal contrast (a situation where both exist)
- Temperature shock (rapid transition)
- Ambivalence (metaphorically, mixed feelings)

**Your turn:** What could \`bri dim\` mean?`,

        2: `## Limn Lesson 2: Order Independence

In Limn, word order doesn't matter - we're describing a REGION in meaning-space.

\`sol liq\` = \`liq sol\` (solid + liquid = liquid + solid)

Both mean: ice, glacier, slush, phase boundary

**Exercise:** These are ALL equivalent:
- \`lif gro you\`
- \`you gro lif\`
- \`gro lif you\`

All describe: alive + growing + young = seedling, child, new venture`,

        3: `## Limn Lesson 3: Operators

Operators modify the word that follows them.

\`nu\` = not/negation (applies to NEXT word only)

\`nu sol liq\` = (not-solid) AND liquid = emphasized liquidity
\`sol nu liq\` = solid AND (not-liquid) = dry solid, powder

**Exercise:** What's the difference between:
- \`nu lif gro\` = not-alive growth = machine process, crystal
- \`lif nu gro\` = alive not-growth = mature, stable, stasis`
      };
      return lessons[level] || 'Lessons 1-3 available. Specify level.';
    }

    case 'limn_poetry': {
      const theme = args.theme || 'transformation';
      return `Generate Limn poetry on the theme: "${theme}"

Create 4-6 lines of Limn with English annotations.
Use ONLY valid Limn vocabulary.

Format:
\`\`\`
[line 1 in limn]
[line 2 in limn]
[line 3 in limn]
[line 4 in limn]
\`\`\`

Then annotate each line:
**Line 1:** \`[limn]\` - [word breakdown] - [meaning]
**Line 2:** ...

End with a brief reflection on the poem's journey.`;
    }

    case 'limn_vocabulary': {
      const word = args.word;
      const domain = args.domain;

      if (word) {
        try {
          const result = await runProlog(`handle_vocab_word('${word}')`);
          if (result.includes('FOUND:true')) {
            return `✓ \`${word}\` is valid Limn vocabulary.`;
          } else {
            return `✗ \`${word}\` not found. Check spelling or browse domains.`;
          }
        } catch {
          return `Error looking up word.`;
        }
      } else if (domain) {
        const domains = {
          physical: 'sol liq gas hot col bri dim mag min',
          spatial: 'abo bel ins out nea far bet cen per',
          temporal: 'now pas fut beg end dur cyc mom ete',
          change: 'gro dec tra mov sta flo ris fal',
          life: 'lif dea hea sic you old bir see roo',
          mind: 'thi fee kno bel dou rem ima dre cur',
          communication: 'say lis wor mea amb cle que ans',
          social: 'sel oth fri ene joi sep giv tak',
          values: 'goo bad tru fal val zer one man all'
        };
        return domains[domain]
          ? `## ${domain}\n\n${domains[domain]}`
          : `Available domains: ${Object.keys(domains).join(', ')}`;
      } else {
        return `## Limn Vocabulary

**Domains:** physical, spatial, temporal, change, life, mind, communication, social, values

**Operators:** nu (not), ve (very), so (somewhat), te (tentative), we (weak)
**Quantifiers:** al (all), ex (exists), on (one)
**References:** yo (this), an (that), sa (same)

Use \`word\` param to look up specific words, or \`domain\` to browse.`;
      }
    }

    default:
      throw new Error(`Unknown tool: ${name}`);
  }
}

// Main MCP server loop
const rl = readline.createInterface({ input: process.stdin, output: process.stdout, terminal: false });

rl.on('line', async (line) => {
  try {
    const request = JSON.parse(line);
    const { method, id, params } = request;

    let result;

    switch (method) {
      case 'initialize':
        result = {
          protocolVersion: "2024-11-05",
          capabilities: { tools: {} },
          serverInfo: { name: "limn-mcp", version: "1.0.0" }
        };
        break;

      case 'tools/list':
        result = { tools: TOOLS };
        break;

      case 'tools/call':
        const content = await handleTool(params.name, params.arguments || {});
        result = { content: [{ type: "text", text: content }] };
        break;

      case 'notifications/initialized':
        return; // No response for notifications

      default:
        if (id !== undefined) {
          console.log(JSON.stringify({
            jsonrpc: "2.0",
            id,
            error: { code: -32601, message: `Method not found: ${method}` }
          }));
        }
        return;
    }

    if (id !== undefined) {
      console.log(JSON.stringify({ jsonrpc: "2.0", id, result }));
    }

  } catch (e) {
    console.log(JSON.stringify({
      jsonrpc: "2.0",
      id: null,
      error: { code: -32603, message: e.message }
    }));
  }
});
