import tlds from 'tlds';

const TLDS = new Set(tlds);

/**
 * Collects spell-checkable text from a DOM tree using stack-based traversal.
 * Ignores script/style/pre elements and elements with spellcheck="false".
 * Returns a set of unique words without punctuation.
 */
export function getSpellCheckableWords(root: HTMLElement): Set<string> {
  // Elements that should never be spell-checked
  const IGNORED_TAGS = new Set([
    'SCRIPT',
    'STYLE',
    'NOSCRIPT',
    'PRE',
    'CODE',
    'SAMP',
    'KBD',
    'VAR',
    'SVG',
    'MATH',
    'IFRAME',
    'CANVAS',
    'AUDIO',
    'VIDEO',
    'HEAD',
    'TITLE',
    'META',
    'LINK',
    'BASE',
    'TEMPLATE',
    'OBJECT',
    'EMBED',
    'AREA',
    'MAP',
  ]);

  // Block-level elements that imply word boundaries
  const BLOCK_TAGS = new Set([
    'ADDRESS', 'ARTICLE', 'ASIDE', 'BLOCKQUOTE', 'DD', 'DETAILS',
    'DIALOG', 'DIV', 'DL', 'DT', 'FIELDSET', 'FIGCAPTION', 'FIGURE',
    'FOOTER', 'FORM', 'H1', 'H2', 'H3', 'H4', 'H5', 'H6', 'HEADER',
    'HGROUP', 'HR', 'LI', 'MAIN', 'NAV', 'OL', 'P', 'PRE', 'SECTION',
    'TABLE', 'UL', 'BR', 'TR', 'TD', 'TH', 'CAPTION', 'THEAD', 'TBODY',
    'TFOOT', 'SUMMARY',
  ]);

  const view = root.ownerDocument.defaultView;

  const stack: Node[] = [root];
  const textParts: string[] = [];

  while (stack.length > 0) {
    const node = stack.pop();

    if (!node) {
      break;
    }

    // Check if this element should be skipped
    // Use nodeType instead of instanceof Element for cross-realm safety
    if (node.nodeType === Node.ELEMENT_NODE) {
      const element = node as Element;
      // Skip if spellcheck attribute is explicitly set to "false"
      const spellcheck = element.getAttribute('spellcheck');
      if (spellcheck === 'false') {
        continue;
      }

      // Skip ignored element types
      // Use toUpperCase() because SVG/MathML elements have lowercase tagName
      const tag = element.tagName.toUpperCase();
      if (IGNORED_TAGS.has(tag)) {
        continue;
      }

      // Check if element creates a block boundary using computed style,
      // falling back to tag-based check in environments without layout
      let isBlock = BLOCK_TAGS.has(tag);
      if (view) {
        const display = view.getComputedStyle(element).display;
        if (display) {
          isBlock = !display.startsWith('inline') && display !== 'contents' && display !== 'none';
        }
      }

      if (isBlock) {
        textParts.push(' ');
      }
    }

    // Collect text from text nodes
    if (node.nodeType === Node.TEXT_NODE) {
      const text = node.textContent || '';
      if (text) {
        textParts.push(text);
      }
    }

    // Push children onto stack in reverse order to maintain document order
    const children = Array.from(node.childNodes);
    for (let i = children.length - 1; i >= 0; i--) {
      const child = children[i];
      if (child) {
        stack.push(child);
      }
    }
  }

  const fullText = textParts.join('');

  // Collapse multiple whitespace characters to single space
  // This matches how browsers handle whitespace in rendered text
  const collapsed = fullText.replace(/\s+/g, ' ');

  return extractWords(collapsed);
}

function extractWords(text: string): Set<string> {
  // Decode HTML entities for apostrophes (&#39;, &apos;, &#8217;, etc.)
  let decoded = text
    .replace(/&#39;/g, "'")
    .replace(/&apos;/g, "'")
    .replace(/&#8217;/g, "'")  // right single quotation mark
    .replace(/&#8216;/g, "'")  // left single quotation mark
    .replace(/&rsquo;/g, "'")
    .replace(/&lsquo;/g, "'");

  // Normalize all apostrophe variants to ASCII apostrophe
  const normalized = decoded.replace(/[\u2018\u2019\u02BC\u2032`\u00B4]/g, "'");

  // Remove tokens that look like URLs or domains (word.tld)
  const noUrls = normalized.replace(/\S+/g, token => {
    // Explicit URLs
    if (/^https?:\/\//i.test(token)) return ' ';
    // Tokens containing .tld (check all dot-separated parts for multi-part TLDs like .id.au)
    for (const match of token.matchAll(/\.([a-z]+)/gi)) {
      const part = match[1];
      if (part && TLDS.has(part.toLowerCase())) return ' ';
    }
    return token;
  });

  // Split on various non-letter chars
  const words = noUrls.split(/[^\p{L}']+/u).filter(w => w.length > 0);

  return new Set(
    words
      .map(word => word.replace(/^'+|'+$/g, ''))  // strip leading/trailing apostrophes
      .filter(word => {
        // Exclude empty strings
        if (word === "") return false;
        // Exclude numbers
        if (!isNaN(Number(word))) return false;
        // Exclude acronyms (words starting with 2+ uppercase letters)
        if (/^[A-Z]{2}/.test(word)) return false;
        // Exclude camelCase identifiers (lowercase followed by uppercase)
        if (/[a-z][A-Z]/.test(word)) return false;
        // Exclude words that are all lowercase with no vowels (likely abbreviations)
        if (/^[a-z]+$/.test(word) && !/[aeiouy]/i.test(word)) return false;
        return true;
      })
  );
}
