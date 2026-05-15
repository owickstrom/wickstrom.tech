import { describe, it, expect, beforeEach } from 'vitest';
import { getSpellCheckableWords } from './wordExtractor.ts';

describe('getSpellCheckableWords', () => {
  let container: HTMLElement;

  beforeEach(() => {
    container = document.createElement('div');
  });

  function words(html: string): Set<string> {
    container.innerHTML = html;
    return getSpellCheckableWords(container);
  }

  describe('plain text', () => {
    it('simple text', () => {
      expect(words('Hello world')).toEqual(new Set(['Hello', 'world']));
    });

    it('multiple words', () => {
      expect(words('Hello world this is a test')).toEqual(
        new Set(['Hello', 'world', 'this', 'is', 'a', 'test'])
      );
    });

    it('collapses multiple spaces', () => {
      expect(words('Hello     world    with    spaces')).toEqual(
        new Set(['Hello', 'world', 'with', 'spaces'])
      );
    });

    it('strips punctuation', () => {
      expect(words('Hello, world! How are you?')).toEqual(
        new Set(['Hello', 'world', 'How', 'are', 'you'])
      );
    });

    it('excludes numbers', () => {
      expect(words('There are 42 cats')).toEqual(
        new Set(['There', 'are', 'cats'])
      );
    });
  });

  describe('contractions and apostrophes', () => {
    it('preserves contractions', () => {
      expect(words("I've been working and I don't think it doesn't work")).toEqual(
        new Set(["I've", 'been', 'working', 'and', 'I', "don't", 'think', 'it', "doesn't", 'work'])
      );
    });

    it("handles possessives", () => {
      expect(words("The cat's pajamas")).toEqual(
        new Set(['The', "cat's", 'pajamas'])
      );
    });

    it('normalizes curly apostrophes', () => {
      expect(words("don\u2019t won\u2018t")).toEqual(
        new Set(["don't", "won't"])
      );
    });

    it('handles HTML entity apostrophes', () => {
      expect(words("can&#39;t shouldn&apos;t")).toEqual(
        new Set(["can't", "shouldn't"])
      );
    });

    it('contraction split across inline elements', () => {
      expect(words("<div>I<span>'</span>ve been working</div>")).toEqual(
        new Set(["I've", 'been', 'working'])
      );
    });
  });

  describe('inline elements', () => {
    it('inline elements do not break words', () => {
      expect(words('<p>Hello <span>world</span> and <strong>goodbye</strong></p>')).toEqual(
        new Set(['Hello', 'world', 'and', 'goodbye'])
      );
    });

    it('deeply nested inline within single block', () => {
      expect(words('<p>A <em>very <strong>deeply</strong> nested</em> phrase</p>')).toEqual(
        new Set(['A', 'very', 'deeply', 'nested', 'phrase'])
      );
    });

    it('adjacent spans do not merge words', () => {
      expect(words('<span>hello</span> <span>world</span>')).toEqual(
        new Set(['hello', 'world'])
      );
    });

    it('adjacent spans without space concatenate text', () => {
      expect(words('<span>hel</span><span>lo</span>')).toEqual(
        new Set(['hello'])
      );
    });
  });

  describe('block elements', () => {
    it('adjacent paragraphs produce separate words', () => {
      expect(words('<p>First</p><p>Second</p>')).toEqual(
        new Set(['First', 'Second'])
      );
    });

    it('adjacent divs produce separate words', () => {
      expect(words('<div>hello</div><div>world</div>')).toEqual(
        new Set(['hello', 'world'])
      );
    });

    it('header followed by paragraph', () => {
      expect(words('<h1>Title</h1><p>Body text</p>')).toEqual(
        new Set(['Title', 'Body', 'text'])
      );
    });

    it('list items', () => {
      expect(words('<ul><li>apple</li><li>banana</li><li>cherry</li></ul>')).toEqual(
        new Set(['apple', 'banana', 'cherry'])
      );
    });

    it('table cells', () => {
      expect(words('<table><tr><td>one</td><td>two</td></tr></table>')).toEqual(
        new Set(['one', 'two'])
      );
    });

    it('br does not merge adjacent words', () => {
      expect(words('hello<br>world')).toEqual(
        new Set(['hello', 'world'])
      );
    });

    it('complex nested structure with blocks and inline', () => {
      const html =
        '<header>\n' +
        '<h1 class="title">Specifying State Machines with Temporal Logic</h1>\n' +
        '<p class="author">Oskar Wickström</p>\n' +
        '<p class="date">May 05, 2021</p>\n' +
        '</header>\n';
      expect(words(html)).toEqual(
        new Set(['Specifying', 'State', 'Machines', 'with', 'Temporal', 'Logic', 'Oskar', 'Wickström', 'May'])
      );
    });
  });

  describe('ignored elements', () => {
    it('ignores script content', () => {
      expect(words('<div>Hello <script>var x = 1;</script> world</div>')).toEqual(
        new Set(['Hello', 'world'])
      );
    });

    it('ignores style content', () => {
      expect(words('<div>Hello <style>.foo { color: red; }</style> world</div>')).toEqual(
        new Set(['Hello', 'world'])
      );
    });

    it('ignores pre content', () => {
      expect(words('<div>Hello <pre>some code here</pre> world</div>')).toEqual(
        new Set(['Hello', 'world'])
      );
    });

    it('ignores code content', () => {
      expect(words('<div>Hello <code>someFunction()</code> world</div>')).toEqual(
        new Set(['Hello', 'world'])
      );
    });

    it('ignores noscript content', () => {
      expect(words('<div>Hello <noscript>Enable JS</noscript> world</div>')).toEqual(
        new Set(['Hello', 'world'])
      );
    });

    it('ignores svg content', () => {
      expect(words('<div>Hello <svg><text>SVG text</text></svg> world</div>')).toEqual(
        new Set(['Hello', 'world'])
      );
    });
  });

  describe('spellcheck attribute', () => {
    it('skips elements with spellcheck="false"', () => {
      expect(words('<div>Hello <span spellcheck="false">ignored</span> world</div>')).toEqual(
        new Set(['Hello', 'world'])
      );
    });

    it('includes elements with spellcheck="true"', () => {
      expect(words('<div>Hello <span spellcheck="true">included</span> world</div>')).toEqual(
        new Set(['Hello', 'included', 'world'])
      );
    });

    it('skips nested content within spellcheck="false"', () => {
      expect(words('<div>Hello <div spellcheck="false"><p>all <em>of</em> this</p></div> world</div>')).toEqual(
        new Set(['Hello', 'world'])
      );
    });
  });

  describe('edge cases', () => {
    it('empty input', () => {
      expect(words('')).toEqual(new Set());
    });

    it('whitespace only', () => {
      expect(words('   ')).toEqual(new Set());
    });

    it('empty elements', () => {
      expect(words('<div><span></span><p></p></div>')).toEqual(new Set());
    });

    it('empty elements mixed with text', () => {
      expect(words('<div><span></span><p>Text</p><span></span></div>')).toEqual(
        new Set(['Text'])
      );
    });

    it('unicode letters are preserved', () => {
      expect(words('café résumé naïve')).toEqual(
        new Set(['café', 'résumé', 'naïve'])
      );
    });

    it('mixed languages', () => {
      expect(words('Hello Wörld über')).toEqual(
        new Set(['Hello', 'Wörld', 'über'])
      );
    });

    it('excludes acronyms starting with 2+ uppercase letters', () => {
      expect(words('The CLI and VSCode use LLMs')).toEqual(
        new Set(['The', 'and', 'use'])
      );
    });

    it('keeps words with single leading uppercase', () => {
      expect(words('Anthropic makes Haskell tools')).toEqual(
        new Set(['Anthropic', 'makes', 'Haskell', 'tools'])
      );
    });

    it('excludes camelCase identifiers', () => {
      expect(words('Use mapM and mkItem for foldable types')).toEqual(
        new Set(['Use', 'and', 'for', 'foldable', 'types'])
      );
    });

    it('excludes words with no vowels (abbreviations)', () => {
      expect(words('The ctx and rhs of the fn')).toEqual(
        new Set(['The', 'and', 'of', 'the'])
      );
    });
  });

  describe('URLs and domains', () => {
    it('ignores plain text URLs', () => {
      expect(words('Visit https://example.com/path for info')).toEqual(
        new Set(['Visit', 'for', 'info'])
      );
    });

    it('ignores bare domains in text', () => {
      expect(words('Check example.com for details')).toEqual(
        new Set(['Check', 'for', 'details'])
      );
    });

    it('ignores domains with ccTLD', () => {
      expect(words('Read Lobste.rs for news')).toEqual(
        new Set(['Read', 'for', 'news'])
      );
    });

    it('ignores domains in link text', () => {
      expect(words('<a href="https://lobste.rs">Lobste.rs</a> is great')).toEqual(
        new Set(['is', 'great'])
      );
    });

    it('keeps link text that is descriptive', () => {
      expect(words('<a href="https://example.com">Read more here</a> about it')).toEqual(
        new Set(['Read', 'more', 'here', 'about', 'it'])
      );
    });

    it('ignores domains with multi-part ccTLDs', () => {
      expect(words('Visit teh.id.au for info')).toEqual(
        new Set(['Visit', 'for', 'info'])
      );
    });

    it('ignores domains with tech TLDs', () => {
      expect(words('Deploy to render.io or fly.io')).toEqual(
        new Set(['Deploy', 'to', 'or'])
      );
    });
  });
});
