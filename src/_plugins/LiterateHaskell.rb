require 'open3'

module Jekyll
  class LiterateHaskell < Converter
    safe true
    priority :high

    def matches(ext)
      ext =~ /^\.lhs$/i
    end

    def output_ext(ext)
      ".html"
    end

    def convert(content)
      stdout, stderr, status = Open3.capture3("pandoc -f markdown+lhs -t html5 --smart --base-header-level=2", :stdin_data => content)
      raise "Failed to convert Literate Haskell source file to HTML: " + stderr unless status == 0
      stdout
    end
  end
end
