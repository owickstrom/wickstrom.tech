require 'tempfile'
require 'base64'
require 'digest'
require 'fileutils'

module Jekyll
  class DiagramBlock < Liquid::Block
    def initialize(tag_name, attrs_str, tokens)
      super
      attrs = eval("{#{attrs_str}}")
      @width = attrs[:width].to_i
      @height = attrs[:height].nil? ? nil : attrs[:height].to_i
      @caption = attrs[:caption].strip
    end

    def render_diagram(content, out_dir)
      Dir.mkdir(out_dir) unless Dir.exists?(out_dir)

      in_f = Tempfile.new(["diagram", ".hs"])
      in_f.write(<<-MARKUP.strip
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
MARKUP
      )
      in_f.write(content)
      in_f.write("\n\nmain = mainWith dia\n")
      in_f.close()

      tmp_out_f = Tempfile.new(["diagram", ".svg"])

      height_param = @height.nil? ? "" : "-h #{@height}"
      %x[cabal v2-build wickstrom-tech]
      %x[cabal v2-exec -- runhaskell #{in_f.path} -w #{@width} #{height_param} -o #{tmp_out_f.path}]
      unless File.exists?(tmp_out_f)
        raise "Failed to generate diagram!"
      end

      out_name = Digest::SHA256.hexdigest(IO.read(tmp_out_f)) + ".svg"
      out_file = File.join(out_dir, out_name)
      FileUtils.mv(tmp_out_f, out_file)

      in_f.unlink
      
      return out_name
    end

    def render(context)
      site = context.registers[:site]
      page = context.registers[:page]

      out_dir = "#{site.source}/generated/diagrams"
      name = render_diagram(super, out_dir)

      site.static_files << Jekyll::StaticFile.new(site, site.source, "/generated/diagrams", name)

      url = "#{site.baseurl}/generated/diagrams/#{name}"

      height_attr = @height.nil? ? "" : "height=\"#{@height}\""

      <<-MARKUP.strip
  <figure class="diagram">
  <img src="#{url}" alt="#{@caption}" width="#{@width}" #{height_attr} />
  <figcaption>
  #{@caption}
  </figcaption>
  </figure>
      MARKUP
    end
  end
end

Liquid::Template.register_tag('diagram', Jekyll::DiagramBlock)
