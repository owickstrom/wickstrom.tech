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

    def render_diagram(content, scale, out_dir)
      Dir.mkdir(out_dir) unless Dir.exists?(out_dir)

      # Create a temporary file to hold a common prefix for Haskell
      # diagram modules, followed by the diagram block contents.
      in_f = Tempfile.new(["diagram", ".hs"])
      in_f.write(<<-MARKUP.strip
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
MARKUP
      )
      in_f.write(content)
      in_f.write("\n\nmain = mainWith dia\n")
      in_f.close()

      # Create the final hash file name out of the library hash,
      # diagram contents, width, and height.
      hash_key = Digest::SHA256.hexdigest(
        IO.read(in_f) +
        @width.to_s +
        @height.to_s)
      out_name = hash_key + ".#{scale}x.png"
      out_file = File.join(out_dir, out_name)

      # Only generate the file in case it doesn't already exist (in
      # which case nothing should have changed).
      unless File.exists?(out_file)
        height_param = @height.nil? ? "" : "-h #{@height * scale}"
        puts "Generating #{out_name}"
        %x[runhaskell #{in_f.path} -w #{@width * scale} #{height_param} -o #{out_file}]
        unless File.exists?(out_file)
          raise "Failed to generate diagram!"
        end
      end

      # Remove the temporary file containing diagram code, used
      # only during compilation.
      in_f.unlink
      
      return out_name
    end

    def render(context)
      site = context.registers[:site]
      page = context.registers[:page]

      out_dir = "#{site.source}/generated/diagrams"
      name_1x = render_diagram(super, 1, out_dir)
      name_2x = render_diagram(super, 2, out_dir)

      site.static_files << Jekyll::StaticFile.new(site, site.source, "/generated/diagrams", name_1x)
      site.static_files << Jekyll::StaticFile.new(site, site.source, "/generated/diagrams", name_2x)

      url_1x = "#{site.baseurl}/generated/diagrams/#{name_1x}"
      url_2x = "#{site.baseurl}/generated/diagrams/#{name_2x}"

      height_attr = @height.nil? ? "" : "height=\"#{@height}\""

      <<-MARKUP.strip
  <figure class="diagram">
  <img src="#{url_1x}" srcset="#{url_1x} 1x, #{url_2x} 2x" alt="#{@caption}" width="#{@width}" #{height_attr} />
  <figcaption>
  #{@caption}
  </figcaption>
  </figure>
      MARKUP
    end
  end
end

Liquid::Template.register_tag('diagram', Jekyll::DiagramBlock)
