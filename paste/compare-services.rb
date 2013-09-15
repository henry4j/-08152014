#!/usr/bin/env ruby
require 'optparse'

options = {}
OptionParser.new do |opts|
  opts.on('-b', '--baseline BASELINE-SERVICES.TXT', 'baseline') { |v| options[:baseline] = v }
  opts.on('-c', '--comparand COMPARAND-SERVICES.TXT', 'comparand') { |v| options[:comparand] = v }
  opts.on('-a', '--annotation ANNOTATION.TXT', 'annotation') { |v| options[:annotation] = v }
end.parse!

baseline_services = options[:baseline] || 'baseline-services.txt'
comparand_services = options[:comparand] || 'comparand-services.txt'
annotations = options[:annotation] || 'annotations.txt'

services_annotated = open(annotations) { |f| f.readlines }.map { |l| l.split(':')[0].strip }
open('services-annotated.txt', 'w') { |f| services_annotated.each { |l| f.puts(l) } }

`grep -x -v -f #{baseline_services} #{comparand_services} > extra-services.txt`
`grep -w -v -f services-annotated.txt extra-services.txt > extra-services-unannotated.txt`
`grep -w -f extra-services.txt services-annotated.txt > extra-services-annotated.txt`

# extra_services_in_annotation = []
# open('extra-services-annotated.txt').each do |l| 
#   extra_services_in_annotation << `grep -e "^#{l.strip}\\b" #{annotations}`
# end
# open('extra-services-in-annotation.txt', 'w') { |f| f.puts(extra_services_in_annotation) }

`rm 'extra-services-in-annotation.txt'`
open('extra-services-annotated.txt').each do |l| 
  `grep -e "^#{l.strip}\\b" #{annotations} >> 'extra-services-in-annotation.txt'`
end

