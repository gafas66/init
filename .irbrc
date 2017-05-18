# -*- ruby -*-

# List non-trivial methods on any class
class Object
  # Return only the methods not present on basic objects
  def im
    (self.methods - Object.new.methods).sort
  end
end
 
#-- For Summable
# Add Summable functionality to Arrays and Ranges
module Summable
  def sum
    inject(0) { |x, y| x + y }
  end
end

class Array
  include Summable
end
 
class Range
  include Summable
end
 
#-- Simplify converting numbers to hex
module Hexable
  def hex
    '0x' + to_s(16)
  end
end
 
class Fixnum
  include Hexable
end
 
class Bignum
  include Hexable
end
 
# pretty print --> provides a "pretty" view of an object
require 'pp'

# Load our methods.rb
def l ;load "~/.irbrc";end

$root = "/net/thebe/space/local_scratch/ekofoed/trunk_regression/asic"
def mfiles
  Dir.glob("#{$root}/libs/*/syn/SConscript2").delete_if {|f| open(f).grep(/DESIGNER/).size==0}
end
def active
  mfiles.reject {|f| open(f).grep(/wactive/).size>0}
end
def designer(f)
  open(f).grep(/DESIGNER */)[0] =~ /DESIGNER *(.*)\..*@.*/
  name = $1
  f =~ %r{.*/libs/(.*)/syn/.*}
  return "#{name} :: #{$1}"
end
def list
  mfiles.each {|f| puts designer f}
end

# End
################################################################################
