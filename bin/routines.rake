#!/bin/env ruby

#
# Put some color to the screen
#

class String
  @@w = "[0m"
  @@r = "[0;31;40m"
  @@g = "[0;32;40m"
  @@y = "[0;33;40m"
  @@b = "[0;34;40m"

  def red   ;  return (@@r + self + @@w);end
  def green ;  return (@@g + self + @@w);end
  def yellow ; return (@@y + self + @@w);end
  def blue   ; return (@@b + self + @@w);end
end

# End
################################################################################
