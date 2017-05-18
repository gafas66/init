#!/bin/sh -e
# the next line restarts using wish \
exec `which wish | tail -1` "$0" "$@"

## # 
## # some helpfull routines
## # 
proc max {a b} {if {$a > $b} {return $a} else {return $b}}
proc min {a b} {if {$a < $b} {return $a} else {return $b}}
proc decr {i_ {by 1}} {upvar $i_ i; set i [expr $i-$by]}
proc swap {a_ b_} {
  upvar $a_ a; upvar $b_ b; set t $a; set a $b; set b $t
}
## # view array contends
proc arview {anarray} {
  upvar $anarray myar
  foreach name [lsort -dictionary [array names myar]] {
    puts stderr [format "%s.%-20s : %s" $anarray $name $myar($name)]
  }
}
########################################################
## # read commandline, read environment variable
## #
proc gui_help {} {
  global var
  set dummy [tk_messageBox -type ok -message $var(help) -icon info]  
}
proc cmd_help {} {
  puts stderr "\nTCL/TK driven path viewer and editor\nusage:"
  puts stderr "[info script]"
  puts stderr "or:"
  puts stderr "[info script] ENIVIRONMENT-VARIABLE"
  puts stderr "or:"
  puts stderr "[info script] 'partial-name-with*'"
  puts stderr "\nPlease note, no leading '\$' sign is required."
}

proc commandline {} {
  global var env argc argv verbose
  if {$argc > 0} {
    set var(env) [lindex $argv 0]
  } else {
    set var(env) PATH
  }  
  if {$argc > 1} {
    if {[lindex $argv 1] == {-verbose}} {set verbose 1}
  }
  # expand if it contains a wildcard
  if {[regexp {\*} $var(env)]} {
    foreach name [array names env] {
      if {[string match $var(env) $name]} {
        set var(env) $name
        break
      }
    }
  }
  # no match / bad argument -> exit
  if {![info exists env($var(env))]} {cmd_help; exit 1}

  set var(orig) $env($var(env))

  set var(delim) { }
  if {[regexp {\:} $var(orig)]} {set var(delim) {:}}

  return [split $var(orig) $var(delim)]
}
## #
## # analyzing a path entry for hdms style version number 
## # 

# line : as in path entry
# vers : version available (if any)
# cur  : current version (if any)
# form : format string containing %s instead version# or defaulting to 'line'
proc path2vers {line vers_ cur_ form_} {
  global var verbose
  upvar $vers_ vers
  upvar $cur_ cur
  upvar $form_ form
  set vers [list ]
  set form $line; # allow consitend approach btween versioned and unversioned
  set cur {nil}
  set dd [file split $line]; set le [llength $dd]
  set tp {}; set ti 0
  foreach d $dd {
    set f 0
    if {[regexp {^v[0-9]+\.[0-9]+} $d]} {break}
    if {$d == {dev}} {break}
    if {$d == {current}} {break}
    append tp $d; append tp {/}
    set f 1; incr ti
  }
  if {$f} {return 0}
  set cur $d
  set form {}
  regsub {//} $tp {/} form
  append form "%s"
  for {set i [expr $ti+1]} {$i < $le} {incr i} {
    append form "/"
    append form [lindex $dd $i]
  }
  set fulls [glob -nocomplain $tp*]
  foreach full $fulls {
    set t [file tail $full]
    if {[regexp {^v[0-9]+\.[0-9]+$} $t]} {lappend vers $t}
    #if {[regexp {^dev$} $t]} {lappend vers $t}
    #if {[regexp {^current$} $t]} {lappend vers $t}
  }
  set vers [lsort -decreasing -dictionary $vers]
  set vers [lrange $vers 0 $var(maxvers)]
  if {$verbose} {
    puts stderr [format "%s with versions %s (current : %s)" $form $vers $cur]
  }
  return 1
}
## #
## # output routines
## # 

## # get the actial path entry (form to plain)
proc get_entry {i} {
  global ver
  if {$ver(c.$i) == {nil}} {
    set str $ver(f.$i)
  } else {
    set str [format $ver(f.$i) $ver(s.$i)]
  }
  return $str
}

## # print variable as setenv command
proc print_cmd {} {
  global ver var
  puts "$ver(cnt)"
  if {$var(delim) == ":"} {
    set str "setenv "
    append str $var(env)
    append str " "
  } else {
    set str "set "
    append str $var(env)
    append str " = "
  }
  set f 0
  for {set i 0} {$i < $ver(cnt)} {incr i} {
    if {!$ver(o.$i)} {continue}
    if {$f != 0} {append str $var(delim)}
    set f 1
    append str [get_entry $i]
  }
  puts $str; flush stdout
}
## # print variable as human readable list
proc list_cmd {} {
  global ver var
  puts "$var(env) contains $ver(cnt) elements:"
  set j 1
  for {set i 0} {$i < $ver(cnt)} {incr i} {
    if {!$ver(o.$i)} {continue}
    puts [format "%3d : %s" $j [get_entry $i]]
    incr j
  }
  flush stdout
}

## # 
## # GUI (build, move and add items)
## # 

## # check the entry exists, color the background
proc path_check {n} {
  global verbose ver var
  if {$verbose} {puts "checking $n"}
  if {$ver(o.$n)} {
    set where [get_entry $n]
    if {[file isdirectory $where]} {
      .en_${n} configure -bg green
    } else {
      .en_${n} configure -bg red
    }
  } else {
    .en_${n} configure -bg grey
  }
  path_complete_showme_not
}
## # file name completion
proc path_unflash {} {
  global var
  .en_$var(unflash.n) configure -bg $var(unflash.col)
}
proc path_flash {n color} {
  global var ver
  set var(unflash.col) [.en_${n} cget -bg]
  set var(unflash.n) $n
  .en_${n} configure -bg $color
  after 100 path_unflash
}
proc path_complete_showme_not {} {
  global var
  set var(complete) nil
}
proc path_complete_showme {items} {
  set txt ""
  foreach item $items {
    append txt [file tail $item]; append txt " "
  }
  set dummy [tk_messageBox -type ok -message $txt -icon info]  
  path_complete_showme_not
}
proc path_complete {n} {
  global verbose ver var
  if {$verbose} {puts "completing $n"}
  set where [get_entry $n]
  set seek $where; append seek "*"
  set items [glob  -type d -nocomplain $seek]
  if {$items == {}} {path_flash $n black; return}
  if {[llength $items] == 1} {
    if {$ver(f.$n) == $items} {
      append ver(f.$n) "/"
    } else {
      set ver(f.$n) $items
    }
    .en_${n} icursor end
  } else {
    if {$ver(f.$n) == $var(complete)} {
      path_complete_showme $items      
      return
    }
    set var(complete) $ver(f.$n)
    path_flash $n white
    after 300 path_complete_showme_not
  }
}
## # enable move when focus enters an entry
proc take_focus {n} {
  global verbose
  if {$verbose} {puts "enter $n"}
  global current f
  set current $n
  $f.top configure -state normal
  $f.up  configure -state normal
  $f.dn  configure -state normal
  $f.btm configure -state normal
}
## # disable move when focus leaves an entry
proc leave_focus {n} {
  global verbose
  if {$verbose} {puts "leave $n"}
  global current f
  set current nil
  $f.top configure -state disabled
  $f.up  configure -state disabled
  $f.dn  configure -state disabled
  $f.btm configure -state disabled
  path_check $n
}
## # move (actually swap) an entry with another distanced 'by' n rows 
proc moveentry {by} {
  global current ver verbose
  if {$current == {nil}} {return}
  set from $current
  set to   [max 0 [min [expr $current+$by] [expr $ver(cnt)-1]]]
  if {$verbose} {
    puts stderr "moving $current by $by ($from $to)"
  }
  if {$from == $to} {return}
  set on ver(o.$to)
  set en it($to)
  swap ver(o.$to) ver(o.$from) 
  swap ver(f.$to) ver(f.$from)
  remove_vers $from 3
  remove_vers $to   3
  swap ver(v.$to) ver(v.$from)
  swap ver(s.$to) ver(s.$from)
  swap ver(c.$to) ver(c.$from)
  add_vers $from 3
  add_vers $to   3
  focus .en_${to}
}
## # GUI: remove radiobuttons for versions in row r starting column c
proc remove_vers {r c} {
  global ver
  if {$ver(c.$r) != {nil}} {
    foreach v $ver(v.$r) {destroy .ra_${r}_${c}; incr c}
  }  
}
## # GUI: add radiobuttons for versions in row r starting column c
proc add_vers {r c} {
  global ver
  if {$ver(c.$r) != {nil}} {
    foreach v $ver(v.$r) {
      radiobutton .ra_${r}_${c} -variable ver(s.$r) -text $v -value $v
      grid .ra_${r}_${c} -row $r -column $c -sticky w -in .; incr c
    }
  }
}
## # GUI: build the list of items found in path
## #      toplvel is fixed as '.'
## #      return the number of rows occipied plus 1
## #      pack items into an array to simplify manipulation

## # The array ver contains the processed path data:
## # ver(cnt) : number of entries (index 0 to cnt-1)
## # ver(o.index) : "on" set to 1 to enable entry
## # ver(s.index) : disused
## # ver(f.index) : "form" item (with placeholder for version)
## # ver(c.index) : current hdms version or "nil"
## # ver(v.index) : list of hdms versions (if present)

proc add_entry {r ver_} {
  global verbose
  upvar $ver_ ver
  checkbutton .ch_${r} -text on -variable ver(o.$r) -command "path_check $r"
  entry .en_${r} -width 80 -text ver(f.$r) -justify l
  bind .en_${r} <FocusIn>  "take_focus $r"
  bind .en_${r} <FocusOut> "leave_focus $r"
  bind .en_${r} <Any-Leave> "path_check $r"
  bind .en_${r} <Key-Escape>   "path_complete $r"
  set c 1
  grid .ch_${r} -row $r -column $c -sticky w -in .; incr c
  grid .en_${r} -row $r -column $c -sticky w -in .; incr c
  if {$ver(c.$r) == {nil}} {
    set ver(s.$r) nil
  } else {
    set ver(s.$r) $ver(c.$r)
    add_vers $r $c
  }
  path_check $r
}
proc build_viewer {items ver_} {
  global verbose
  upvar $ver_ ver
  set r 0
  foreach item $items {
    path2vers $item ver(v.$r) ver(c.$r) ver(f.$r)
    set ver(o.$r) 1
    add_entry $r ver
    incr r
  }
  set ver(cnt) $r
  if {$verbose} {arview ver}
  return $r
}
proc add {} {
  global ver
  set r $ver(cnt)
  set ver(v.$r) [list ]
  set ver(c.$r) nil
  set ver(f.$r) nil
  set ver(o.$r) 0
  add_entry $r ver
  incr ver(cnt)
}

#########################################################################
## #
## # MAIN : read variable / stdin
## #

# default to non verbose operation
set verbose 0

# maximum number of version printed on right side of GUI
set var(maxvers) 5; # (prints n highest)

set var(maxitems) 100; # maximum number of items the script can handle

path_complete_showme_not; # name completion remember last completed

set var(items) [commandline]; # read commandline, ontain path

set var(help) "LIST shows a human readable listing, PRINT shows the setenv command. \
\n\nEditing:\nSelect an entry, then move it using UP/DOWN buttons. \
\nEntries can be excluded using the 'on' button on the left. New entries can be added using the 'ADD' button. \
\nIf an hdms style path is detected, versions can be selected (only the $var(maxvers) newest versions are displayed). \
\nPath segments are editable, please do not remove the '%s' version placeholders. The tool automatically checks the existence of a path, turning the background green or red.\
\n\nOriginal version Martin Hummel autumn 2007, this version 7th April 2009."


## # 
## # build GUI
## # 

## # create viewer
set r [build_viewer $var(items) ver]
## # create buttons
set r $var(maxitems)
frame  .btn; # container for navigation and quit
button .bt2 -text "PRINT"  -command { print_cmd }
button .bt3 -text "LIST"   -command { list_cmd }
button .bt4 -text "HELP"   -command { gui_help }
set c 2; set last [expr $c+$var(maxvers)+1]
grid .btn -row $r -column $c    -sticky news -in .; incr c
grid .bt2 -row $r -column $c    -sticky e -in .; incr c
grid .bt3 -row $r -column $c    -sticky e -in .; incr c
grid .bt4 -row $r -column $last -sticky e -in .; incr c

set f .btn
button $f.bt1 -text "PRINT & QUIT" -command { print_cmd; exit }
button $f.add -text "ADD"      -command {add}
frame  $f.spc -width 10
button $f.top -text "TOP"      -command {moveentry -999} -state disabled
button $f.up  -text "---UP---" -command {moveentry -1}   -state disabled
button $f.dn  -text "--DOWN--" -command {moveentry +1}   -state disabled
button $f.btm -text "BOTTOM"   -command {moveentry +999} -state disabled
set current nil

pack $f.bt1 -side left -in $f
#pack $f.btm $f.dn $f.up $f.top -side right -in $f
pack $f.dn $f.up $f.spc $f.add -side right -in $f

# Note on the TOP/BUTTOM buttons 
#  Currently moveentry works by swapping line. Allowing it
#  to swap over a distance greater that one creates unexpected
#  results (though still correct).

wm title . $var(env)

# End of file
################################################################################
# Local Variables:
# comment-column: 60
# End:
################################################################################
