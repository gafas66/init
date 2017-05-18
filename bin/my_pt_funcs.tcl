#!/usr/bin/tclsh
########################################################################################################################
# Created: Thursday, January 20 2011
# Author: Erik KOFOED
#
# Description: useful pt functions

# Colors:
set r "\033\[0;31m"
set g "\033\[0;32m"
set y "\033\[0;33m"
set c "\033\[0;36m"
set w "\033\[m"

set ::min_slack 0

################################################################################
#
#

proc p {text} {if {[info exist ::activate_comments] && $::activate_comments} {echo "$::{g}TZ_INFO: [subst $text]$::{w}"}}
proc pop {text} {sh echo $text | xmessage -center -timeout 5 -file - }
proc tz_clean {mylist} {
    upvar $mylist local

    set new ""
    foreach ::line [split $local "\n"] {
	if [regexp -- {^\s+(.*)}     $::line match first] { set ::line $first } ;#Leading space
	if [regexp -- {(\S*)\s*}     $::line match first] { set ::line $first } ;#Trailing space
	if [regexp -- {^#} $::line]    { continue }                           ;#Skip comments
	if [regexp -- {^(.*);\s*#.*} $::line match first] { set ::line $first } ;#Skip comments
	if [regexp -- {^$} $::line]    { continue } ;#Skip empty ::lines
	uplevel 1 {set ::line [subst $::line]}
	set new [concat $new $::line]
    }
    set local $new
}

proc d {netOrPin} {
    #desc Returns driving pin of net or pin
    if [regexp -- {^_sel} $netOrPin] {set netOrPin [gon $netOrPin]}
    if {[get_net -q $netOrPin] == ""} {
	if {[catch {set netOrPin [gon [get_net -of [get_pin $netOrPin]]]}]} {
	    p "$netOrPin is unused?"
	    return
	}
    }
    # p "Net is $netOrPin"
    set fanin [remove_from_coll [all_fanin -only -flat -to [get_net $netOrPin]] [get_net $netOrPin]]
    set index [expr [sizeof_ $fanin] - 1]
    # p "Index is $index"
    set driving_cell [gon [index_collection $fanin $index]]
    set driving_pin  [gon [get_pin $driving_cell/* -filter "direction == out"]]
    set driving_ref  [get_attr [get_cell $driving_cell] ref_name]
    # p "CELL: $driving_cell"
    # p "PIN:  $driving_pin"
    # p "REF:  $driving_ref"
    return "$driving_ref ;# $driving_pin"
}

################################################################################
#
#C Report timing -slack_less $::min_slack -path end (or to given ep)

proc rt {{ep 0}} {
    if { $ep == 0 } {
	report_timing -slack_less $::min_slack -path end
    } else {
	report_timing -slack_less $::min_slack -to $ep
    }
}    
#C Report timing -slack_less 0 -path end -delay min (or to given ep)
proc rth {{ep 0}} {
    if {$ep == 0} {
	report_timing -slack_less $::min_slack -path end -delay min
    } else {
	report_timing -slack_less $::min_slack -delay min -to $ep
    }
}
#C Report_constraints -all_viol -max_delay
proc rpc {{type -max_delay}} {report_constraint -all_violators $type}

#CS
#C Find clocks of net
proc get_clock_of_net {net} {get_attr [all_fanin -flat -level 0 -to [get_net $net]] clocks}
proc gs {} {get_selection}
proc gc {} {
    # works on selection
    if {[get_pin -q [gon [gs]]] != ""} {
	return [get_attr [gs] clocks]
    }
    if {[get_cell -q [gon [gs]]] != ""} {
	foreach_ pin [get_pins [gon [gs]]/*] {
	    puts "[gon $pin]: [gon [get_attr -q $pin clocks]]"
	}
	return
    }
    puts "# Dont know [gon [gs]]"
}
proc cs {x} {change_selection [get_pin $x]}
#C Get sizeof_collection
proc sc {collection} {return [sizeof_collection $collection]}
#CS
#C Print status for run
proc s {} {
    puts "$::c--------------------------------------------------------------------------------$::w"
    puts "\$STA(MODE/AVS/CORNER/TEMP/RC) = $::STA(MODE) $::STA(AVS) $::STA(CORNER) $::STA(TEMP) $::STA(RC)"
    puts "\$CLASS                        = $::CLASS"
    puts "\$STEPNAME                     = $::STEPNAME"
    puts "\$BA_PATH                      = $::BA_PATH"
    puts "\$MASS_ILM                     = $::MASS_ILM"
    puts "\$STA(COND)                    = $::STA(COND)"
    puts "$::c--------------------------------------------------------------------------------$::w"
}

########################################################################################################################
#C Report tabulated timing results. Args: -single, -slack_min n
proc qor {args} {

    suppress_message ATTR-3

    parse_proc_arguments -args $args results
   
    # What is to be reported?
    if {[info exists results(-single)]} {
        set report_style [list all]
    } else {
        set report_style [list std async clock_gating default]
    }

    if ([info exists results(-slack_min)]) {
        set min_slack $results(-slack_min)
    } else {
        set min_slack 0.0
    }

    foreach del [list max min] {
        foreach reps $report_style {
            switch $reps {
                "all" {
                      set paths [get_timing_paths -slack_lesser_than $min_slack -max_path 1000000 -nworst 1 -delay $del]
                }
                "std" {
                      set paths ""
                      foreach_in_collection pp [get_path_groups -filter {full_name !~ "\*\**"}] {
        	              set paths [add_to_collection $paths [get_timing_paths -slack_lesser_than $min_slack -max_path 1000000 -nworst 1 -delay $del -group $pp]]
                      }
                }
                "async" {
                      set paths [get_timing_paths -slack_lesser_than $min_slack -max_path 1000000 -nworst 1 -delay $del -group {**async_default**}]
                }
                "clock_gating" {
                      set paths [get_timing_paths -slack_lesser_than $min_slack -max_path 1000000 -nworst 1 -delay $del -group {**clock_gating_default**}]
                }
                "default" {
                      set paths [get_timing_paths -slack_lesser_than $min_slack -max_path 1000000 -nworst 1 -delay $del -group [get_path_group -filter "full_name == **default**"]]
                }
            }

            switch $del {
                max {set title "Setup Worst and Total negative slack ($reps paths)"}
                min {set title "Hold Worst and Total negative slack ($reps paths)"}
            }
    
            foreach type [list reg2reg in2reg reg2out in2out all] {
                set totalWNS($type) 0
                set totalTNS($type) 0
                set totalNbNS($type) 0
            }
            foreach_in_collection path $paths {
                if { [get_attribute $path slack] != "INFINITY" } {
                    set endClock [get_object_name [get_attribute $path endpoint_clock]]
                    set startClock [get_object_name [get_attribute $path startpoint_clock]]
                    set pathType [ptGetPathType $path]
                    set slack [get_attribute $path slack]
                    if {[info exists TNS($endClock)] == 0} {
                        set TNS($endClock) 0.0
                        set WNS($endClock) $slack
                        set NbNS($endClock) 0
                        foreach type [list reg2reg in2reg reg2out in2out] {
                            set TNStype($endClock,$type) 0
                            set WNStype($endClock,$type) 0.0
                            set NbNStype($endClock,$type) 0
    
                            set WNStype($endClock,$pathType) $slack
                        }
                    }
                    if {[info exists TNSfromto($endClock,$startClock)] == 0} {
                        set TNSfromto($endClock,$startClock) 0.0
                        set WNSfromto($endClock,$startClock) $slack
                        set NbNSfromto($endClock,$startClock) 0
                        foreach type [list reg2reg in2reg reg2out in2out] {
                            set TNStype($endClock,$startClock,$type) 0
                            set WNStype($endClock,$startClock,$type) 0.0
                            set NbNStype($endClock,$startClock,$type) 0
    
                            set WNStype($endClock,$startClock,$pathType) $slack
                        }
                    }
        
                    if { $WNS($endClock) > $slack } { set WNS($endClock) $slack }
                    set TNS($endClock) [expr $TNS($endClock)+$slack]
                    incr NbNS($endClock)
        
                    if { $WNStype($endClock,$pathType) > $slack } { set WNStype($endClock,$pathType) $slack }
                    set TNStype($endClock,$pathType) [expr $TNStype($endClock,$pathType)+$slack]
                    incr NbNStype($endClock,$pathType)
        
                    if { $WNSfromto($endClock,$startClock) > $slack } { set WNSfromto($endClock,$startClock) $slack }
                    set TNSfromto($endClock,$startClock) [expr $TNSfromto($endClock,$startClock)+$slack]
                    incr NbNSfromto($endClock,$startClock)
        
                    if { $WNStype($endClock,$startClock,$pathType) > $slack } { set WNStype($endClock,$startClock,$pathType) $slack }
                    set TNStype($endClock,$startClock,$pathType) [expr $TNStype($endClock,$startClock,$pathType)+$slack]
                    incr NbNStype($endClock,$startClock,$pathType)
    
                    if { $totalWNS($pathType) > $slack } { set totalWNS($pathType) $slack }
                    set totalTNS($pathType) [expr $totalTNS($pathType)+$slack]
                    incr totalNbNS($pathType)
    
                    if { $totalWNS(all) > $slack } { set totalWNS(all) $slack }
                    set totalTNS(all) [expr $totalTNS(all)+$slack]
                    incr totalNbNS(all)
                }
            }
        
            echo $title
            if {[sizeof_collection $paths] != 0} {
        
                echo "           Overall         |            reg2reg         |     in2reg     |     reg2out    |     in2out     |"
                echo "   WNS         TNS    NbNS |    WNS         TNS    NbNS |    WNS    NbNS |    WNS    NbNS |    WNS    NbNS | Start Clock -> End Clock"
                echo "---------------------------+----------------------------+----------------+----------------+----------------+-------------------------"
                    echo [format "%8.4f %11.4f %5i | %8.4f %11.4f %5i | %8.4f %5i | %8.4f %5i | %8.4f %5i | %-24s" $totalWNS(all) $totalTNS(all) $totalNbNS(all) $totalWNS(reg2reg) $totalTNS(reg2reg) $totalNbNS(reg2reg) $totalWNS(in2reg) $totalNbNS(in2reg) $totalWNS(reg2out) $totalNbNS(reg2out) $totalWNS(in2out) $totalNbNS(in2out) "* -> *"]
                echo "---------------------------+----------------------------+----------------+----------------+----------------+-------------------------"
                foreach name [lsort [array names TNS]] {
                    echo [format "%8.4f %11.4f %5i | %8.4f %11.4f %5i | %8.4f %5i | %8.4f %5i | %8.4f %5i | %-24s" $WNS($name) $TNS($name) $NbNS($name) $WNStype(${name},reg2reg) $TNStype(${name},reg2reg) $NbNStype(${name},reg2reg) $WNStype(${name},in2reg) $NbNStype(${name},in2reg) $WNStype(${name},reg2out) $NbNStype(${name},reg2out) $WNStype(${name},in2out) $NbNStype(${name},in2out) "* -> $name"]
                    foreach fromto [array names TNSfromto "${name},*"] {
                        set fromClk [lindex [split $fromto ","] 1]
                        echo [format "%8.4f %11.4f %5i | %8.4f %11.4f %5i | %8.4f %5i | %8.4f %5i | %8.4f %5i | %-24s" $WNSfromto($fromto) $TNSfromto($fromto) $NbNSfromto($fromto) $WNStype(${fromto},reg2reg) $TNStype(${fromto},reg2reg) $NbNStype(${fromto},reg2reg) $WNStype(${fromto},in2reg) $NbNStype(${fromto},in2reg) $WNStype(${fromto},reg2out) $NbNStype(${fromto},reg2out) $WNStype(${fromto},in2out) $NbNStype(${fromto},in2out) "$fromClk -> $name"]
                    }
                    echo "---------------------------+----------------------------+----------------+----------------+----------------+-------------------------"
                }
            } else {
                echo "No violations"
            }
            echo ""
            echo ""
            array unset TNS
            array unset WNS
            array unset NbNS
            array unset TNSfromto
            array unset WNSfromto
            array unset NbNSfromto
            array unset TNStype
            array unset WNStype
            array unset NbNStype
        }
    }
    unsuppress_message ATTR-3
}
define_proc_attributes qor\
  -info "Reports tabulated listing of timing violations " \
  -define_args \
  {
    {-single    "Report only normal setup/hold" "" boolean optional }
    {-slack_min "Minimum slack to report" "" float optional }
  }


################################################################################
#CI (Help routine)
proc ptGetPathType { path } {

    switch "[sizeof [get_port -quiet [get_attribute $path startpoint]]][sizeof [get_port -quiet [get_attribute $path endpoint]]]" {
        11 { return "in2out" }
        10 { return "in2reg" }
        01 { return "reg2out" }
        00 { return "reg2reg" }
        default { return "reg2reg" }
    }
    
}

################################################################################
proc gon {x} {get_object_name $x}
proc tz_clocks {} {
    set total 0
    set all_reg [all_reg]
    foreach_ ck [all_clock] {
	set regs [all_reg -clock $ck]
	set size [sizeof_ $regs]
	puts "[gon $ck] , $size"
	set total [expr $total + $size]
	set all_reg [remove_from_coll $all_reg $regs]
    }
    puts "Total is $total"
    puts "All regs is [sizeof_ [all_reg]]"
    puts "[gon $all_reg]"
}

########################################################################################################################
#CEND
proc m {} {
    # list all funcs with help lines in this file
    set file /home/ekofoed/bin/my_pt_funcs.tcl
    # slurp file
    set fp [open $file r]
    set file_data [read $fp]
    close $fp
    # Process file
    set data [split $file_data "\n"]
    set comment ""
    set my_format "$::y%-30s $::r\# %s$::w"
    puts "$::c--------------------------------------------------------------------------------$::w"
    puts [format $my_format "Command" "Description"]
    puts "$::c--------------------------------------------------------------------------------$::w"
    set skip 0
    foreach line $data {
	if [regexp -- {\#CEND} $line dummy] {
	    puts "$::c--------------------------------------------------------------------------------$::w"
	    return
	}
	if [regexp -- {\#CS} $line dummy] {puts ""}
	regexp -- {\#C (.*)} $line dummy comment
	if [regexp -- {\#CI} $line dummy] {set skip 1}
	if [regexp -- {proc +([A-Za-z0-9_]+) +\{(.*?)\} *\{} $line dummy command param] {
	    if { ! $skip } {
		if { $param == ""} {
		    puts [format $my_format $command $comment]
		} else {
		    puts [format $my_format "$command $param" $comment]
		}
	    }
	    set skip 0
	}
	
    }
}
# End of file
########################################################################################################################
# Local Variables:
# comment-column: 60
# End:
########################################################################################################################
