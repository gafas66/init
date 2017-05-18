#! /usr/bin/env tclsh
################################################################################
# Created: Friday, May 11 2012
# Author: ekofoed
#
# Description:
#

source /home/ekofoed/bin/my_pt_funcs.tcl

set ::activate_comments 1

proc p {text} {if {[info exist ::activate_comments] && $::activate_comments} {echo "EK_INFO: [subst $text]"}}
proc pop {text} {sh echo $text | xmessage -center -timeout 5 -file - }
proc ek_clean {mylist} {
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
    return $new
}

################################################################################

proc is_port {p} {if {[get_port -q $p] != ""} {return true};return false}

proc ek_get_driver {net} {
    if {[get_pin -q $net] != ""} {set net [get_net -q -of [get_pin $net]]}
    if {$net == ""} {return "UC"}
    if [regexp -- {SYNOPSYS_UNCONNECTED_} [gon $net]] {return "UC"}

    set d [all_fanin -to $net -flat -only_cell -level 1]
    if {$d != ""} {return $d}
    set d [all_fanin -to $net -flat -level 1]
    if {$d != ""} {return $d} ;# Port I presume
    return "UC"
}

proc ek_drive_v {net} {
    set d [ek_get_driver $net]
    if [is_port $d] {
	set d [gon $d]
	if [regexp -- {_3v} $d] {return "3V"}
	return "1V"
    }
    if {$d == "UC"} {return "UC"}
    # Driver is a cell
    set ref [get_attr $d ref_name]
    if [regexp -- {LSHL} $ref] {return "1V"}
    if [regexp -- {TMZ}  $ref] {return "3V"}
    return "1V"
}

################################################################################

proc ek_number_of_sinks {} {
    #desc Outputs csv w/number of sinks per clock
    set clocked 0
    echo "Clock:20,Sinks:8"
    foreach_ clk [all_clocks] {
	puts "[gon $clk],[sizeof_ [all_reg -clock $clk]]"
	set clocked [expr $clocked + [sizeof_ [all_reg -clock $clk]]]
    }
    puts "Total,[sizeof_ [all_reg]]"
    set missing [expr [sizeof_ [all_reg]] - $clocked]
    puts "With clock,$missing"
}

################################################################################

proc ek_only_input {pin} {
    # set case to all other inputs on net to zero (for CTS use)
    set pins [get_pin -of [get_net -of [get_pin $pin]] -filter "pin_direction == in"]
    set pins [remove_from_coll $pins $pin]
    set_case_analysis 0 $pins
}

proc pp {col} {
    #desc Pretty print collection
    foreach_ el $col {puts [get_object_name $el]}
}
proc pps {str} {
    #desc Split input string on space/pretty print result
    foreach el [split $str] {puts $el}}
proc gs {} {
    #desc Short for get_selection
    get_selection}
proc cs {args} {
    #desc Short for change_selection
    if {[get_cell -q $args] != ""} {change_selection [get_cell $args];return}
    if {[get_net  -q $args] != ""} {change_selection [get_net  $args];return}
    if {[get_pin  -q $args] != ""} {change_selection [get_pin  $args];return}
    puts "What is '$args'? Its not cell/net/pin"
}
proc csio {} {
    #desc Change selection to all cells w/is_io true
    change_selection [get_cells * -hier -filter "is_io == true"]}
proc cshm {} {
    #desc Change selection to all cells w/is_hard_macro true
    change_selection [get_cells * -hier -filter "is_hard_macro == true"]}
proc cssc {} {
    #desc Change selection to all standard cells (i.e., not io/hm)
    set cells [remove_from_coll [get_cells * -hier] [get_cells * -hier -filter "is_hard_macro == true"]]
    change_selection [remove_from_coll $cells [get_cells * -hier -filter "is_io == true"]]
}
proc csr {ref} {
    #desc Change selection to all cells with given *reference* name (wildcard)
    change_selection [get_cells -all * -hier -filter "ref_name =~ *$ref*"]}
proc gc {} {
    # works on selection
    if {[get_pin -q [gon [gs]]] != ""} {
	set ck [get_attr -q [gs] clocks]
	set cs [get_attr -q [gs] case_value]
	if {$ck != ""} {return $ck}
	if {$cs != ""} {return $cs}
	return "No clock/case"
    }
    if {[get_cell -q [gon [gs]]] != ""} {
	set cs [get_attr -q [gs] dont_touch]
	if {$cs != ""} {puts "${::r}dont_touch${::w}: [gon [gs]]"}
	foreach_ pin [get_pins [gon [gs]]/*] {
	    #echo "CHECKING [gon $pin]"
	    set ck  [get_attr -q $pin clocks]
	    set cs  [get_attr -q $pin case_value]
	    set dir [get_attr -q $pin direction]
	    regexp -- {(.*)/(.+)} [gon $pin] x main p
	    if {$ck != ""} {puts "\[$dir\] $p => $::g[gon $ck]${::w}" ;continue }
	    if {$cs != ""} {puts "\[$dir\] $p => ${::r}$cs$::w"} else {puts "$p =>"}
	}
	return
    }
    puts "# Dont know [gon [gs]]"
}
proc gon {o} {
    #desc Short for get_object_name
    return [get_object_name $o]}

proc gi {} {
    #desc Get info on object (ref/height/width) - OR general about current state/scen
    if {[gs] == ""} {
	echo "STEP               : $::env(STEP)"
	echo "Active scenarios   : [all_active_scenarios]"
	echo "All scenatios      : [all_scenarios]"
	current_scenario
    } else {
	echo "Instance : [gon [gs]]"
	echo "Reference: [get_attr [gs] ref_name]"
	set bbox [get_attr [gs] bbox]
	echo "BBOX     : $bbox"
	scan [get_attr [gs] bbox] "{%f %f} {%f %f}" a b c d
	echo "Width    : [expr $c-$a]"
	echo "Height   : [expr $d-$b]"
    }
}

proc ek_fpc {ck1 ck2} {
    set_false_path -from [get_clock $ck1] -to   [get_clock $ck2]
    set_false_path -to   [get_clock $ck1] -from [get_clock $ck2]
}

proc ek_report_clock {} {
    foreach_ clock [all_clocks] {
	set is_gen [get_attr $clock is_generated]
	if {! $is_gen} {
	    puts "# [gon $clock]"
	    if {[sizeof_ [get_attr -q $clock generated_clocks]] > 0} {
		set stop_pins {}
		foreach_ gen [get_attr $clock generated_clocks] {
		    set pin [gon [get_attr -q $gen sources]]
		    puts "#   [gon $gen] gon $pin"
		    lappend stop_pins [gon [driver $pin]]
		}
	    }
	    puts "set_clock_tree_exceptions -stop_pins {$stop_pins}"
	}
    }
}

proc ek_replace_ref {from to} {
    puts "# Replacing $from => $to"
    foreach_in cell [get_cells * -quiet -hier -filter "ref_name == $from"] {
	replace_cell_reference -lib_cell $to $cell
	puts "# ... $to <= [gon $cell]"
    }
    set_dont_use [get_lib_cell */$from]
}

################################################################################

proc ek_analyzeQor {{step_name 1_unknown} {header false}} {

    #desc Outputs csv list of "report_qor" command (for Excel sheet)
    update_timing > /dev/null
    redirect -v ::_qor {report_qor}

    set tpath  0
    set active 0
    set scenario "no_scenario"
    lappend group(scenarios) $scenario

    foreach line [split $::_qor "\n"] {
	# Get scenario - if any
	if [regexp -- {Scenario '(.*)'} $line m scenario] {
	    lappend groups(scenarios) $scenario
	    continue
	}
	if [regexp -- {Timing Path Group '(.*)'} $line m clk] {
	    incr tpath
	    set active 1
	    lappend groups(clocks) $clk
	    continue
	}
	if {! $active} {continue}

	if [regexp -- {-----} $line] {incr tpath;continue}

	if {$tpath == 3} {
	    set tpath  0
	    set active 0
	    continue
	}

	if {$tpath == 2} {
	    regexp -- { *(.*): +(-?\d+\.?\d*)} $line m type val
	    lappend groups(VAL,$scenario,$clk) ",$val"
	}
    }

    # Output in better format

    if {$header} {
	puts "Step:10,Scen:8,Group/clock:30,LoL:12,CPL:12,WNS:12,Period:12,TNS:12,#:12,WNS-H:12,TNS:12,#:12"
    }
    foreach clk $groups(clocks) {
	foreach scen $groups(scenarios) {
	    set line [regsub -all -- { } $groups(VAL,$scen,$clk) ""]
	    puts "$step_name,$scen,$clk$line"
	}
    }
}

################################################################################

proc ek_excelHeader {{stage DetailRouteOpt}} {

    #desc Outputs csv header for Excel sheet
    #verify_zrt_route

    set head outputs/1_Summary.csv
    echo "What:30,Value:60,Comment:40"     > $head

    echo "Date,[date]"                    >> $head
    echo "User,$::env(USER)"              >> $head
    echo "Working dir,$::env(PWD)"        >> $head
    echo "Summary from stage,$stage"      >> $head

    echo "" >> $head

    update_timing > /dev/null

    set drc [sizeof_ [get_drc_errors]]
    echo "DRC violations,$drc"            >> $head

    echo "" >> $head

    foreach_in path [get_timing_path -slack_less 0] {
	set group [get_object_name [get_attr $path path_group]]
	set slack [get_attr $path slack]
	echo "Setup/WNS - $group,$slack"  >> $head	
    }    
    foreach_in path [get_timing_path -delay min -slack_less 0] {
	set group [get_object_name [get_attr $path path_group]]
	set slack [get_attr $path slack]
	echo "Hold/WNS - $group,$slack"   >> $head	
    }
}

################################################################################

proc ek_report_timing {} {

    #desc Outputs csv of timing max/min for excel sheet
    set n 1000
    set file "outputs/5_Timing.csv"

    echo "Type:8,Launch:8,Capture:8,Slack:10,Startpoint:30,Start clock:20,Endpoint:30,End clock:20" ;#> $file

    foreach type {max min} {
	foreach_in path [get_timing_paths -delay $type -slack_less 0 -max_paths $n] {
	    set slack   [get_attr $path slack]
	    set end     [get_object_name [get_attr $path endpoint]]
	    set start   [get_object_name [get_attr $path startpoint]]
	    set endck   [get_object_name [get_attr $path endpoint_clock]]
	    set startck [get_object_name [get_attr $path startpoint_clock]]
	    set launch  [get_attr $path startpoint_clock_open_edge_value]
	    set capture [get_attr $path endpoint_clock_open_edge_value]
	    echo "$type,$launch,$capture,$slack,$start,$startck,$end,$endck" ;#>> $file
	}
    }
}

################################################################################

proc ek_reportClocks {} {
    #desc Outputs csv overview of clocks (for Excel)
    update_timing > /dev/null
    puts "Clock:20,period \[ns\]:8,Freq \[MHz\]:8,Setup unc:10,Hold unc:10,max Lat:10,min Lat:10"
    foreach_ clock [get_clocks] {
	set name   [get_object_name $clock]
	set period [get_attr -quiet $clock period]
	if {$period == ""} {set period 0.1}
	set regs [sizeof_ [all_reg -clock [gon $clock]]]
	set freq   [expr 1000.0 / $period]
	set setUnc [get_attr -quiet $clock setup_uncertainty]
	set holUnc [get_attr -quiet $clock hold_uncertainty]
	set maxLat [get_attr -quiet $clock clock_latency_rise_max]
	set minLat [get_attr -quiet $clock clock_latency_rise_min]
	if {$setUnc == ""} {set setUnc 0.0}
	if {$holUnc == ""} {set holUnc 0.0}
	if {$maxLat == ""} {set maxLat 0.0}
	if {$minLat == ""} {set minLat 0.0}
	puts [format "%s,%5d,%5.2f,%5.3f,%5.3f,%5.3f,%5.3f,%5.3f" $name $regs $period $freq $setUnc $holUnc $maxLat $minLat]
    }
}

proc ek_reportSkew {} {
    redirect -var x {report_clock_tree -nosplit -summ}
    set active 0
    echo "Clock:20,Master:15,Sinks:10,Ctbuff:8,Ratio:8,Ctcell:8,Skew:10,Long:10,DRC:8,Buff:10"
    foreach line [split $x "\n"] {
	if [regexp -- {^---} $line]    { set active 1 ;continue }
	if [regexp -- {^1}   $line]    { set active 0 ;continue }
	if $active {
	    regexp -- {(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)} $line match \
		clock sinks ctbuff ctcell skew long drc buff
	    if {$ctbuff == 0} {set ctb 1} else {set ctb $ctbuff}
	    set ratio [expr $sinks / $ctb]
	    set master [get_attr -q [get_clock $clock] master_clock]
	    if {$master != ""} {set master [gon $master]}
	    echo "$clock,$master,$sinks,$ctbuff,$ratio,$ctcell,$skew,$long,$drc,$buff"
	}
    }
}

################################################################################

proc ek_excelHeader {} {
    puts "What:30,Value:50"
    puts "Project,$::env(PROJECT)"
    puts "User,$::env(USER)"
    puts "Rundir,[pwd]"
    puts "Date,[date]"
}

set ::__TIMEFILE outputs/9_Runtime.csv

proc ek_begin {step} {
    #desc Sets timestamp in a file to check time between begin/end pairs
    puts "# START OF STEP=$step"
    set ::env(STEP) $step
    
    set ::__STARTTIME [clock seconds];set ::__STEPNAME $step;ek_init_time_file

    if {[get_mw_cels -q] == ""} {
	if [file exists $::VAR_OUT] {
	    echo "# Sourcing previous variables"
	    uplevel 1 {source $::VAR_OUT}
	}
	if [info exist ::env(_LIB)] {
	    echo "# Forcing values in interactive mode"
	    set ::LIB $::env(_LIB) ; unset ::env(_LIB)
	    set ::FLOW flow
	    set ::STEPP $::env(STEPP)
	    echo "# Done forcing values in interactive mode"
	}
	open_mw_lib $::LIB
	set delim _ ;if {$::STEPP == ""} {set delim ""}
	open_mw_cel ${::DESIGN_NAME}${delim}$::STEPP;# Previous step
    }
}
proc ek_end   {}     {
    #desc Sets timestamp in a file to check time between begin/end pairs
    echo "# END OF SCRIPT  STEP=$::env(STEP)"
    set ::__STOPTIME [clock seconds]
    set duration [expr $::__STOPTIME - $::__STARTTIME]

    #
    # Get accumulated time through runs - pass info using perl/YAML
    #

    set accFile outputs/acc.yml
    set acc 0
    if [file exist $accFile] {
	set acc [exec perl -e "use YAML;my \$x = YAML::LoadFile('$accFile');print \$\$x{acc}"]
    }
    set acc [expr $acc + $duration]
    echo "acc: $acc" > $accFile

    set line "$::__STEPNAME, [ek_sec_to_min $duration],[ek_sec_to_min $acc]"
    echo $line >> $::__TIMEFILE

    #
    # Save design
    #
    
    uplevel 1 {
	save_mw_cel -as ${DESIGN_NAME}
	save_mw_cel -as ${DESIGN_NAME}_$env(STEP)

	set STEPP $env(STEP) ;# NOTE keeps note of what was last step run !
	interp'dump > $::VAR_OUT

	#
	# Save stuff for jameal/ijeoma
	#

	if {[info exist env(NOREP)]} {return}

	write_verilog -pg -diode_ports outputs/${DESIGN_NAME}_$::env(STEP).pg.v
	write_verilog -pg -diode_ports -no_physical_only_cells  outputs/${DESIGN_NAME}_$::env(STEP).notap.pg.v
	write_verilog -wire_declaration  -no_physical_only_cells -supply_statement none outputs/${DESIGN_NAME}_$::env(STEP).v
	save_upf                       outputs/${DESIGN_NAME}_$::env(STEP).upf
	write_sdf                      outputs/${DESIGN_NAME}_$::env(STEP).sdf
	write_sdc -nospl               outputs/${DESIGN_NAME}_$::env(STEP).sdc

	if {[info exists QUIT] && $QUIT} {quit}
    }
}

proc ek_init_time_file {} {
    if {[file exist $::__TIMEFILE]} {return}
    echo "Step:20,Duration \[h:m:s\]:10,Accum:10" > $::__TIMEFILE
}

proc ek_sec_to_min {sec} {
    set hours [expr $sec / 3600];set sec [expr $sec % 3600]
    set mins  [expr $sec / 60  ];set sec [expr $sec % 60]
    return "$hours:$mins:$sec"
}

################################################################################

proc ek_llibs {} {
    #desc Outputs csv list of libraries (for Excel sheet)
    redirect -v libs {list_libs}
    puts "Library:15,DB:20,Location:50,_PG_:10"
    set step 0
    foreach line [split $libs "\n"] {
	if {$line == "1"} {break}
	if [regexp -- {^----} $line] {incr step;continue}
	if {$step == 2} {
	    set lib  [lindex $line 0]
	    set db   [lindex $line 1]
	    set dir  [lindex $line 2]
	    regexp -- {(.*).db} $db x fname
	    set libname "$dir/$fname.lib"
	    set is_pg false
	    #puts "checking: $libname"
	    if [file exist $libname] {
		if {[exec perl -ne "/pg_/i && (\$f=1,last);END{\$f && (print \$ARGV)}" $libname] != ""} {set is_pg true}
	    }
	    puts [format "%s,%s,%s,%s" $lib $db $dir $is_pg]
	}
    }
}

################################################################################

proc ek_ct {} {
    #desc Outputs csv list of check_timing command (for Excel)
    redirect -v ct {check_timing}
    set type ""
    set step 0
    set count 0
    puts "Type:50,Location:100"
    foreach line [split $ct "\n"] {
	#puts $line;incr count;if {$count == 20} {return}
	if [regexp -- {Warning: The following (.*)} $line x] {set step 1;continue}
	regexp -- {Information: Checking (.*)} $line x type
	if {$step == 1 && [regexp -- {^----} $line]} {set step 2;continue}
	if {[regexp -- {^$} $line] && ($step == 2)} {set step 0;continue}
	if {$step == 2} {
	    puts "$type,$line"
	}
    }
}

################################################################################

proc ek_stats {} {
    #desc Outputs csv list of each reference, and how often used (for Excel)
    set s(dummy) 1
    foreach_ cell [get_cells * -hier] {
	if {! [get_attr $cell is_hierarchical]} {
	    set x [get_attr $cell ref_name]
	    if [info exist s($x)] {
		incr s($x)
	    } else {
		set s($x) 1
	    }
	}
    }
    array unset s(dummy)
    echo "Cell:25,Count:10"
    foreach ref [array names s] {
	echo "$ref,$s($ref)"
    }
}

proc ek_vars {} {
    #desc Outputs all application vars with value (for Excel)
    redirect -v result {print_variable_group system}
    echo "Var:20,Value:100"
    foreach {var equal val} $result {
	echo "$var,$val"
    }
}

################################################################################

proc ek_ideal_clock {} {
    #desc Sets ideal nets on all clocks
    foreach_in clock [all_clocks] {
	puts "CLOCK: [gon $clock]"
    	set sources [get_attr $clock sources]
	puts "SOURCES: [gon $sources]"
    	set pin     [gon $sources]
	puts "PIN: $pin"
	if {[get_port -q $pin] != ""} {
	    set_ideal_net [gon [get_net -of_obj [get_port $pin]]]
	} else {
	    # check if net exist
	    set net [get_net -quiet -of_obj [get_pin $pin]]
	    if {$net != ""} {set_ideal_net [gon $net]} else {puts "Error: $pin has no net"}
	}
    }
}

################################################################################

proc ek_dont_touch_to_ideal {} {
    #desc Temporary solve dont_touch nets by making them ideal (always-on nets)
    redirect -var rpt {report_constr -nosplit -all -max_fan}
    foreach line [split $rpt "\n"] {
	if {![regexp -- {dont_touch} $line]} { continue }
	regexp -- {^\s+(\S+)} $line x match
	puts "# Setting ideal net: $match"
	set_ideal_net $match
    }
}

################################################################################

proc interp'dump {} {
    uplevel 1 {
	set r1 "\# interpreter status dump\n"
	foreach i [info vars] {
	    if [regexp -- {^(_)|(erro)} $i] {continue} ;# Ignore underscore vars
	    if [regexp -- {^.$} $i] {continue} ;# Ignore single letter vars
	    if {![info exists ::$i]} {continue} ;# Ignore non-global vars
	    if {$i == "VARS_IN"} {continue}
	    
	    if {[info exists ::VARS_IN($i)]} {
		if {[array exists $i]} {
		    if {$::VARS_IN($i) == [array get $i]} { continue }
		} else {
		    if {$::VARS_IN($i) == [set $i]} { continue }
		} ; #dump only changed vars
	    }

	    if {$i == "env"} continue  ;# don't dump environment..
	    
	    if {[array exists ::$i]} {
		append r1 [list array set $i [array get ::$i]]\n
	    } else {
		append r1 [list set $i [set ::$i]]\n
	    }
	}
	set r1
    }
}

proc ek_vars_in {} {
    uplevel 1 {
	foreach v [info vars] {
	    if {[array exists $v]} {
		set ::VARS_IN($v) [array get $v]
	    } else {
		set ::VARS_IN($v) [set $v] ;# [set $x] is indirection
	    }
	}
    }
}

################################################################################

proc test {in} {
    array unset ::result
    foreach_ cell [get_cells * -hier -filter "ref_name == $in"] {
	set o [lindex [get_attr $cell origin] 0]
	set o [expr $o - int($o)]
	puts $o ;continue
	#if {[expr ($o > 0.35) && ($o < 0.45)]} { set ::result([get_attr $cell ref_name]) 1}
	set w [get_attr $cell width]
	#set ::result($o) 1
    }
}

################################################################################

proc ek_scen {command} {
    #desc Updates each scenario with command
    set current [current_scenario]
    foreach scen [all_scenarios] {
	echo "$scen"
	current_scenario $scen
	eval $command
    }
    current_scenario $current
}

################################################################################

proc latencyCsv {} {
    foreach_ ck [all_clock] {
	if {"[get_attr -q $ck sources]" == ""} {
	    echo "[gon $ck] is virtual"
	}
    }
}

################################################################################

proc ek_ck_info {} {
    #desc table clocks
    set masters {}
    foreach_ c [all_clock] {if { ! [get_attr $c is_generated] } {lappend masters [gon $c]}}
    foreach m $masters {
	rec_gen $m
	puts "-++-"
    }
}
proc rec_gen {ck {lead "----"}} {
    set p [get_attr [get_clock $ck] period]
    set f [expr 1000.0 / $p]
    set unit MHz
    if {$f < 1.000} {set f [expr $f * 1000];set unit kHz}
    set f [format "%6.2f" $f]
    set r [sizeof_ [all_reg -clock $ck]]
    set r [format "%6d" $r]
    puts "$f $unit $r (registers) $lead $ck"
    foreach_ gen [get_attr -q [get_clock $ck] generated_clocks] {
	rec_gen [gon $gen] "$lead----"
    }
}

################################################################################

proc m {} {
    #desc Reload icc_procs - and show help menu
    #DESC
    global FLOW
    set file flow/icc_procs.tcl
    source $file

    puts "--------------------------------------------------------------------------------"
    set fp [open $file r] ;set file [read $fp];close $fp
    foreach line [split $file "\n"] {
	if [regexp -- {^proc\s+(\S+)} $line x name] {set procName $name}
	if [regexp -- {\#desc (.*)} $line x desc] {puts [format "%-25s # %s" $procName $desc]}
	if [regexp -- {\#DESC} $line] { break }
    }
    puts "--------------------------------------------------------------------------------"
}

# End of file
################################################################################
# Local Variables:
# comment-column: 60
# End:
################################################################################
