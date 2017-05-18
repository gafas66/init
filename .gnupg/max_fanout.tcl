

set high_fanout 2


set gan [get_nets -h *]

foreach_in_collection gg $gan {

   set ggname [get_attribute $gg full_name]

   # echo "Processing $ggname"

   set gpins [get_pins -leaf -quiet -of $gg]

   set gpins_in [filter_collection $gpins "direction==in"]

   set gpins_in_soc [sizeof_collection $gpins_in]

   if { $gpins_in_soc > $high_fanout } {

	echo "Net $ggname is a high fanout net $gpins_in_soc greater than $high_fanout connections"

   }
}
   
