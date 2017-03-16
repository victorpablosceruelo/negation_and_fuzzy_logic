#!/bin/bash

#MODES="default relative"
MODES="default"

first_elem() {
    echo ${1}
}

create_outdir() {
    mkdir -p ${outdir} || exit 1
}

update_analyze_program() {
    if ciaotool comp --dynexec analyze analyze; then
	true
    else
	echo "error: 'analyze' program could not be compiled"
	exit 1 
    fi
}
clean_analyze_program() {
    rm -f analyze
}

analyze_input() {
    cat ${CONFIG}_config.txt
    cat ${input}
}

collect_tests() {
    update_analyze_program

    MACHINES=`analyze_input | ./analyze --list-machines`
    MEANS=`analyze_input | ./analyze --list-means`
    TESTS=`analyze_input | ./analyze --list-tests`
    RESOURCES=`analyze_input | ./analyze --list-resources`
    ALLTESTS="${MEANS} ${TESTS}"

    # Default test and mode for the HTML view
    DEF_TEST=`first_elem ${TESTS}`
    DEF_RESOURCE=`first_elem ${RESOURCES}`
    DEF_MODE=`first_elem ${MODES}`
    DEF_MACHINE=`first_elem ${MACHINES}`
}

gen_plots() {
    if which convert > /dev/null; then
	true
    else
	echo "error: 'convert' tool is not found, please install ImageMagick package (e.g. apt-get install imagemagick)"
	exit 1
    fi
    update_analyze_program
    collect_tests
    create_outdir
    echo "Generating plots"
    analyze_input | ./analyze --gnuplot ${outdir} | gnuplot
#    for m in ${MACHINES}; do
#	for j in ${MODES}; do
#	    for i in ${ALLTESTS}; do
#		for r in ${RESOURCES}; do
#		    echo "Generating plot-${m}-${j}-${i}-${r}.png"
#		    convert -density 150 ${outdir}/plot-${m}-${j}-${i}-${r}.eps ${outdir}/plot-${m}-${j}-${i}-${r}.png
#		done
#	    done
#	done
#    done
}

gen_html() {
    update_analyze_program
    collect_tests
    echo "Generating HTML"
    create_outdir

    out="${outdir}/index.html"
    cat > ${out} <<EOF 
<html>
<head>
<title>Abstract Machine Generation Versions - Benchmark result visualization</title>
<style type="text/css">
  body {
    padding-left: 4em;
    padding-top: 1em;
    font-family: Helvetica, Geneva, Arial, sans-serif;
    font-size: 10pt;
    color: black;
    background-color: white; 
  }
  h1 {
    font-family: Helvetica, Geneva, Arial, sans-serif;
    color: #202020;
  }
  span.paramsel {
    padding-left: 2px;
    padding-right: 2px;
    color: #20a020;
    cursor: pointer;    
  }
  span.paramsel:hover {
    padding-left: 2px;
    padding-right: 2px;
    color: #20c020;
  }
  span.paramselon {
    padding-left: 2px;
    padding-right: 2px;
    color: white;
    background-color: #20a020;
    cursor: pointer;    
  }
  span.paramselon:hover {
    padding-left: 2px;
    padding-right: 2px;
    color: white;
    background-color: #20c020;
  }
</style>
<script language="javascript">
var machine = null;
var mode = null;
var test = null;
var resource = null;
function update_plot() {
  document.getElementById("plot").src = "plot-"+machine+"-"+mode+"-"+test+"-"+resource+".png";
}
function set_machine(newmachine) {
  if (machine != newmachine) {
    if (machine != null) document.getElementById("machine_"+machine).className = "paramsel";
    document.getElementById("machine_"+newmachine).className = "paramselon";
    machine = newmachine;
    update_plot();
  }
}
function set_mode(newmode) {
  if (mode != newmode) {
    if (mode != null) document.getElementById("mode_"+mode).className = "paramsel";
    document.getElementById("mode_"+newmode).className = "paramselon";
    mode = newmode;
    update_plot();
  }
}
function set_test(newtest) {
  if (test != newtest) {
    if (test != null) document.getElementById("test_"+test).className = "paramsel";
    document.getElementById("test_"+newtest).className = "paramselon";
    test = newtest;
    update_plot();
  }
}
function set_resource(newresource) {
  if (resource != newresource) {
    if (resource != null) document.getElementById("resource_"+resource).className = "paramsel";
    document.getElementById("resource_"+newresource).className = "paramselon";
    resource = newresource;
    update_plot();
  }
}
function init() {
  set_machine("${DEF_MACHINE}");
  set_mode("${DEF_MODE}");
  set_test("${DEF_TEST}");
  set_resource("${DEF_RESOURCE}");
}
</script>
</head>
<body onLoad="init()">
<h1>Abstract Machine Generation Versions</h1>
<h2>Benchmark Result Viewer</h2>
EOF
    # legend
    cat >> ${out} <<EOF 
<h4>Description</h4>
<p>This is the description of the options for each generated abstrat
machine. The Y axis represents the HO options, the point shape the LO
options, and the X axis the parameter to be compared
(e.g. speed-up). The plot legend uses the abbreviated notation, with a
'-' for disabled options and the option abbreviation character for
enabled options.
</p>
<pre>
EOF
    analyze_input | ./analyze --opts-legend >> ${out}
    cat >> ${out} <<EOF 
</pre>
EOF

    # Links to modes, means and tests
    cat >> ${out} <<EOF 
<h4>Links</h4>
<p>Click on a link to change the current plot.</p>
EOF
    cat >> ${out} <<EOF 
Machine:
EOF
    for h in ${MACHINES}; do 
	cat >> ${out} <<EOF 
<span id="machine_${h}" class="paramsel" onclick="set_machine('${h}')">${h}</span>
EOF
    done
    cat >> ${out} <<EOF 
<br/>
EOF
    cat >> ${out} <<EOF 
Mode:
EOF
    for h in ${MODES}; do 
	cat >> ${out} <<EOF 
<span id="mode_${h}" class="paramsel" onclick="set_mode('${h}')">${h}</span>
EOF
    done
    cat >> ${out} <<EOF 
<br/>
EOF
    cat >> ${out} <<EOF 
Mean:
EOF
    for h in ${MEANS}; do 
	cat >> ${out} <<EOF 
<span id="test_${h}" class="paramsel" onclick="set_test('${h}')">${h}</span>
EOF
    done
    cat >> ${out} <<EOF 
<br/>
EOF
    cat >> ${out} <<EOF 
Single test:
EOF
    for h in ${TESTS}; do 
	cat >> ${out} <<EOF 
<span id="test_${h}" class="paramsel" onclick="set_test('${h}')">${h}</span>
EOF
    done
    cat >> ${out} <<EOF 
<br/>
EOF
    cat >> ${out} <<EOF 
Resources:
EOF
    for h in ${RESOURCES}; do 
	cat >> ${out} <<EOF 
<span id="resource_${h}" class="paramsel" onclick="set_resource('${h}')">${h}</span>
EOF
    done
    cat >> ${out} <<EOF 
<br/>
EOF
    # Image and rest of page
    cat >> ${out} <<EOF 
<img id="plot"/>
</body>
</html>
EOF
}

help() {
    cat <<EOF
Usage: `basename ${0}` CONFIG INPUT OUTPUT [eps|png|html|all]

Where:
  CONFIG_config.txt is the configuration file
  INPUT is the data file

The output is generated in the '${outdir}' directory.
EOF
}

CONFIG=${1}
if [ -r ${CONFIG}_config.txt ]; then
    true
else
    help 
    exit -1
fi

input=${2}
if [ -r ${input} ]; then
    true
else
    help 
    exit -1
fi

outdir=${3}

case ${4} in
    plots) gen_plots; clean_analyze_program ;;
    html) gen_html; clean_analyze_program ;;
    *) help ;;
esac

