
<%@page import="auxiliar.ServletsAuxMethodsClass"%>
<%@page import="auxiliar.FunctionsClass"%>

<script type="text/javascript" src="js/highcharts.js" ></script>

<script type="text/javascript">

	/*!
	 * JavaScript Personalization - FleSe
	 * 
	 * https://moises.ls.fi.upm.es/java-apps/flese/
	 * 
	 */

	// Script: Personalize.js
	
	function fuzzificationDef(predDefined, predNecessary, functionsArray) {
		this.predDefined = predDefined;
		this.predNecessary = predNecessary;
		this.functionsArray = functionsArray;
	}
	
	function fuzzificationFunctionDef(name, data) {
		this.name = name; // this is the function owner.
		this.data = data; // this are the functions points (an array).
	}

	fuzzificationDefs = new Array();
	<%
		String filePath = (String) request.getAttribute("filePath");
		String fuzzification = (String) request.getAttribute("fuzzification");
		if (filePath != null) request.removeAttribute("filePath");
		if (fuzzification != null) request.removeAttribute("fuzzification");
		
		
		FunctionsClass functions = new FunctionsClass(filePath, fuzzification);
		String [] functionsInJS = functions.getResultInJavaScript();
		for (int i=0; i<functionsInJS.length; i++) {
			out.println("fuzzificationDefs["+i+"]= " + functionsInJS[i] + ";");
		}
		
	%>
	
	var fileName = "<%= (String) request.getAttribute("fileName") %>";
	var fileOwner = "<%= (String) request.getAttribute("fileOwner") %>";
	var fuzzification = "<%= fuzzification %>";

	var personalizeServletEditAction="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.PersonalizeServletEditAction, request, null)%>";
	var personalizeServletSaveAction = "<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.PersonalizeServletSaveAction, request, null) %>";

	
	var charts = new Array(); // globally available
		
	function drawChart(identifier, index) {
		
		if ((fuzzificationDefs[index] != null) && (fuzzificationDefs[index].functionsArray != null)) {
	
			$(document).ready(function() {
			      charts[i] = new Highcharts.Chart({
			         chart: {
		    	        renderTo: identifier,
		        	    type: 'line',
						style: {
							margin: '0 auto'
						}
			         },
			         title: {
		    	        text: fuzzificationDefs[index].predDefined
		        	 },
			         xAxis: {
						title: {
							text: 'value of ' + fuzzificationDefs[index].predNecessary
						},
						min: 0

			            // categories: ['Apples', 'Bananas', 'Oranges']
			         },
			         yAxis: {
						title: {
							text: 'Truth value'
						},
						min: 0,
						max: 1
			         	// categories: [0, 0.25, 0.5, 0.75, 1]
		    	     },
		        	 series: fuzzificationDefs[index].functionsArray
			         		/*	[{
			            name: 'Jane',
			            data: [1, 0, 4]
		    	     }, {
		        	    name: 'John',
		            	data: [5, 7, 3]
			         }] */
			      });
			   });
		}
	}

	/*
	function drawChart(i) {
		var myFunction = personalizePredInfo[i][3];
		
		// $.jqplot('chartDiv_' + i,  [[[1, 2],[3,5.12],[5,13.1],[7,33.6],[9,85.9],[11,219.9]]]);
		$.jqplot('chartDiv_' + i,  [myFunction], {
		      // Give the plot a title.
		      // title: '',
		      // An axes object holds options for all axes.
		      // Allowable axes are xaxis, x2axis, yaxis, y2axis, y3axis, ...
		      // Up to 9 y axes are supported.
		      axes: {
		        // options for each axis are specified in seperate option objects.
		        xaxis: {
		          label: personalizePredInfo[i][1],
		          // Turn off "padding".  This will allow data point to lie on the
		          // edges of the grid.  Default padding is 1.2 and will keep all
		          // points inside the bounds of the grid.
		          pad: 0
		        },
		        yaxis: {
		          label: personalizePredInfo[i][0],
		          labelRenderer: $.jqplot.CanvasAxisLabelRenderer
		        }
		      },
			  highlighter: {
			       show: true,
			       sizeAdjust: 7.5
			  },
			  cursor: {
			       show: false
			  }
		});
	}
	*/
	
</script>
	
	