
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

	personalizePredInfo = new Array();
	<%
		String filePath = (String) request.getAttribute("filePath");
		String fuzzification = (String) request.getAttribute("fuzzification");
		
		FunctionsClass functions = new FunctionsClass(filePath);
		String [] functionsInJS = functions.getResultInJavaScript();
		for (int i=0; i<functionsInJS.length; i++) {
			out.println("personalizePredInfo["+i+"]= " + functionsInJS[i] + ";");
		}
		
	%>

	var personalizeServlet="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.PersonalizeServletEditAction, request, null)%>";


	var functionValues = new Array();
	var charts = new Array(); // globally available
	
	
	
	function drawChart(identifier, index) {
		var series = new Array();
		
		if ((personalizePredInfo[index] != null) && (personalizePredInfo[index][2] != null)) {
			for (var i=0; i<personalizePredInfo[index][2].length; i++) {
				series[i] = new Object();
				series[i].name = personalizePredInfo[index][2][i][0];
				series[i].data = personalizePredInfo[index][2][i][1];
			}
		}
		
		$(document).ready(function() {
		      charts[i] = new Highcharts.Chart({
		         chart: {
		            renderTo: identifier,
		            type: 'bar'
		         },
		         title: {
		            text: personalizePredInfo[index][0]
		         },
		         xAxis: {
		            // categories: ['Apples', 'Bananas', 'Oranges']
		         },
		         yAxis: {
		            /* title: {
		               text: 'Fruit eaten'
		            } */
		         },
		         series: series
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
	
	