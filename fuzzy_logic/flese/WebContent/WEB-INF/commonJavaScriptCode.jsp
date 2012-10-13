
<script type="text/javascript">
function init_callback_inpage() {
  function debug_callback( level ) { 
    var args = Array.prototype.slice.call( arguments, 1 ); 
    $('#debug').length || $('<div id="debug"><h2>debug output<\/h2><\/div>').appendTo( 'body' ); 
    $('<div/>') 
      .addClass( 'debug-' + level ) 
      .html( '[' + level + '] ' + args ) 
      .appendTo( '#debug' ); 
  };
  debug.setCallback( debug_callback, true );
}

function init_callback_firebuglite() {
  if ( !window.firebug ) {
    
    // from firebug lite bookmarklet
    window.firebug = document.createElement('script');
    firebug.setAttribute( 'src', 'http://getfirebug.com/releases/lite/1.2/firebug-lite-compressed.js' );
    document.body.appendChild( firebug );
    (function(){
      if ( window.firebug.version ) {
        firebug.init();
      } else {
        setTimeout( arguments.callee );
      }
    })();
    void( firebug );
    
    if ( window.debug && debug.setCallback ) {
      (function(){
        if ( window.firebug && window.firebug.version ) {
          debug.setCallback(function( level ) {
            var args = Array.prototype.slice.call( arguments, 1 );
            firebug.d.console.cmd[level].apply( window, args );
          }, true);
        } else {
          setTimeout( arguments.callee, 100);
        }
      })();
    }
  }
}

function showInfoMessage(divName,msg){
	// document.getElementById(divName);
	if (msg=="") msg = "&nbsp;";
	document.getElementById(divName).innerHTML = msg;
	//messageBox.style.top=obj.offsetTop;
	//messageBox.style.left=obj.offsetLeft+obj.offsetWidth+5;
	//contents.innerHTML=msg+"<p>"+obj.href;
	//messageBox.style.display="block";
}

var clientSoftware = "";
var clientSoftwareVersion = "";

function detectClientSoftware() {
	if (/Firefox[\/\s](\d+\.\d+)/.test(navigator.userAgent)){ //test for Firefox/x.x or Firefox x.x (ignoring remaining digits);
		clientSoftware="firefox";
		clientSoftwareVersion=new Number(RegExp.$1); // capture x.x portion and store as a number
	}
	else {
		if (/MSIE (\d+\.\d+);/.test(navigator.userAgent)){ //test for MSIE x.x;
			clientSoftware="msie";
			clientSoftwareVersion=new Number(RegExp.$1); // capture x.x portion and store as a number
		}
		else {
			//Note: userAgent in Opera9.24 WinXP returns: Opera/9.24 (Windows NT 5.1; U; en)
			//      userAgent in Opera 8.5 (identified as IE) returns: Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1) Opera 8.50 [en]
			//      userAgent in Opera 8.5 (identified as Opera) returns: Opera/8.50 (Windows NT 5.1; U) [en]

			if (/Opera[\/\s](\d+\.\d+)/.test(navigator.userAgent)){ //test for Opera/x.x or Opera x.x (ignoring remaining decimal places);
				clientSoftware="opera";
				clientSoftwareVersion=new Number(RegExp.$1); // capture x.x portion and store as a number
			}
		}
	}
}
</script>
