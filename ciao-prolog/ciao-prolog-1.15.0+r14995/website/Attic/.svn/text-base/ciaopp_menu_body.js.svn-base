//////////////////////
//    GLOBAL VARS   //
//////////////////////


var nc   = (document.layers) ? true:false ;
var ie   = (document.all) ? true:false ;
var n6   = (document.getElementById) ? true:false ;
var gebn = (document.getElementByName) ? true:false ;


//////////////////////
//  MENU QUESTION   //
//////////////////////


// menu question 
function menuq ( flag , title , options , def_opt , guard )
{
    // fields
    this.flag     = flag                 ;
    this.title    = title                ;
    this.options  = options.split( "," ) ;
    this.def_opt  = def_opt              ;
    this.guard    = guard                ;
    this.in_use   = true                 ;
    this.is_combo = (options != "int")   ;
    
    // methods
    this.display_menu              = display_menu            ;
    this.exec_guard                = exec_guard              ;
    this.activate                  = activate                ;
    this.get_combo                 = get_combo               ;
    this.get_text_field            = get_text_field          ;
    this.get_combo_title           = get_combo_title         ;
    this.fill_combo                = fill_combo              ;
    this.update_flag_value         = update_flag_value       ;
    this.update_flag_with_value    = update_flag_with_value  ;
    this.update_flag_with_index    = update_flag_with_index  ;
    this.set_flag_def_opt          = set_flag_def_opt        ;
    this.get_flag_value            = get_flag_value          ;
}


function display_menu ( )
{
   if ( this.is_combo )
       {
	   document.write( ' <div id="t_'  + this.flag + '">' ) ;
	   document.write( ' <div class="title" style="text-align:right;">' + this.title ) ;
	   document.write( ' <select name="' + this.flag + '" class="select" onchange="update_menu(this)"></select>\n</div>\n</div>\n' ) ;
       }
   else
       {
	   document.write( ' <div id="t_'  + this.flag + '">' ) ;
	   document.write( ' <div class="title"  style="text-align:right;">' + this.title ) ;
	   document.write( ' <input name="' + 
	                      this.flag + 
                              '" class="text" onchange="update_menu(this)" value="' + 
                              this.def_opt + 
                              '">\n</div>\n</div>\n' ) ;
       }
}


function exec_guard ( )
{
    this.in_use = eval( this.guard ) ;
}


function activate ( v )
{
   var combo       = this.get_combo( )       ;
   var combo_t     = this.get_combo_title( ) ;
   
   combo.disabled = !v ;
      
//   if ( document.styleSheets )
     {
	if ( !ie )
       	     combo_t.style.visibility = (v)? 'visible': 'hidden' ;
	
	combo_t.style.height     = (v)? "auto" : 0 + "px" ;
     }
}


function get_combo ( )
{
  if ( gebn )
    return document.getElementByName( this.flag ) ;
  else
    return eval( "document.frmMenu." + this.flag ) ;
}


function get_text_field ( )
{
  return this.get_combo( ) ;
}


function get_combo_title ( )
{
  if ( n6 )
    return document.getElementById( "t_" + this.flag ) ;
  else if(ie)
    return document.all[ "t_" + this.flag ] ;
  else
    return eval( "t_" + this.flag ) ;
}


function update_flag_value ( )
{
    this.update_flag_with_value( this.options[ this.get_combo( ).selectedIndex ] ) ;
}


function set_flag_def_opt ( )
{
    this.update_flag_with_value( this.def_opt ) ;
}


function update_flag_with_index ( i )
{
    this.update_flag_with_value( this.options[ i ] ) ;
}


function update_flag_with_value ( value )
{
    eval( 'v_' + this.flag + '= "' + value + '"' ) ;
}


function get_flag_value ( )
{
    return eval( 'v_' + this.flag ) ;
}


function fill_combo ( )
{
  //   if ( !this.in_use )
  //      return ;

   if ( this.is_combo )
     {
	 var combo = this.get_combo( ) ;

	 clear( combo ) ;
	 
	 for ( var o = 0  ;  o < this.options.length ; o ++ )
	    {
	       combo.options[ combo.length ] = new Option( this.options[ o ] , this.options[ o ] ) ;
	  
	       if ( this.options[ o ] == this.get_flag_value( ) )
		    combo.selectedIndex = o ;
	    }
     }
   else // It has to be a text box!!!
     {
	 var textf = this.get_text_field( ) ;
	
	 textf.value = this.def_opt ;
     }
}


function clear ( combo )
{
   for ( var c = combo.length  ;  c > 0 ; c -- )
         combo.options[ c ] = null ;
}


//////////////////////
// Normal functions //
//////////////////////

function fill_menu_combo_by_number ( m )
{
   menus[ m ].fill_combo( ) ;
}


function flag_to_number ( flag )
{
   return eval( flag ) ;
}


function fill_menu_combo_by_name ( combo_name )
{
   var menu_number = flag_to_number( combo_name ) ;
   
   fill_menu_combo_by_number( menu_number ) ;
}


function draw_menus ( )
{
   for ( var m = 0  ;  m < menus.length  ;  m ++ )
	 menus[ m ].display_menu( ) ;
}


function fill_all_menu_combos ( )
{
   for ( var m = 0  ;  m < menus.length  ;  m ++ )  
     {
         fill_menu_combo_by_number( m ) ;
	 menus[ m ].activate( menus[ m ].in_use ) ;
     }
}


function exec_menu_guards ( )
{
   for ( var m = 0  ;  m < menus.length  ;  m ++ )
	 menus[ m ].exec_guard( ) ;
}


function update_def_values ( )
{
   for ( var m = 0  ;  m < menus.length  ;  m ++ )
         menus[ m ].set_flag_def_opt( ) ;
}


//////////////////////
//      HOOKS       //
//////////////////////

function update_menu ( combo , index )
{
   var cm = flag_to_number( combo.name ) ;
   
   menus[ cm ].update_flag_with_index( combo.selectedIndex ) ;

 
   for ( var m = 0  ;  m < menus.length  ;  m ++ )
      {
  	 var b4 = menus[ m ].in_use ; 
	 
	 menus[ m ].exec_guard( ) ;
	 
	 if ( b4 != menus[ m ].in_use )
	   {
	     //	     alert( menus[ m ].title + " changed" ) ;
              menus[ m ].activate( !b4 ) ;
	   }
      }
}



function submit_everything ( )
{
   for ( var m = 0  ;  m < menus.length  ;  m ++ )
	 menus[ m ].activate( true ) ;
	//get_combo( ).disabled = false ;
   
   return true ;
}


//////////////////////
//       INIT       //
//////////////////////

// CAUTION: Combos are not generated in this point, 
//          so we cannot access them.
function init ( )
{
   // we do have to init flags with defaults values
   update_def_values( ) ;
   
   // Lets see which menus have to be displayed
   exec_menu_guards( ) ;
}
