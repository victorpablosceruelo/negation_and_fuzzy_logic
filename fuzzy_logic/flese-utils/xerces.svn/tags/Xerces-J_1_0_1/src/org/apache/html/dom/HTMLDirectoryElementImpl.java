package org.apache.html.dom;


import org.w3c.dom.*;
import org.w3c.dom.html.*;


/**
 * @version $Revision: 315119 $ $Date: 1999-12-14 22:27:05 +0100 (Tue, 14 Dec 1999) $
 * @author <a href="mailto:arkin@exoffice.com">Assaf Arkin</a>
 * @see org.w3c.dom.html.HTMLDirectoryElement
 * @see ElementImpl
 */
public final class HTMLDirectoryElementImpl
    extends HTMLElementImpl
    implements HTMLDirectoryElement
{
    
    
    public boolean getCompact()
    {
        return getBinary( "compact" );
    }
    
    
    public void setCompact( boolean compact )
    {
        setAttribute( "compact", compact );
    }
  

      /**
     * Constructor requires owner document.
     * 
     * @param owner The owner HTML document
     */
    public HTMLDirectoryElementImpl( HTMLDocumentImpl owner, String name )
    {
        super( owner, name );
    }


}

