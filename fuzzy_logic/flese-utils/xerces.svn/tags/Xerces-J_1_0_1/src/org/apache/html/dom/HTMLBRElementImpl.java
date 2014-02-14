package org.apache.html.dom;


import org.w3c.dom.*;
import org.w3c.dom.html.*;


/**
 * @version $Revision: 315119 $ $Date: 1999-12-14 22:27:05 +0100 (Tue, 14 Dec 1999) $
 * @author <a href="mailto:arkin@exoffice.com">Assaf Arkin</a>
 * @see org.w3c.dom.html.HTMLBRElement
 * @see ElementImpl
 */
public final class HTMLBRElementImpl
    extends HTMLElementImpl
    implements HTMLBRElement
{

    
    public String getClear()
    {
        return capitalize( getAttribute( "clear" ) );
    }
    
    
    public void setClear( String clear )
    {
        setAttribute( "clear", clear );
    }

    
    /**
     * Constructor requires owner document.
     * 
     * @param owner The owner HTML document
     */
    public HTMLBRElementImpl( HTMLDocumentImpl owner, String name )
    {
        super( owner, name );
    }


}

