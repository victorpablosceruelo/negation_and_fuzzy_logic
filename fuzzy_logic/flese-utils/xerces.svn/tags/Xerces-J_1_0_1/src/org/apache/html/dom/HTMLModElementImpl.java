package org.apache.html.dom;


import org.w3c.dom.*;
import org.w3c.dom.html.*;


/**
 * @version $Revision: 315119 $ $Date: 1999-12-14 22:27:05 +0100 (Tue, 14 Dec 1999) $
 * @author <a href="mailto:arkin@exoffice.com">Assaf Arkin</a>
 * @see org.w3c.dom.html.HTMLModElement
 * @see ElementImpl
 */
public final class HTMLModElementImpl
    extends HTMLElementImpl
    implements HTMLModElement
{

    
    
    public String getCite()
    {
        return getAttribute( "cite" );
    }
    
    
    public void setCite( String cite )
    {
        setAttribute( "cite", cite );
    }
    
    
      public String getDateTime()
    {
        return getAttribute( "datetime" );
    }
    
    
    public void setDateTime( String dateTime )
    {
        setAttribute( "datetime", dateTime );
    }
    

    /**
     * Constructor requires owner document and tag name.
     * 
     * @param owner The owner HTML document
     */
    public HTMLModElementImpl( HTMLDocumentImpl owner, String name )
    {
        super( owner, name );
    }


}

