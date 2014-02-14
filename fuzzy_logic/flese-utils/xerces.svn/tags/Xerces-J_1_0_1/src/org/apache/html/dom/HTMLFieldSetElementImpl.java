package org.apache.html.dom;


import org.w3c.dom.*;
import org.w3c.dom.html.*;


/**
 * @version $Revision: 315119 $ $Date: 1999-12-14 22:27:05 +0100 (Tue, 14 Dec 1999) $
 * @author <a href="mailto:arkin@exoffice.com">Assaf Arkin</a>
 * @see org.w3c.dom.html.HTMLFieldSetElement
 * @see ElementImpl
 */
public final class HTMLFieldSetElementImpl
    extends HTMLElementImpl
    implements HTMLFieldSetElement, HTMLFormControl
{


      /**
     * Constructor requires owner document.
     * 
     * @param owner The owner HTML document
     */
    public HTMLFieldSetElementImpl( HTMLDocumentImpl owner, String name )
    {
        super( owner, name );
    }
  

}

