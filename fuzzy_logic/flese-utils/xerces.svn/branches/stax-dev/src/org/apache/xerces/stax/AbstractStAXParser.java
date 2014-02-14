/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.xerces.stax;

import java.util.Stack;

import javax.xml.stream.XMLStreamConstants;

import org.apache.xerces.parsers.AbstractXMLDocumentParser;
import org.apache.xerces.parsers.XML11Configuration;
import org.apache.xerces.xni.Augmentations;
import org.apache.xerces.xni.NamespaceContext;
import org.apache.xerces.xni.QName;
import org.apache.xerces.xni.XMLAttributes;
import org.apache.xerces.xni.XMLLocator;
import org.apache.xerces.xni.XMLResourceIdentifier;
import org.apache.xerces.xni.XMLString;
import org.apache.xerces.xni.XNIException;
import org.apache.xerces.xni.parser.XMLEntityResolver;
import org.apache.xerces.xni.parser.XMLErrorHandler;

/**
 * @xerces.internal
 * 
 * @author Wei Duan
 * 
 * @version $Id: AbstractStAXParser.java 730259 2008-12-30 21:55:11Z mrglavas $
 */
public class AbstractStAXParser extends AbstractXMLDocumentParser {

	// The current event type
	protected int eventType;

	// Document encoding style
	protected String encodingDoc = null;

	// XML document version
	protected String versionXML = null;

	// XML encoding style
	protected String encodingXML = null;

	// Standalone description
	protected String standaloneXML = null;

	// Process Instruction target
	protected String piTarget = null;
    
    // Name of entity referrence
    protected String entityReferrenceName = null;

	// Character content
	protected XMLString characters = null;

	// Process Instruction data
	protected XMLString piData = null;

	// Comment data
	protected XMLString comment = null;

	// Element QName
	protected QName elementName = null;

	// Element attribute stack
    // The XNI interface "endElement" will not give out XMLAttributes info.
    // However, stax inteface needs to give namespace information when
    // START_ELEMENT or END_ELEMENT��
    protected Stack atrributeStack = new Stack();
    
	protected XMLAttributes curElementAttr = null;

	// Namespace context
	protected NamespaceContext namespaceContext = null;

	// XML locator
	protected XMLLocator locator = null;

	//
	// Constructors
	//
	/** Default constructor. */
	protected AbstractStAXParser(XML11Configuration configuration) {
		super(configuration);

		// TODO: Investigate and make clear StAX related features
		configuration.setFeature(
				"http://apache.org/xml/features/allow-java-encodings", true);

	} // <init>(XML11Configuration)

	/**
	 * reset all components before parsing
	 */
	protected void reset() throws XNIException {
		super.reset();

		versionXML = null;
		encodingXML = null;
		standaloneXML = null;
		characters = null;
		piTarget = null;
		piData = null;
        entityReferrenceName = null;
		elementName = null;
		curElementAttr = null;
		namespaceContext = null;
        atrributeStack.clear();
	} // reset()

    /**
     * Allow an application to register an error event handler.
     *
     * @param errorHandler The error handler.
     */
    public void setErrorHandler(XMLErrorHandler errorHandler) {
        if (errorHandler != null)
        {
            fConfiguration.setProperty(ERROR_HANDLER, errorHandler);
        }
    } 
    
    /**
     * Allow an application to register an entity resolver handler.
     *
     * @param resolver The Entity Resolver
     */
    public void setEntityResolver(XMLEntityResolver resolver) {
        if (resolver != null)
        {
            fConfiguration.setProperty(ENTITY_RESOLVER, resolver);
        }
    } 
    
	//
	// XMLDocumentHandler methods
	//

	/**
	 * The start of the document.
	 * 
	 * @param locator
	 *            The system identifier of the entity if the entity is external,
	 *            null otherwise.
	 * @param encoding
	 *            The auto-detected IANA encoding name of the entity stream.
	 *            This value will be null in those situations where the entity
	 *            encoding is not auto-detected (e.g. internal entities or a
	 *            document entity that is parsed from a java.io.Reader).
	 * @param namespaceContext
	 *            The namespace context in effect at the start of this document.
	 *            This object represents the current context. Implementors of
	 *            this class are responsible for copying the namespace bindings
	 *            from the the current context (and its parent contexts) if that
	 *            information is important.
	 * @param augs
	 *            Additional information that may include infoset augmentations
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */

	public void startDocument(XMLLocator locator, String encoding,
			NamespaceContext namespaceContext, Augmentations augs)
			throws XNIException {
		eventType = XMLStreamConstants.START_DOCUMENT;
		this.locator = locator;
		this.namespaceContext = namespaceContext;
		this.encodingDoc = encoding;
	} // startDocument(XMLLocator,String)

	/**
	 * Notifies of the presence of an XMLDecl line in the document. If present,
	 * this method will be called immediately following the startDocument call.
	 * 
	 * @param version
	 *            The XML version.
	 * @param encoding
	 *            The IANA encoding name of the document, or null if not
	 *            specified.
	 * @param standalone
	 *            The standalone value, or null if not specified.
	 * @param augs
	 *            Additional information that may include infoset augmentations
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */
	public void xmlDecl(String version, String encoding, String standalone,
			Augmentations augs) throws XNIException {
		this.versionXML = version;
		this.encodingXML = encoding;
		this.standaloneXML = standalone;
	} // xmlDecl(String,String,String)

	/**
	 * Notifies of the presence of the DOCTYPE line in the document.
	 * 
	 * @param rootElement
	 *            The name of the root element.
	 * @param publicId
	 *            The public identifier if an external DTD or null if the
	 *            external DTD is specified using SYSTEM.
	 * @param systemId
	 *            The system identifier if an external DTD, null
	 * @param augs
	 *            Additional information that may include infoset augmentations
	 *            otherwise.
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */
	public void doctypeDecl(String rootElement, String publicId,
			String systemId, Augmentations augs) throws XNIException {
        this.eventType = XMLStreamConstants.DTD;
	} // doctypeDecl(String,String,String)

	/**
	 * The start of an element. If the document specifies the start element by
	 * using an empty tag, then the startElement method will immediately be
	 * followed by the endElement method, with no intervening methods.
	 * 
	 * @param element
	 *            The name of the element.
	 * @param attributes
	 *            The element attributes.
	 * @param augs
	 *            Additional information that may include infoset augmentations
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */
	public void startElement(QName element, XMLAttributes attributes,
			Augmentations augs) throws XNIException {
		eventType = XMLStreamConstants.START_ELEMENT;

		this.elementName = element;
        
        this.curElementAttr = attributes;
		this.atrributeStack.push(attributes);
	} // startElement(QName,XMLAttributes)

	/**
	 * An empty element.
	 * 
	 * @param element
	 *            The name of the element.
	 * @param attributes
	 *            The element attributes.
	 * @param augs
	 *            Additional information that may include infoset augmentations
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */
	public void emptyElement(QName element, XMLAttributes attributes,
			Augmentations augs) throws XNIException {

		// Assume that type of empty element is START_ELEMENT
		eventType = XMLStreamConstants.START_ELEMENT;

		startElement(element, attributes, augs);
		endElement(element, augs);
	} // emptyElement(QName,XMLAttributes)

	/**
	 * Character content.
	 * 
	 * @param text
	 *            The content.
	 * @param augs
	 *            Additional information that may include infoset augmentations
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */
	public void characters(XMLString text, Augmentations augs)
			throws XNIException {
		eventType = XMLStreamConstants.CHARACTERS;
		this.characters = text;
	} // characters(XMLString)

	/**
	 * Ignorable whitespace. For this method to be called, the document source
	 * must have some way of determining that the text containing only
	 * whitespace characters should be considered ignorable. For example, the
	 * validator can determine if a length of whitespace characters in the
	 * document are ignorable based on the element content model.
	 * 
	 * @param text
	 *            The ignorable whitespace.
	 * @param augs
	 *            Additional information that may include infoset augmentations
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */
	public void ignorableWhitespace(XMLString text, Augmentations augs)
			throws XNIException {
		eventType = XMLStreamConstants.SPACE;
		this.characters = text;
	} // ignorableWhitespace(XMLString)

	/**
	 * The end of an element.
	 * 
	 * @param element
	 *            The name of the element.
	 * @param augs
	 *            Additional information that may include infoset augmentations
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */
	public void endElement(QName element, Augmentations augs)
			throws XNIException {
		eventType = XMLStreamConstants.END_ELEMENT;

		this.elementName = element;
        
        // The endElement and startElement are in pair
        this.curElementAttr = (XMLAttributes) this.atrributeStack.pop();
    } // endElement(QName)

	/**
	 * The start of a CDATA section.
	 * 
	 * @param augs
	 *            Additional information that may include infoset augmentations
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */
	public void startCDATA(Augmentations augs) throws XNIException {
		eventType = XMLStreamConstants.CDATA;
	} // startCDATA()

	/**
	 * The end of a CDATA section.
	 * 
	 * @param augs
	 *            Additional information that may include infoset augmentations
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */
	public void endCDATA(Augmentations augs) throws XNIException {
		eventType = XMLStreamConstants.CDATA;
	} // endCDATA()

	/**
	 * The end of the document.
	 * 
	 * @param augs
	 *            Additional information that may include infoset augmentations
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */
	public void endDocument(Augmentations augs) throws XNIException {
		eventType = XMLStreamConstants.END_DOCUMENT;
	} // endDocument()

	/**
	 * This method notifies the start of an entity.
	 * <p>
	 * <strong>Note:</strong> This method is not called for entity references
	 * appearing as part of attribute values.
	 * 
	 * @param name
	 *            The name of the entity.
	 * @param identifier
	 *            The resource identifier.
	 * @param encoding
	 *            The auto-detected IANA encoding name of the entity stream.
	 *            This value will be null in those situations where the entity
	 *            encoding is not auto-detected (e.g. internal entities or a
	 *            document entity that is parsed from a java.io.Reader).
	 * @param augs
	 *            Additional information that may include infoset augmentations
	 * 
	 * @exception XNIException
	 *                Thrown by handler to signal an error.
	 */
	public void startGeneralEntity(String name,
			XMLResourceIdentifier identifier, String encoding,
			Augmentations augs) throws XNIException {
        eventType = XMLStreamConstants.ENTITY_REFERENCE;
        this.entityReferrenceName = name;
	} // startGeneralEntity(String,XMLResourceIdentifier,String,Augmentations)

	/**
	 * Notifies of the presence of a TextDecl line in an entity. If present,
	 * this method will be called immediately following the startEntity call.
	 * <p>
	 * <strong>Note:</strong> This method will never be called for the document
	 * entity; it is only called for external general entities referenced in
	 * document content.
	 * <p>
	 * <strong>Note:</strong> This method is not called for entity references
	 * appearing as part of attribute values.
	 * 
	 * @param version
	 *            The XML version, or null if not specified.
	 * @param encoding
	 *            The IANA encoding name of the entity.
	 * @param augs
	 *            Additional information that may include infoset augmentations
	 * 
	 * @exception XNIException
	 *                Thrown by handler to signal an error.
	 */
	public void textDecl(String version, String encoding, Augmentations augs)
			throws XNIException {
	} // textDecl(String, String, Augmentations)

	/**
	 * This method notifies the end of an entity.
	 * <p>
	 * <strong>Note:</strong> This method is not called for entity references
	 * appearing as part of attribute values.
	 * 
	 * @param name
	 *            The name of the entity.
	 * @param augs
	 *            Additional information that may include infoset augmentations
	 * 
	 * @exception XNIException
	 *                Thrown by handler to signal an error.
	 */
	public void endGeneralEntity(String name, Augmentations augs)
			throws XNIException {
        eventType = XMLStreamConstants.ENTITY_REFERENCE;
	} // endGeneralEntity(String,Augmentations)

	/**
	 * A comment.
	 * 
	 * @param text
	 *            The text in the comment.
	 * @param augs
	 *            Additional information that may include infoset augmentations
	 * 
	 * @exception XNIException
	 *                Thrown by application to signal an error.
	 */
	public void comment(XMLString text, Augmentations augs) throws XNIException {
		eventType = XMLStreamConstants.COMMENT;
		this.comment = text;
	} // comment (XMLString, Augmentations)

	/**
	 * A processing instruction. Processing instructions consist of a target
	 * name and, optionally, text data. The data is only meaningful to the
	 * application.
	 * <p>
	 * Typically, a processing instruction's data will contain a series of
	 * pseudo-attributes. These pseudo-attributes follow the form of element
	 * attributes but are <strong>not</strong> parsed or presented to the
	 * application as anything other than text. The application is responsible
	 * for parsing the data.
	 * 
	 * @param target
	 *            The target.
	 * @param data
	 *            The data or null if none specified.
	 * @param augs
	 *            Additional information that may include infoset augmentations
	 * 
	 * @exception XNIException
	 *                Thrown by handler to signal an error.
	 */
	public void processingInstruction(String target, XMLString data,
			Augmentations augs) throws XNIException {
		eventType = XMLStreamConstants.PROCESSING_INSTRUCTION;

		this.piData = data;
		this.piTarget = target;
	} // processingInstruction(String, XMLString, Augmentations)

	//
	// XMLDTDHandler methods
	//

	/**
	 * The start of the DTD.
	 * 
	 * @param locator
	 *            The document locator, or null if the document location cannot
	 *            be reported during the parsing of the document DTD. However,
	 *            it is <em>strongly</em> recommended that a locator be
	 *            supplied that can at least report the base system identifier
	 *            of the DTD.
	 * @param augs
	 *            Additional information that may include infoset augmentations.
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */
	public void startDTD(XMLLocator locator, Augmentations augs)
			throws XNIException {
		fInDTD = true;
	} // startDTD(XMLLocator)

	/**
	 * The start of the DTD external subset.
	 * 
	 * @param augmentations
	 *            Additional information that may include infoset augmentations.
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */
	public void startExternalSubset(XMLResourceIdentifier identifier,
			Augmentations augmentations) throws XNIException {
	} // startExternalSubset(Augmentations)

	/**
	 * The end of the DTD external subset.
	 * 
	 * @param augmentations
	 *            Additional information that may include infoset augmentations.
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */
	public void endExternalSubset(Augmentations augmentations)
			throws XNIException {
	} // endExternalSubset(Augmentations)

	/**
	 * This method notifies the start of an entity.
	 * <p>
	 * <strong>Note:</strong> This method is not called for entity references
	 * appearing as part of attribute values.
	 * 
	 * @param name
	 *            The name of the entity.
	 * @param identifier
	 *            The resource identifier.
	 * @param encoding
	 *            The auto-detected IANA encoding name of the entity stream.
	 *            This value will be null in those situations where the entity
	 *            encoding is not auto-detected (e.g. internal entities or a
	 *            document entity that is parsed from a java.io.Reader).
	 * @param augs
	 *            Additional information that may include infoset augmentations
	 * 
	 * @exception XNIException
	 *                Thrown by handler to signal an error.
	 */
	public void startParameterEntity(String name,
			XMLResourceIdentifier identifier, String encoding,
			Augmentations augs) throws XNIException {
        eventType = XMLStreamConstants.ENTITY_DECLARATION;
	} // startParameterEntity(String,XMLResourceIdentifier,String,Augmentations)

	/**
	 * This method notifies the end of an entity.
	 * <p>
	 * <strong>Note:</strong> This method is not called for entity references
	 * appearing as part of attribute values.
	 * 
	 * @param name
	 *            The name of the entity.
	 * @param augs
	 *            Additional information that may include infoset augmentations
	 * 
	 * @exception XNIException
	 *                Thrown by handler to signal an error.
	 */
	public void endParameterEntity(String name, Augmentations augs)
			throws XNIException {
        eventType = XMLStreamConstants.ENTITY_DECLARATION;
	} // endParameterEntity(String,Augmentations)

	/**
	 * Characters within an IGNORE conditional section.
	 * 
	 * @param text
	 *            The ignored text.
	 * @param augs
	 *            Additional information that may include infoset augmentations.
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */
	public void ignoredCharacters(XMLString text, Augmentations augs)
			throws XNIException {
	} // ignoredCharacters(XMLString, Augmentations)

	/**
	 * An element declaration.
	 * 
	 * @param name
	 *            The name of the element.
	 * @param contentModel
	 *            The element content model.
	 * @param augs
	 *            Additional information that may include infoset augmentations.
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */
	public void elementDecl(String name, String contentModel, Augmentations augs)
			throws XNIException {
	} // elementDecl(String,String)

	/**
	 * The start of an attribute list.
	 * 
	 * @param elementName
	 *            The name of the element that this attribute list is associated
	 *            with.
	 * @param augs
	 *            Additional information that may include infoset augmentations.
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */
	public void startAttlist(String elementName, Augmentations augs)
			throws XNIException {      
	} // startAttlist(String)

	/**
	 * An attribute declaration.
	 * 
	 * @param elementName
	 *            The name of the element that this attribute is associated
	 *            with.
	 * @param attributeName
	 *            The name of the attribute.
	 * @param type
	 *            The attribute type. This value will be one of the following:
	 *            "CDATA", "ENTITY", "ENTITIES", "ENUMERATION", "ID", "IDREF",
	 *            "IDREFS", "NMTOKEN", "NMTOKENS", or "NOTATION".
	 * @param enumeration
	 *            If the type has the value "ENUMERATION" or "NOTATION", this
	 *            array holds the allowed attribute values; otherwise, this
	 *            array is null.
	 * @param defaultType
	 *            The attribute default type. This value will be one of the
	 *            following: "#FIXED", "#IMPLIED", "#REQUIRED", or null.
	 * @param defaultValue
	 *            The attribute default value, or null if no default value is
	 *            specified.
	 * @param nonNormalizedDefaultValue
	 *            The attribute default value with no normalization performed,
	 *            or null if no default value is specified.
	 * @param augs
	 *            Additional information that may include infoset augmentations.
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */
	public void attributeDecl(String elementName, String attributeName,
			String type, String[] enumeration, String defaultType,
			XMLString defaultValue, XMLString nonNormalizedDefaultValue,
			Augmentations augs) throws XNIException {
	} // attributeDecl(String,String,String,String[],String,XMLString,
		// XMLString, Augmentations)

	/**
	 * The end of an attribute list.
	 * 
	 * @param augs
	 *            Additional information that may include infoset augmentations.
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */
	public void endAttlist(Augmentations augs) throws XNIException {
	} // endAttlist()

	/**
	 * An internal entity declaration.
	 * 
	 * @param name
	 *            The name of the entity. Parameter entity names start with '%',
	 *            whereas the name of a general entity is just the entity name.
	 * @param text
	 *            The value of the entity.
	 * @param nonNormalizedText
	 *            The non-normalized value of the entity. This value contains
	 *            the same sequence of characters that was in the internal
	 *            entity declaration, without any entity references expanded.
	 * @param augs
	 *            Additional information that may include infoset augmentations.
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */
	public void internalEntityDecl(String name, XMLString text,
			XMLString nonNormalizedText, Augmentations augs)
			throws XNIException {
        eventType = XMLStreamConstants.ENTITY_DECLARATION;
	} // internalEntityDecl(String,XMLString,XMLString)

	/**
	 * An external entity declaration.
	 * 
	 * @param name
	 *            The name of the entity. Parameter entity names start with '%',
	 *            whereas the name of a general entity is just the entity name.
	 * @param identifier
	 *            An object containing all location information pertinent to
	 *            this entity.
	 * @param augs
	 *            Additional information that may include infoset augmentations.
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */
	public void externalEntityDecl(String name,
			XMLResourceIdentifier identifier, Augmentations augs)
			throws XNIException {
        eventType = XMLStreamConstants.ENTITY_DECLARATION;
	} // externalEntityDecl(String,XMLResourceIdentifier, Augmentations)

	/**
	 * An unparsed entity declaration.
	 * 
	 * @param name
	 *            The name of the entity.
	 * @param identifier
	 *            An object containing all location information pertinent to
	 *            this entity.
	 * @param notation
	 *            The name of the notation.
	 * @param augs
	 *            Additional information that may include infoset augmentations.
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */
	public void unparsedEntityDecl(String name,
			XMLResourceIdentifier identifier, String notation,
			Augmentations augs) throws XNIException {
        eventType = XMLStreamConstants.ENTITY_DECLARATION;
	} // unparsedEntityDecl(String,XMLResourceIdentifier, String,
		// Augmentations)

	/**
	 * A notation declaration
	 * 
	 * @param name
	 *            The name of the notation.
	 * @param identifier
	 *            An object containing all location information pertinent to
	 *            this notation.
	 * @param augs
	 *            Additional information that may include infoset augmentations.
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */
	public void notationDecl(String name, XMLResourceIdentifier identifier,
			Augmentations augs) throws XNIException {
        eventType = XMLStreamConstants.NOTATION_DECLARATION;
	} // notationDecl(String,XMLResourceIdentifier, Augmentations)

	/**
	 * The start of a conditional section.
	 * 
	 * @param type
	 *            The type of the conditional section. This value will either be
	 *            CONDITIONAL_INCLUDE or CONDITIONAL_IGNORE.
	 * @param augs
	 *            Additional information that may include infoset augmentations.
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 * 
	 * @see #CONDITIONAL_INCLUDE
	 * @see #CONDITIONAL_IGNORE
	 */
	public void startConditional(short type, Augmentations augs)
			throws XNIException {
	} // startConditional(short)

	/**
	 * The end of a conditional section.
	 * 
	 * @param augs
	 *            Additional information that may include infoset augmentations.
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */
	public void endConditional(Augmentations augs) throws XNIException {
	} // endConditional()

	//
	// XMLDTDContentModelHandler methods
	//

	/**
	 * The start of a content model. Depending on the type of the content model,
	 * specific methods may be called between the call to the startContentModel
	 * method and the call to the endContentModel method.
	 * 
	 * @param elementName
	 *            The name of the element.
	 * @param augs
	 *            Additional information that may include infoset augmentations.
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */
	public void startContentModel(String elementName, Augmentations augs)
			throws XNIException {
	} // startContentModel(String, Augmentations)

	/**
	 * A content model of ANY.
	 * 
	 * @param augs
	 *            Additional information that may include infoset augmentations.
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 * 
	 * @see #empty
	 * @see #startGroup
	 */
	public void any(Augmentations augs) throws XNIException {
	} // any(Augmentations)

	/**
	 * A content model of EMPTY.
	 * 
	 * @param augs
	 *            Additional information that may include infoset augmentations.
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 * 
	 * @see #any
	 * @see #startGroup
	 */
	public void empty(Augmentations augs) throws XNIException {
	} // empty(Augmentations)

	/**
	 * A start of either a mixed or children content model. A mixed content
	 * model will immediately be followed by a call to the <code>pcdata()</code>
	 * method. A children content model will contain additional groups and/or
	 * elements.
	 * 
	 * @param augs
	 *            Additional information that may include infoset augmentations.
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 * 
	 * @see #any
	 * @see #empty
	 */
	public void startGroup(Augmentations augs) throws XNIException {
	} // stargGroup(Augmentations)

	/**
	 * The appearance of "#PCDATA" within a group signifying a mixed content
	 * model. This method will be the first called following the content model's
	 * <code>startGroup()</code>.
	 * 
	 * @param augs
	 *            Additional information that may include infoset augmentations.
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 * 
	 * @see #startGroup
	 */
	public void pcdata(Augmentations augs) throws XNIException {
	} // pcdata(Augmentations)

	/**
	 * A referenced element in a mixed or children content model.
	 * 
	 * @param elementName
	 *            The name of the referenced element.
	 * @param augs
	 *            Additional information that may include infoset augmentations.
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */
	public void element(String elementName, Augmentations augs)
			throws XNIException {
	} // element(String, Augmentations)

	/**
	 * The separator between choices or sequences of a mixed or children content
	 * model.
	 * 
	 * @param separator
	 *            The type of children separator.
	 * @param augs
	 *            Additional information that may include infoset augmentations.
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 * 
	 * @see #SEPARATOR_CHOICE
	 * @see #SEPARATOR_SEQUENCE
	 */
	public void separator(short separator, Augmentations augs)
			throws XNIException {
	} // separator(short, Augmentations)

	/**
	 * The occurrence count for a child in a children content model or for the
	 * mixed content model group.
	 * 
	 * @param occurrence
	 *            The occurrence count for the last element or group.
	 * @param augs
	 *            Additional information that may include infoset augmentations.
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 * 
	 * @see #OCCURS_ZERO_OR_ONE
	 * @see #OCCURS_ZERO_OR_MORE
	 * @see #OCCURS_ONE_OR_MORE
	 */
	public void occurrence(short occurrence, Augmentations augs)
			throws XNIException {
	} // occurence(short, Augmentations)

	/**
	 * The end of a group for mixed or children content models.
	 * 
	 * @param augs
	 *            Additional information that may include infoset augmentations.
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */
	public void endGroup(Augmentations augs) throws XNIException {
	} // endGroup(Augmentations)

	/**
	 * The end of a content model.
	 * 
	 * @param augs
	 *            Additional information that may include infoset augmentations.
	 * 
	 * @throws XNIException
	 *             Thrown by handler to signal an error.
	 */
	public void endContentModel(Augmentations augs) throws XNIException {
	} // endContentModel(Augmentations)
}
