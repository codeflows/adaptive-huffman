<?xml version="1.0" encoding="iso-8859-1"?>

<!--
    XSL stylesheet for visualizing the Huffman tree.
    $Id: tree.xsl,v 1.3 2003/02/14 08:54:20 foo Exp $
-->

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output
    method="html"
    encoding="iso-8859-1" />

<xsl:template match="huffman">
    <html>
        <head>
            <title>Adaptive Huffman tree dump</title>
            <style type="text/css">
                div.node {
                    border: 1px dotted #333;
                    text-align: center;
                    padding: 5px;
                }
                td.leaf div.node {
                    border: 1px solid #333;
                }
                span.name {
                    font-size: 22pt;
                    font-weight: bold;
                }
                div.text {
                    font-style: italic;
                    font-size: 16pt;
                }
            </style>
            <link rel="stylesheet" type="text/css" href="http://www.cs.helsinki.fi/u/jaarnial/ugly.css"/>
        </head>
        <body>
        <div class="content">
            <h3>Adaptive Huffman tree dump</h3>
            <p>
                For the text
                <div class="text"><xsl:apply-templates select="source"/></div>
            </p>
            <p>
                This is the formatted XML output of the Adaptive Huffman
                encoding program I implemented in
                <a href="http://www.ocaml.org">O'Caml</a>.
            </p>
        </div>
        <div class="content">
            <xsl:apply-templates select="node"/>
        </div>
        <div class="gibberish">
            Jari Aarniala, <a href="mailto:jari.aarniala@cs.helsinki.fi">jari.aarniala@cs.helsinki.fi</a>
        </div>
        </body>
    </html>
</xsl:template>

<xsl:template match="node">
    <table style="width:100%">
        <tr>
            <td colspan="{count(node)}">
                <xsl:if test="count(node)=0">
                    <xsl:attribute name="class">leaf</xsl:attribute>
                </xsl:if>
                <div class="node">
                    <span class="name">
                        <xsl:choose>
                            <xsl:when test="../../huffman">
                                root
                            </xsl:when>
                            <xsl:when test="@weight=0">
                                NYT
                            </xsl:when>
                            <xsl:when test="not(node)">
                                '<xsl:value-of select="@value"/>'
                            </xsl:when>
                        </xsl:choose>
                    </span>
                    <br/>
                    order: <xsl:value-of select="@order"/>, weight: <xsl:value-of select="@weight"/>
                </div>
            </td>
        </tr>
        <xsl:if test="count(node)&gt;0">
            <tr valign="top">
                <td width="50%"><xsl:apply-templates select="node[position()=1]"/></td>
                <td width="50%"><xsl:apply-templates select="node[position()=2]"/></td>
            </tr>
        </xsl:if>
    </table>
</xsl:template>
</xsl:stylesheet>
