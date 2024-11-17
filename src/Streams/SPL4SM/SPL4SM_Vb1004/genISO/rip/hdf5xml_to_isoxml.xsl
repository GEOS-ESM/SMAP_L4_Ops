<?xml version="1.0" encoding="UTF-8"?>
<!--
*******************************************************************************
**
**  Copyright 2014, Global Modeling and Assimilation Office, NASA GSFC
**
*******************************************************************************
-->
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                              xmlns:gmi="http://www.isotc211.org/2005/gmi"
                              xmlns:gco="http://www.isotc211.org/2005/gco"
                              xmlns:gmd="http://www.isotc211.org/2005/gmd"
                              xmlns:gml="http://www.opengis.net/gml/3.2"
                              xmlns:gmx="http://www.isotc211.org/2005/gmx"
                              xmlns:gss="http://www.isotc211.org/2005/gss"
                              xmlns:gsr="http://www.isotc211.org/2005/gsr"
                              xmlns:gts="http://www.isotc211.org/2005/gts"
                              xmlns:hdf5="http://hdfgroup.org/DTDs/HDF5-File"
                              xmlns:nc="http://www.unidata.ucar.edu/namespaces/netcdf/ncml-2.2"
                              xmlns:xs="http://www.w3.org/2001/XMLSchema"
                              xmlns:xlink="http://www.w3.org/1999/xlink"
                              xmlns:srv="http://www.isotc211.org/2005/srv"
                              xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                              xmlns:eos="http://earthdata.nasa.gov/schema/eos"
                              xsi:schemaLocation="http://hdfgroup.org/DTDs/HDF5-File http://www.hdfgroup.org/DTDs/HDF5-File.xsd"
                              exclude-result-prefixes="hdf5 nc">
    <!--
    ===========================================================================
    Transforms SMAP L4 HDF-5 granule metadata into ISO 19115-2 compliant XML.
    The HDF-5 XML dump command (note: load environment modules):

    h5dump -xml -onlyattr -m "%12.7g" ${HDF5_FILEPATH} > ${HDF5_XML_FILEPATH}

    This XSLT file generates both granule- and series-level ISO compliant XML.
    ===========================================================================
    -->
    <xsl:variable name="stylesheetVersion" select="'0.0'"/>
    <xsl:output method="xml" encoding="UTF-8" indent="yes"/>
    
    <!-- output series metadata: 1:on 0:off -->
    <xsl:variable name="series">1</xsl:variable>
    
    <!-- file properties -->
    <xsl:variable name="base-uri" select="base-uri(.)"/>
    <xsl:variable name="document-uri" select="document-uri(.)"/>
    <xsl:variable name="filename" select="(tokenize($document-uri,'/'))[last()]"/>
    
    <!-- file identifier -->
    <xsl:variable name="fi">
        <xsl:choose>
            <xsl:when test="contains($filename, 'aup')">
                <xsl:value-of select="'L4_SM_aup'"/>
            </xsl:when>
            <xsl:when test="contains($filename, 'gph')">
                <xsl:value-of select="'L4_SM_gph'"/>
            </xsl:when>
            <xsl:when test="contains($filename, 'lmc')">
                <xsl:value-of select="'L4_SM_lmc'"/>
            </xsl:when>
            <xsl:when test="contains($filename, 'mdl')">
                <xsl:value-of select="'L4_C_mdl'"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="$filename"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:variable>
    
    <!--
    ===========================================================================
    root template
    ===========================================================================
    -->
    <xsl:template match="/">
        <!--
        =======================================================================
        series metadata output: series.xml
        =======================================================================
        -->
        <xsl:if test="$series = 1">
            <xsl:result-document method="xml" href="{concat($fi, '_series.xml')}" xml:base="base-uri()">
            <gmd:DS_Series xmlns:gmi="http://www.isotc211.org/2005/gmi"
                           xmlns:gco="http://www.isotc211.org/2005/gco"
                           xmlns:gmd="http://www.isotc211.org/2005/gmd"
                           xmlns:gml="http://www.opengis.net/gml/3.2"
                           xmlns:gmx="http://www.isotc211.org/2005/gmx"
                           xmlns:gss="http://www.isotc211.org/2005/gss"
                           xmlns:gsr="http://www.isotc211.org/2005/gsr"
                           xmlns:gts="http://www.isotc211.org/2005/gts"
                           xmlns:xlink="http://www.w3.org/1999/xlink"
                           xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                           xsi:schemaLocation="http://www.isotc211.org/2005/gmi http://cdn.earthdata.nasa.gov/iso/schema/1.0/ISO19115-2_EOS.xsd">
                <xsl:element name="{'gmd:composedOf'}">
                    <xsl:attribute name="gco:nilReason">
                        <xsl:value-of select="'inapplicable'"/>
                    </xsl:attribute>
                </xsl:element>
                <xsl:call-template name="series"/>
            </gmd:DS_Series>
            </xsl:result-document>
        </xsl:if>
        
        <!--
        =======================================================================
        normal output of granular dataset + series metdata
        =======================================================================
        -->
        <gmd:DS_Series xmlns:gmi="http://www.isotc211.org/2005/gmi"
                       xmlns:gco="http://www.isotc211.org/2005/gco"
                       xmlns:gmd="http://www.isotc211.org/2005/gmd"
                       xmlns:gml="http://www.opengis.net/gml/3.2"
                       xmlns:gmx="http://www.isotc211.org/2005/gmx"
                       xmlns:gss="http://www.isotc211.org/2005/gss"
                       xmlns:gsr="http://www.isotc211.org/2005/gsr"
                       xmlns:gts="http://www.isotc211.org/2005/gts"
                       xmlns:xlink="http://www.w3.org/1999/xlink"
                       xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                       xmlns:eos="http://earthdata.nasa.gov/schema/eos"
                       xsi:schemaLocation="http://www.isotc211.org/2005/gmi http://cdn.earthdata.nasa.gov/iso/schema/1.0/ISO19115-2_EOS.xsd">
            <gmd:composedOf>
                <gmd:DS_DataSet>
                    <gmd:has>
                        <gmi:MI_Metadata>
                            <xsl:variable name="root" select="/hdf5:HDF5-File/hdf5:RootGroup/hdf5:Group[@Name='Metadata']"/>
                            <!--
                            ===================================================
                            Metadata Entity: root entity that defines
                                information about imagery or gridded data
                            ===================================================
                            -->
                            <xsl:call-template name="type">
                                <xsl:with-param name="tag">gmd:fileIdentifier</xsl:with-param>
                                <xsl:with-param name="type">gmx:FileName</xsl:with-param>
                                <xsl:with-param name="value" select="//hdf5:Group[@Name='DatasetIdentification']/hdf5:Attribute[@Name='fileName']/hdf5:Data/hdf5:DataFromFile"/>
                            </xsl:call-template>
                            <xsl:call-template name="type">
                                <xsl:with-param name="tag">gmd:language</xsl:with-param>
                                <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                <xsl:with-param name="value" select="//hdf5:Group[@Name='DatasetIdentification']/hdf5:Attribute[@Name='language']/hdf5:Data/hdf5:DataFromFile"/>
                            </xsl:call-template>
                            <xsl:call-template name="codeList">
                                <xsl:with-param name="tag">gmd:characterSet</xsl:with-param>
                                <xsl:with-param name="type">gmd:MD_CharacterSetCode</xsl:with-param>
                                <xsl:with-param name="value" select="//hdf5:Group[@Name='DatasetIdentification']/hdf5:Attribute[@Name='characterSet']/hdf5:Data/hdf5:DataFromFile"/>
                            </xsl:call-template>
                            <xsl:call-template name="codeList">
                                <xsl:with-param name="tag">gmd:hierarchyLevel</xsl:with-param>
                                <xsl:with-param name="type">gmd:MD_ScopeCode</xsl:with-param>
                                <xsl:with-param name="value">dataset</xsl:with-param>
                            </xsl:call-template>
                            <gmd:contact>
                                <gmd:CI_ResponsibleParty>
                                    <xsl:call-template name="type">
                                        <xsl:with-param name="tag">gmd:organisationName</xsl:with-param>
                                        <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                        <xsl:with-param name="value">NSIDC DAAC &gt; National Snow and Ice Data Center DAAC</xsl:with-param>
                                    </xsl:call-template>
                                    <gmd:contactInfo>
                                        <gmd:CI_Contact>
                                            <gmd:address>
                                                <gmd:CI_Address>
                                                    <xsl:call-template name="type">
                                                        <xsl:with-param name="tag">gmd:electronicMailAddress</xsl:with-param>
                                                        <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                                        <xsl:with-param name="value">nsidc@nsidc.org</xsl:with-param>
                                                    </xsl:call-template>
                                                </gmd:CI_Address>
                                            </gmd:address>
                                            <gmd:onlineResource>
                                                <gmd:CI_OnlineResource>
                                                    <xsl:call-template name="type">
                                                        <xsl:with-param name="tag">gmd:linkage</xsl:with-param>
                                                        <xsl:with-param name="type">gmd:URL</xsl:with-param>
                                                        <xsl:with-param name="value">http://nsidc.org/daac/</xsl:with-param>
                                                    </xsl:call-template>
                                                </gmd:CI_OnlineResource>
                                            </gmd:onlineResource>
                                        </gmd:CI_Contact>
                                    </gmd:contactInfo>
                                    <gmd:role>
                                        <gmd:CI_RoleCode codeList="http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#CI_RoleCode" codeListValue="pointOfContact">pointOfContact</gmd:CI_RoleCode>
                                    </gmd:role>
                                </gmd:CI_ResponsibleParty>
                            </gmd:contact>
                            <gmd:contact>
                                <xsl:call-template name="responsible">
                                    <xsl:with-param name="name" select="//hdf5:Group[@Name='DatasetIdentification']/hdf5:Attribute[@Name='originatorOrganizationName']/hdf5:Data/hdf5:DataFromFile"/>
                                    <xsl:with-param name="role" select="'originator'"/>
                                </xsl:call-template>
                            </gmd:contact>
                            <xsl:call-template name="type">
                                <xsl:with-param name="tag">gmd:dateStamp</xsl:with-param>
                                <xsl:with-param name="type">gco:DateTime</xsl:with-param>
                                <xsl:with-param name="value" select="//hdf5:Group[@Name='DatasetIdentification']/hdf5:Attribute[@Name='creationDate']/hdf5:Data/hdf5:DataFromFile"/>
                            </xsl:call-template>
                            <xsl:call-template name="type">
                                <xsl:with-param name="tag">gmd:metadataStandardName</xsl:with-param>
                                <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                <xsl:with-param name="value">ISO 19115-2 Geographic information - Metadata - Part 2: Extensions for imagery and gridded data</xsl:with-param>
                            </xsl:call-template>
                            <xsl:call-template name="type">
                                <xsl:with-param name="tag">gmd:metadataStandardVersion</xsl:with-param>
                                <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                <xsl:with-param name="value">ISO 19115-2:2009-02-15</xsl:with-param>
                            </xsl:call-template>
                            
                            <!--
                            ===================================================
                            Spatial Representation: digital representation of
                                spatial information in the dataset
                            ===================================================
                            -->
                            <xsl:for-each select="//hdf5:Group[@Name='GridSpatialRepresentation']">
                                <gmd:spatialRepresentationInfo>
                                    <xsl:call-template name="mdGeoreferenceable">
                                        <xsl:with-param name="objxid" select="./@OBJ-XID"/>
                                    </xsl:call-template>
                                </gmd:spatialRepresentationInfo>
                            </xsl:for-each>
                            
                            <!--
                            ===================================================
                            DatasetIdentification
                            ===================================================
                            -->
                            <xsl:for-each select="//hdf5:Group[@Name='DatasetIdentification']">
                                <gmd:identificationInfo>
                                    <xsl:variable name="objxid" select="./@OBJ-XID"/>
                                    <xsl:for-each select="//hdf5:Group[@OBJ-XID=$objxid]">
                                        <gmd:MD_DataIdentification>
                                            <gmd:citation>
                                                <xsl:call-template name="citation">
                                                    <xsl:with-param name="file" select="hdf5:Attribute[@Name='fileName']/hdf5:Data/hdf5:DataFromFile"/>
                                                    <xsl:with-param name="date" select="hdf5:Attribute[@Name='creationDate']/hdf5:Data/hdf5:DataFromFile"/>
                                                    <xsl:with-param name="dateType" select="'creation'"/>
                                                    <xsl:with-param name="edition" select="hdf5:Attribute[@Name='CompositeReleaseID']/hdf5:Data/hdf5:DataFromFile"/>
                                                    <xsl:with-param name="organization" select="hdf5:Attribute[@Name='originatorOrganizationName']/hdf5:Data/hdf5:DataFromFile"/>
                                                    <xsl:with-param name="role" select="'processor'"/>
                                                    <xsl:with-param name="identifier" select="hdf5:Attribute[@Name='shortName']/hdf5:Data/hdf5:DataFromFile"/>
                                                    <xsl:with-param name="identifierCode" select="'http://gmao.gsfc.nasa.gov'"/>
                                                    <xsl:with-param name="identifierDesc" select="'The ECS Short Name'"/>
                                                    <xsl:with-param name="ECS" select="hdf5:Attribute[@Name='ECSVersionID']/hdf5:Data/hdf5:DataFromFile"/>
                                                    <xsl:with-param name="ECSCode" select="'gov.nasa.esdis'"/>
                                                    <xsl:with-param name="ECSDesc" select="'The ECS Version ID'"/>
                                                    <xsl:with-param name="UUID" select="hdf5:Attribute[@Name='UUID']/hdf5:Data/hdf5:DataFromFile"/>
                                                    <xsl:with-param name="UUIDDesc" select="'A Universally Unique Identifier (UUID)'"/>
                                                    <xsl:with-param name="series" select="hdf5:Attribute[@Name='SMAPShortName']/hdf5:Data/hdf5:DataFromFile"/>
                                                </xsl:call-template>
                                            </gmd:citation>
                                            <xsl:call-template name="type">
                                                <xsl:with-param name="tag">gmd:abstract</xsl:with-param>
                                                <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                                <xsl:with-param name="value" select="hdf5:Attribute[@Name='abstract']/hdf5:Data/hdf5:DataFromFile"/>
                                            </xsl:call-template>
                                            <xsl:call-template name="type">
                                                <xsl:with-param name="tag">gmd:purpose</xsl:with-param>
                                                <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                                <xsl:with-param name="value" select="hdf5:Attribute[@Name='purpose']/hdf5:Data/hdf5:DataFromFile"/>
                                            </xsl:call-template>
                                            <xsl:call-template name="type">
                                                <xsl:with-param name="tag">gmd:credit</xsl:with-param>
                                                <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                                <xsl:with-param name="value" select="hdf5:Attribute[@Name='credit']/hdf5:Data/hdf5:DataFromFile"/>
                                            </xsl:call-template>
                                            <xsl:call-template name="codeList">
                                                <xsl:with-param name="tag">gmd:status</xsl:with-param>
                                                <xsl:with-param name="type">gmd:MD_ProgressCode</xsl:with-param>
                                                <xsl:with-param name="value" select="hdf5:Attribute[@Name='status']/hdf5:Data/hdf5:DataFromFile"/>
                                            </xsl:call-template>
                                            <!--
                                            ===================================================
                                            QADatasetIdentification
                                            ===================================================
                                            -->
                                            <xsl:for-each select="//hdf5:Group[@Name='DatasetIdentification']">
                                                <gmd:aggregationInfo>
                                                    <xsl:variable name="objxid" select="./@OBJ-XID"/>
                                                    <xsl:for-each select="//hdf5:Group[@OBJ-XID=$objxid]">
                                                        <gmd:MD_AggregateInformation>
                                                            <gmd:aggregateDataSetName>
                                                                <xsl:call-template name="citation">
                                                                    <xsl:with-param name="file" select="hdf5:Attribute[@Name='QAFileName']/hdf5:Data/hdf5:DataFromFile"/>
                                                                    <xsl:with-param name="date" select="hdf5:Attribute[@Name='QACreationDate']/hdf5:Data/hdf5:DataFromFile"/>
                                                                    <xsl:with-param name="dateType" select="'creation'"/>
                                                                    <!--<xsl:with-param name="edition" select="hdf5:Attribute[@Name='CompositeReleaseID']/hdf5:Data/hdf5:DataFromFile"/> -->
                                                                    <xsl:with-param name="other" select="hdf5:Attribute[@Name='QAAbstract']/hdf5:Data/hdf5:DataFromFile"/>
                                                                </xsl:call-template>
                                                            </gmd:aggregateDataSetName>
                                                            <xsl:call-template name="codeList">
                                                                <xsl:with-param name="tag">gmd:associationType</xsl:with-param>
                                                                <xsl:with-param name="type">gmd:DS_AssociationTypeCode</xsl:with-param>
                                                                <xsl:with-param name="value">crossReference</xsl:with-param>
                                                            </xsl:call-template>
                                                        </gmd:MD_AggregateInformation>
                                                    </xsl:for-each>
                                                </gmd:aggregationInfo>
                                            </xsl:for-each>
                                            <xsl:if test="hdf5:Attribute[@Name='spatialRepresentationType']/hdf5:Data/hdf5:DataFromFile">
                                            <xsl:call-template name="codeList">
                                                <xsl:with-param name="tag">gmd:spatialRepresentationType</xsl:with-param>
                                                <xsl:with-param name="type">gmd:MD_SpatialRepresentationTypeCode</xsl:with-param>
                                                <xsl:with-param name="value" select="hdf5:Attribute[@Name='spatialRepresentationType']/hdf5:Data/hdf5:DataFromFile"/>
                                            </xsl:call-template>
                                            </xsl:if>
                                            <xsl:call-template name="type">
                                                <xsl:with-param name="tag">gmd:language</xsl:with-param>
                                                <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                                <xsl:with-param name="value" select="hdf5:Attribute[@Name='language']/hdf5:Data/hdf5:DataFromFile"/>
                                            </xsl:call-template>
                                            <xsl:call-template name="codeList">
                                                <xsl:with-param name="tag">gmd:characterSet</xsl:with-param>
                                                <xsl:with-param name="type">gmd:MD_CharacterSetCode</xsl:with-param>
                                                <xsl:with-param name="value" select="hdf5:Attribute[@Name='characterSet']/hdf5:Data/hdf5:DataFromFile"/>
                                            </xsl:call-template>
                                            <xsl:call-template name="type">
                                                <xsl:with-param name="tag">gmd:topicCategory</xsl:with-param>
                                                <xsl:with-param name="type">gmd:MD_TopicCategoryCode</xsl:with-param>
                                                <xsl:with-param name="value" select="hdf5:Attribute[@Name='topicCategoryCode']/hdf5:Data/hdf5:DataFromFile"/>
                                            </xsl:call-template>
                                            <gmd:environmentDescription>
                                                <gco:CharacterString>Data product generated by the SMAP mission in HDF5 format with ISO 19115 conformant metadata.</gco:CharacterString>
                                            </gmd:environmentDescription>
                                            <gmd:extent>
                                                <xsl:for-each select="//hdf5:Group[@Name='Extent']">
                                                    <xsl:call-template name="extent">
                                                        <xsl:with-param name="objxid" select="./@OBJ-XID"/>
                                                    </xsl:call-template>
                                                </xsl:for-each>
                                            </gmd:extent>
                                        </gmd:MD_DataIdentification>
                                    </xsl:for-each>
                                </gmd:identificationInfo>
                            </xsl:for-each>
                            
                            

                            <!--
                            ===================================================
                            Data Quality: provides overal assessment of
                                quality of a resource(s)
                            ===================================================
                            -->
                            <!-- no dq in lmc so we need to code one -->
                            <xsl:if test="not(//hdf5:Group[@Name='DataQuality'])">
                                <gmd:dataQualityInfo>
                                    <gmd:DQ_DataQuality>
                                    <gmd:scope>
                                        <gmd:DQ_Scope>
                                            <gmd:level>
                                                <gmd:MD_ScopeCode codeList="http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#MD_ScopeCode" codeListValue="dataset">dataset</gmd:MD_ScopeCode>
                                            </gmd:level>
                                        </gmd:DQ_Scope>
                                    </gmd:scope>
                                    <gmd:lineage>
                                        <gmd:LI_Lineage>
                                            <gmd:processStep>
                                                <gmi:LE_ProcessStep>
                                                    <gmd:description/>
                                                    <gmd:dateTime>
                                                        <gco:DateTime>2014-06-25T15:59:52.000Z</gco:DateTime>
                                                    </gmd:dateTime>
                                                </gmi:LE_ProcessStep>
                                            </gmd:processStep>
                                        </gmd:LI_Lineage>
                                    </gmd:lineage>
                                    </gmd:DQ_DataQuality>
                                </gmd:dataQualityInfo>
                            </xsl:if>
                            <xsl:if test="//hdf5:Group[@Name='DataQuality']">
                                    <xsl:for-each select="//hdf5:Group[@Name='DataQuality']/hdf5:Group[@*]">
                                        <xsl:variable name="scope" select="hdf5:Attribute[@Name='Scope']/hdf5:Data/hdf5:DataFromFile"/>
                                        <xsl:variable name="this" select="."/>
                                        <xsl:for-each select="tokenize(normalize-space($scope),' ')">
                                        <gmd:dataQualityInfo>
                                            <gmd:DQ_DataQuality>
                                            <xsl:call-template name="dqDataQuality">
                                                <xsl:with-param name="this" select="$this"/>
                                                <xsl:with-param name="root" select="$root"/>
                                            </xsl:call-template>
                                            <xsl:if test="position()=1">
                                            <xsl:call-template name="line">
                                                <xsl:with-param name="root" select="$root"/>
                                            </xsl:call-template>
                                            </xsl:if>
                                            </gmd:DQ_DataQuality>
                                        </gmd:dataQualityInfo>
                                        </xsl:for-each>
                                    </xsl:for-each>
                            </xsl:if>

                            <!--
                            ===================================================
                            Acquisition: designations for the measuring
                                instruments, the platform carrying them, and
                                the mission to which the data contributes
                            ===================================================
                            -->
                            <xsl:for-each select="//hdf5:Group[@Name='AcquisitionInformation']">
                                <gmi:acquisitionInformation>
                                    <xsl:call-template name="miAcquisitionInformation">
                                        <xsl:with-param name="objxid" select="./@OBJ-XID"/>
                                    </xsl:call-template>
                                </gmi:acquisitionInformation>
                            </xsl:for-each>
                        </gmi:MI_Metadata>
                    </gmd:has>
                </gmd:DS_DataSet>
            </gmd:composedOf>
            <!--
            ===================================================================
            series Metadata within granule (turn off by using nilReason line)
            ===================================================================
            -->
            <gmd:seriesMetadata gco:nilReason="missing"/>
            <!-- <xsl:call-template name="series"/> -->
        </gmd:DS_Series>
    </xsl:template>
    
    <!--
    ===========================================================================
    series template
    ===========================================================================
    -->
    <xsl:template name="series">
        <gmd:seriesMetadata>
            <gmi:MI_Metadata>
                <!--
                ===============================================================
                Metadata Entity: root entity that defines information about
                    imagery or gridded data
                ===============================================================
                -->
                <xsl:call-template name="type">
                    <xsl:with-param name="tag">gmd:fileIdentifier</xsl:with-param>
                    <xsl:with-param name="type">gmx:FileName</xsl:with-param>
                    <xsl:with-param name="value" select="$fi"/>
                </xsl:call-template>
                <xsl:call-template name="type">
                    <xsl:with-param name="tag">gmd:language</xsl:with-param>
                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                    <xsl:with-param name="value" select="//hdf5:Group[@Name='SeriesIdentification']/hdf5:Attribute[@Name='language']/hdf5:Data/hdf5:DataFromFile"/>
                </xsl:call-template>
                <xsl:call-template name="codeList">
                    <xsl:with-param name="tag">gmd:characterSet</xsl:with-param>
                    <xsl:with-param name="type">gmd:MD_CharacterSetCode</xsl:with-param>
                    <xsl:with-param name="value" select="//hdf5:Group[@Name='SeriesIdentification']/hdf5:Attribute[@Name='characterSet']/hdf5:Data/hdf5:DataFromFile"/>
                </xsl:call-template>
                <xsl:call-template name="codeList">
                    <xsl:with-param name="tag">gmd:hierarchyLevel</xsl:with-param>
                    <xsl:with-param name="type">gmd:MD_ScopeCode</xsl:with-param>
                    <xsl:with-param name="value">series</xsl:with-param>
                </xsl:call-template>
                <gmd:contact>
                    <gmd:CI_ResponsibleParty>
                        <xsl:call-template name="type">
                            <xsl:with-param name="tag">gmd:organisationName</xsl:with-param>
                            <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                            <xsl:with-param name="value">NSIDC DAAC &gt; National Snow and Ice Data Center DAAC</xsl:with-param>
                        </xsl:call-template>
                        <gmd:contactInfo>
                            <gmd:CI_Contact>
                                <gmd:address>
                                    <gmd:CI_Address>
                                        <xsl:call-template name="type">
                                            <xsl:with-param name="tag">gmd:electronicMailAddress</xsl:with-param>
                                            <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                            <xsl:with-param name="value">nsidc@nsidc.org</xsl:with-param>
                                        </xsl:call-template>
                                    </gmd:CI_Address>
                                </gmd:address>
                                <gmd:onlineResource>
                                    <gmd:CI_OnlineResource>
                                        <xsl:call-template name="type">
                                            <xsl:with-param name="tag">gmd:linkage</xsl:with-param>
                                            <xsl:with-param name="type">gmd:URL</xsl:with-param>
                                            <xsl:with-param name="value">http://nsidc.org/daac/</xsl:with-param>
                                        </xsl:call-template>
                                    </gmd:CI_OnlineResource>
                                </gmd:onlineResource>
                            </gmd:CI_Contact>
                        </gmd:contactInfo>
                        <gmd:role>
                            <gmd:CI_RoleCode codeList="http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#CI_RoleCode" codeListValue="pointOfContact">pointOfContact</gmd:CI_RoleCode>
                        </gmd:role>
                    </gmd:CI_ResponsibleParty>
                </gmd:contact>
                <xsl:call-template name="type">
                    <xsl:with-param name="tag">gmd:dateStamp</xsl:with-param>
                    <xsl:with-param name="type">gco:Date</xsl:with-param>
                    <xsl:with-param name="value" select="//hdf5:Group[@Name='SeriesIdentification']/hdf5:Attribute[@Name='revisionDate']/hdf5:Data/hdf5:DataFromFile"/>
                </xsl:call-template>
                <xsl:call-template name="type">
                    <xsl:with-param name="tag">gmd:metadataStandardName</xsl:with-param>
                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                    <xsl:with-param name="value">ISO 19115-2 Geographic information - Metadata - Part 2: Extensions for imagery and gridded data</xsl:with-param>
                </xsl:call-template>
                <xsl:call-template name="type">
                    <xsl:with-param name="tag">gmd:metadataStandardVersion</xsl:with-param>
                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                    <xsl:with-param name="value">ISO 19115-2:2009-02-15</xsl:with-param>
                </xsl:call-template>
                
                <!--
                ===============================================================
                DatasetIdentification
                ===============================================================
                -->
                <xsl:for-each select="//hdf5:Group[@Name='SeriesIdentification']">
                    <gmd:identificationInfo>
                        <xsl:variable name="objxid" select="./@OBJ-XID"/>
                        <xsl:for-each select="//hdf5:Group[@OBJ-XID=$objxid]">
                            <gmd:MD_DataIdentification>
                                <gmd:citation>
                                    <xsl:call-template name="citation">
                                        <xsl:with-param name="title" select="hdf5:Attribute[@Name='longName']/hdf5:Data/hdf5:DataFromFile"/>
                                        <xsl:with-param name="date" select="hdf5:Attribute[@Name='revisionDate']/hdf5:Data/hdf5:DataFromFile"/>
                                        <xsl:with-param name="dateType" select="'revision'"/>
                                        <xsl:with-param name="edition" select="hdf5:Attribute[@Name='VersionID']/hdf5:Data/hdf5:DataFromFile"/>
                                        <xsl:with-param name="identifier" select="hdf5:Attribute[@Name='shortName']/hdf5:Data/hdf5:DataFromFile"/>
                                        <xsl:with-param name="identifierCode" select="'http://gmao.gsfc.nasa.gov'"/>
                                        <xsl:with-param name="identifierDesc" select="'The ECS Short Name'"/>
                                        <xsl:with-param name="ECS" select="hdf5:Attribute[@Name='ECSVersionID']/hdf5:Data/hdf5:DataFromFile"/>
                                        <xsl:with-param name="ECSCode" select="'gov.nasa.esdis'"/>
                                        <xsl:with-param name="ECSDesc" select="'The ECS Version ID'"/>
                                        <xsl:with-param name="DOI" select="hdf5:Attribute[@Name='identifier_product_DOI']/hdf5:Data/hdf5:DataFromFile"/>
                                        <xsl:with-param name="DOICode" select="'gov.nasa.esdis'"/>
                                        <xsl:with-param name="DOIDesc" select="'A Digital Object Identifier (DOI) that provides a persistent interoperable means to locate the SMAP Level 4 Radar data product.'"/>
                                        <xsl:with-param name="UUID" select="hdf5:Attribute[@Name='UUID']/hdf5:Data/hdf5:DataFromFile"/>
                                        <xsl:with-param name="UUIDDesc" select="'A Universally Unique Identifier (UUID)'"/>
                                        <xsl:with-param name="organization" select="hdf5:Attribute[@Name='resourceProviderOrganizationName']/hdf5:Data/hdf5:DataFromFile"/>
                                        <xsl:with-param name="role" select="'resourceProvider'"/>
                                        <xsl:with-param name="series" select="hdf5:Attribute[@Name='SMAPShortName']/hdf5:Data/hdf5:DataFromFile"/>
                                        <xsl:with-param name="other" select="'The launch ready Release of the SMAP Level 4 Surface and Root Zone Soil Moisture Science Processing Software.'"/>
                                    </xsl:call-template>
                                </gmd:citation>
                                <xsl:call-template name="type">
                                    <xsl:with-param name="tag">gmd:abstract</xsl:with-param>
                                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                    <xsl:with-param name="value" select="hdf5:Attribute[@Name='abstract']/hdf5:Data/hdf5:DataFromFile"/>
                                </xsl:call-template>
                                <xsl:call-template name="type">
                                    <xsl:with-param name="tag">gmd:purpose</xsl:with-param>
                                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                    <xsl:with-param name="value" select="hdf5:Attribute[@Name='purpose']/hdf5:Data/hdf5:DataFromFile"/>
                                </xsl:call-template>
                                <xsl:call-template name="type">
                                    <xsl:with-param name="tag">gmd:credit</xsl:with-param>
                                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                    <xsl:with-param name="value" select="hdf5:Attribute[@Name='credit']/hdf5:Data/hdf5:DataFromFile"/>
                                </xsl:call-template>
                                <xsl:call-template name="codeList">
                                    <xsl:with-param name="tag">gmd:status</xsl:with-param>
                                    <xsl:with-param name="type">gmd:MD_ProgressCode</xsl:with-param>
                                    <xsl:with-param name="value" select="hdf5:Attribute[@Name='status']/hdf5:Data/hdf5:DataFromFile"/>
                                </xsl:call-template>
                                <gmd:pointOfContact>
                                    <xsl:call-template name="responsible">
                                        <xsl:with-param name="name" select="hdf5:Attribute[@Name='pointOfContact']/hdf5:Data/hdf5:DataFromFile"/>
                                        <xsl:with-param name="role" select="'distributor'"/>
                                    </xsl:call-template>
                                </gmd:pointOfContact>
                                <gmd:resourceMaintenance>
                                    <gmd:MD_MaintenanceInformation>
                                        <xsl:call-template name="codeList">
                                            <xsl:with-param name="tag">gmd:maintenanceAndUpdateFrequency</xsl:with-param>
                                            <xsl:with-param name="type">gmd:MD_MaintenanceFrequencyCode</xsl:with-param>
                                            <xsl:with-param name="value" select="hdf5:Attribute[@Name='maintenanceAndUpdateFrequency']/hdf5:Data/hdf5:DataFromFile"/>
                                        </xsl:call-template>
                                        <xsl:call-template name="type">
                                            <xsl:with-param name="tag">gmd:dateOfNextUpdate</xsl:with-param>
                                            <xsl:with-param name="type">gco:Date</xsl:with-param>
                                            <xsl:with-param name="value" select="hdf5:Attribute[@Name='maintenanceDate']/hdf5:Data/hdf5:DataFromFile"/>
                                        </xsl:call-template>
                                        <xsl:call-template name="codeList">
                                            <xsl:with-param name="tag">gmd:updateScope</xsl:with-param>
                                            <xsl:with-param name="type">gmd:MD_ScopeCode</xsl:with-param>
                                            <xsl:with-param name="value" select="'series'"/>
                                        </xsl:call-template>
                                    </gmd:MD_MaintenanceInformation>
                                </gmd:resourceMaintenance>
                                <gmd:resourceFormat>
                                    <gmd:MD_Format>
                                        <xsl:call-template name="type">
                                            <xsl:with-param name="tag">gmd:name</xsl:with-param>
                                            <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                            <xsl:with-param name="value" select="hdf5:Attribute[@Name='format']/hdf5:Data/hdf5:DataFromFile"/>
                                        </xsl:call-template>
                                        <xsl:call-template name="type">
                                            <xsl:with-param name="tag">gmd:version</xsl:with-param>
                                            <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                            <xsl:with-param name="value" select="hdf5:Attribute[@Name='formatVersion']/hdf5:Data/hdf5:DataFromFile"/>
                                        </xsl:call-template>
                                    </gmd:MD_Format>
                                </gmd:resourceFormat>
                                <gmd:descriptiveKeywords>
                                    <gmd:MD_Keywords>
                                        <xsl:call-template name="type">
                                            <xsl:with-param name="tag">gmd:keyword</xsl:with-param>
                                            <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                            <xsl:with-param name="value">EARTH SCIENCE &gt; LAND SURFACE &gt; SOILS &gt; SOIL MOISTURE/WATER CONTENT</xsl:with-param>
                                        </xsl:call-template>
                                        <xsl:call-template name="type">
                                            <xsl:with-param name="tag">gmd:keyword</xsl:with-param>
                                            <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                            <xsl:with-param name="value">EARTH SCIENCE &gt; LAND SURFACE &gt; SOILS &gt; SOIL MOISTURE/WATER CONTENT &gt; NONE &gt; NONE &gt; SURFACE SOIL MOISTURE</xsl:with-param>
                                        </xsl:call-template>
                                        <xsl:call-template name="type">
                                            <xsl:with-param name="tag">gmd:keyword</xsl:with-param>
                                            <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                            <xsl:with-param name="value">EARTH SCIENCE &gt; LAND SURFACE &gt; SOILS &gt; SOIL MOISTURE/WATER CONTENT &gt; NONE &gt; NONE &gt; ROOT ZONE SOIL MOISTURE</xsl:with-param>
                                        </xsl:call-template>
                                        <xsl:call-template name="codeList">
                                            <xsl:with-param name="tag">gmd:type</xsl:with-param>
                                            <xsl:with-param name="type">gmd:MD_KeywordTypeCode</xsl:with-param>
                                            <xsl:with-param name="value">theme</xsl:with-param>
                                        </xsl:call-template>
                                        <gmd:thesaurusName>
                                            <xsl:call-template name="citation">
                                                <xsl:with-param name="title">NASA/GCMD Earth Science Keywords</xsl:with-param>
                                            </xsl:call-template>
                                        </gmd:thesaurusName>
                                    </gmd:MD_Keywords>
                                </gmd:descriptiveKeywords>
                                <gmd:descriptiveKeywords>
                                    <gmd:MD_Keywords>
                                        <xsl:call-template name="type">
                                            <xsl:with-param name="tag">gmd:keyword</xsl:with-param>
                                            <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                            <xsl:with-param name="value">Earth Remote Sensing Instruments &gt; Active Remote Sensing &gt; NONE &gt; SMAP L-BAND RADAR &gt; SMAP L-Band Radar</xsl:with-param>
                                        </xsl:call-template>
                                        <xsl:call-template name="codeList">
                                            <xsl:with-param name="tag">gmd:type</xsl:with-param>
                                            <xsl:with-param name="type">gmd:MD_KeywordTypeCode</xsl:with-param>
                                            <xsl:with-param name="value">theme</xsl:with-param>
                                        </xsl:call-template>
                                        <gmd:thesaurusName>
                                            <xsl:call-template name="citation">
                                                <xsl:with-param name="title">NASA/GCMD Earth Science Keywords</xsl:with-param>
                                            </xsl:call-template>
                                        </gmd:thesaurusName>
                                    </gmd:MD_Keywords>
                                </gmd:descriptiveKeywords>
                                <gmd:descriptiveKeywords>
                                    <gmd:MD_Keywords>
                                        <xsl:call-template name="type">
                                            <xsl:with-param name="tag">gmd:keyword</xsl:with-param>
                                            <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                            <xsl:with-param name="value">Earth Observation Satellites &gt; NASA Decadal Survey &gt; SMAP &gt; Soil Moisture Active and Passive Observatory</xsl:with-param>
                                        </xsl:call-template>
                                        <xsl:call-template name="codeList">
                                            <xsl:with-param name="tag">gmd:type</xsl:with-param>
                                            <xsl:with-param name="type">gmd:MD_KeywordTypeCode</xsl:with-param>
                                            <xsl:with-param name="value">theme</xsl:with-param>
                                        </xsl:call-template>
                                        <gmd:thesaurusName>
                                            <xsl:call-template name="citation">
                                                <xsl:with-param name="title">NASA/GCMD Earth Science Keywords</xsl:with-param>
                                            </xsl:call-template>
                                        </gmd:thesaurusName>
                                    </gmd:MD_Keywords>
                                </gmd:descriptiveKeywords>
                                <gmd:descriptiveKeywords>
                                    <gmd:MD_Keywords>
                                        <xsl:call-template name="type">
                                            <xsl:with-param name="tag">gmd:keyword</xsl:with-param>
                                            <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                            <xsl:with-param name="value">GEOGRAPHIC REGION &gt; GLOBAL</xsl:with-param>
                                        </xsl:call-template>
                                        <xsl:call-template name="codeList">
                                            <xsl:with-param name="tag">gmd:type</xsl:with-param>
                                            <xsl:with-param name="type">gmd:MD_KeywordTypeCode</xsl:with-param>
                                            <xsl:with-param name="value">theme</xsl:with-param>
                                        </xsl:call-template>
                                        <gmd:thesaurusName>
                                            <xsl:call-template name="citation">
                                                <xsl:with-param name="title">NASA/GCMD Earth Science Keywords</xsl:with-param>
                                            </xsl:call-template>
                                        </gmd:thesaurusName>
                                    </gmd:MD_Keywords>
                                </gmd:descriptiveKeywords>
                                <gmd:aggregationInfo>
                                    <gmd:MD_AggregateInformation>
                                        <!--gmd:aggregateDataSetName>
                                            <xsl:call-template name="citation">
                                                <xsl:with-param name="file/title" select=""/>
                                                
                                            </xsl:call-template>
                                        </gmd:aggregateDataSetName>-->
                                        <gmd:aggregateDataSetIdentifier>
                                            <gmd:MD_Identifier>
                                                <xsl:call-template name="type">
                                                    <xsl:with-param name="tag">gmd:code</xsl:with-param>
                                                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                                    <xsl:with-param name="value" select="hdf5:Attribute[@Name='mission']/hdf5:Data/hdf5:DataFromFile"/>
                                                </xsl:call-template>
                                            </gmd:MD_Identifier>
                                        </gmd:aggregateDataSetIdentifier>
                                        <xsl:call-template name="codeList">
                                            <xsl:with-param name="tag">gmd:associationType</xsl:with-param>
                                            <xsl:with-param name="type">gmd:DS_AssociationTypeCode</xsl:with-param>
                                            <xsl:with-param name="value" select="'largerWorkCitation'"/>
                                        </xsl:call-template>
                                        <xsl:call-template name="codeList">
                                            <xsl:with-param name="tag">gmd:initiativeType</xsl:with-param>
                                            <xsl:with-param name="type">gmd:DS_InitiativeTypeCode</xsl:with-param>
                                            <xsl:with-param name="value" select="'mission'"/>
                                        </xsl:call-template>
                                    </gmd:MD_AggregateInformation>
                                </gmd:aggregationInfo>
                                <xsl:if test="hdf5:Attribute[@Name='spatialRepresentationType']/hdf5:Data/hdf5:DataFromFile">
                                    <xsl:call-template name="codeList">
                                        <xsl:with-param name="tag">gmd:spatialRepresentationType</xsl:with-param>
                                        <xsl:with-param name="type">gmd:MD_SpatialRepresentationTypeCode</xsl:with-param>
                                        <xsl:with-param name="value" select="hdf5:Attribute[@Name='spatialRepresentationType']/hdf5:Data/hdf5:DataFromFile"/>
                                    </xsl:call-template>
                                </xsl:if>
                                <xsl:call-template name="type">
                                    <xsl:with-param name="tag">gmd:language</xsl:with-param>
                                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                    <xsl:with-param name="value" select="hdf5:Attribute[@Name='language']/hdf5:Data/hdf5:DataFromFile"/>
                                </xsl:call-template>
                                <xsl:call-template name="codeList">
                                    <xsl:with-param name="tag">gmd:characterSet</xsl:with-param>
                                    <xsl:with-param name="type">gmd:MD_CharacterSetCode</xsl:with-param>
                                    <xsl:with-param name="value" select="hdf5:Attribute[@Name='characterSet']/hdf5:Data/hdf5:DataFromFile"/>
                                </xsl:call-template>
                                <xsl:call-template name="type">
                                    <xsl:with-param name="tag">gmd:topicCategory</xsl:with-param>
                                    <xsl:with-param name="type">gmd:MD_TopicCategoryCode</xsl:with-param>
                                    <xsl:with-param name="value" select="hdf5:Attribute[@Name='topicCategoryCode']/hdf5:Data/hdf5:DataFromFile"/>
                                </xsl:call-template>
                                <xsl:call-template name="type">
                                    <xsl:with-param name="tag">gmd:environmentDescription</xsl:with-param>
                                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                    <xsl:with-param name="value" select="'Data product generated by the SMAP mission in HDF5 format with metadata that conforms to the ISO 19115 model.'"/>
                                </xsl:call-template>
                                <gmd:extent>
                                    <xsl:for-each select="//hdf5:Group[@Name='Extent']">
                                        <xsl:call-template name="extent_series">
                                            <xsl:with-param name="objxid" select="./@OBJ-XID"/>
                                        </xsl:call-template>
                                    </xsl:for-each>
                                </gmd:extent>
                            </gmd:MD_DataIdentification>
                        </xsl:for-each>
                    </gmd:identificationInfo>
                </xsl:for-each>
                
                <!--
                ===============================================================
                QADatasetIdentification
                ===============================================================
                -->
                <xsl:for-each select="//hdf5:Group[@Name='SeriesIdentification']">
                    <gmd:identificationInfo>
                        <xsl:variable name="objxid" select="./@OBJ-XID"/>
                        <xsl:for-each select="//hdf5:Group[@OBJ-XID=$objxid]">
                            <gmd:MD_DataIdentification>
                                <gmd:citation>
                                    <xsl:call-template name="citation">
                                        <xsl:with-param name="title" select="hdf5:Attribute[@Name='PSDTitle']/hdf5:Data/hdf5:DataFromFile"/>
                                        <xsl:with-param name="date" select="hdf5:Attribute[@Name='PSDPublicationDate']/hdf5:Data/hdf5:DataFromFile"/>
                                        <xsl:with-param name="dateType" select="'publication'"/>
                                        <xsl:with-param name="edition" select="hdf5:Attribute[@Name='PSDEdition']/hdf5:Data/hdf5:DataFromFile"/>
                                        <xsl:with-param name="identifier" select="hdf5:Attribute[@Name='SMAPShortName']/hdf5:Data/hdf5:DataFromFile"/>
                                        <xsl:with-param name="identifierCode" select="'http://gmao.gsfc.nasa.gov'"/>
                                        <xsl:with-param name="identifierDesc" select="'A short name used by the Soil Moisture Active Passive (SMAP) mission to identify the Level 4 Radar product.'"/>
                                        <xsl:with-param name="series" select="hdf5:Attribute[@Name='SMAPShortName']/hdf5:Data/hdf5:DataFromFile"/>
                                    </xsl:call-template>
                                </gmd:citation>
                                <xsl:call-template name="type">
                                    <xsl:with-param name="tag">gmd:abstract</xsl:with-param>
                                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                    <xsl:with-param name="value" select="hdf5:Attribute[@Name='abstract']/hdf5:Data/hdf5:DataFromFile"/>
                                </xsl:call-template>
                                <xsl:call-template name="type">
                                    <xsl:with-param name="tag">gmd:language</xsl:with-param>
                                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                    <xsl:with-param name="value" select="hdf5:Attribute[@Name='language']/hdf5:Data/hdf5:DataFromFile"/>
                                </xsl:call-template>
                            </gmd:MD_DataIdentification>
                        </xsl:for-each>
                    </gmd:identificationInfo>
                </xsl:for-each>
            </gmi:MI_Metadata>
        </gmd:seriesMetadata>
    </xsl:template>
    
<!-- ====================================================================== -->
<!-- ====================================================================== -->
<!-- ====================================================================== -->

    <!--
    ===========================================================================
    primary class templates
    ===========================================================================
    -->

    <xsl:template name="dqDataQuality">
        <xsl:param name="this"/>
        <xsl:param name="root"/>
        <xsl:variable name="coval" select="tokenize(normalize-space($this/hdf5:Group[@Name='CompletenessOmission']/hdf5:Attribute[@Name='value']/hdf5:Data/hdf5:DataFromFile),' ')"/>
        <!--<xsl:variable name="dcval" select="tokenize(normalize-space($this/hdf5:Group[@Name='DomainConsistency']/hdf5:Attribute[@Name='value']/hdf5:Data/hdf5:DataFromFile),' ')"/>-->
        <!-- data quality identifier -->
        <xsl:variable name="dq">
            <xsl:choose>
                <xsl:when test="contains(., 'freeze_thaw_fraction')">
                    <xsl:value-of select="'FT'"/>
                </xsl:when>
                <xsl:when test="contains(., 'tb_h_obs')">
                    <xsl:value-of select="'TBH_O'"/>
                </xsl:when>
                <xsl:when test="contains(., 'tb_h_forecast')">
                    <xsl:value-of select="'TBH_F'"/>
                </xsl:when>
                <xsl:when test="contains(., 'tb_v_obs')">
                    <xsl:value-of select="'TBV_O'"/>
                </xsl:when>
                <xsl:when test="contains(., 'tb_v_forecast')">
                    <xsl:value-of select="'TBV_F'"/>
                </xsl:when>
                <xsl:when test="contains(., 'sm_surface_pctl')">
                    <xsl:value-of select="'SM_SP'"/>
                </xsl:when>
                <xsl:when test="contains(., 'sm_rootzone_pctl')">
                    <xsl:value-of select="'SM_RZP'"/>
                </xsl:when>
                <xsl:when test="contains(., 'sm_profile_pctl')">
                    <xsl:value-of select="'SM_PP'"/>
                </xsl:when>
                <xsl:when test="contains(., 'sm_profile_pctl')">
                    <xsl:value-of select="'SM_PP'"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="upper-case(.)"/>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        
        <!-- <gmd:DQ_DataQuality> -->
            <gmd:scope>
                <gmd:DQ_Scope>
                    <xsl:call-template name="codeList">
                        <xsl:with-param name="tag">gmd:level</xsl:with-param>
                        <xsl:with-param name="type">gmd:MD_ScopeCode</xsl:with-param>
                        <xsl:with-param name="value">attribute</xsl:with-param>
                    </xsl:call-template>
                    <gmd:levelDescription>
                        <gmd:MD_ScopeDescription>
                            <xsl:call-template name="type">
                                <xsl:with-param name="tag">gmd:dataset</xsl:with-param>
                                <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                <xsl:with-param name="value" select="."/>
                            </xsl:call-template>
                        </gmd:MD_ScopeDescription>
                    </gmd:levelDescription>
                </gmd:DQ_Scope>
            </gmd:scope>
            <gmd:report>
                <xsl:call-template name="dqReport">
                    <xsl:with-param name="tag">gmd:DQ_CompletenessOmission</xsl:with-param>
                    <xsl:with-param name="nameOfMeasure" select="$this/hdf5:Group[@Name='CompletenessOmission']/hdf5:Attribute[@Name='nameOfMeasure']/hdf5:Data/hdf5:DataFromFile"/>
                    <xsl:with-param name="measureDescription" select="$this/hdf5:Group[@Name='CompletenessOmission']/hdf5:Attribute[@Name='measureDescription']/hdf5:Data/hdf5:DataFromFile"/>
                    <xsl:with-param name="evaluation" select="$this/hdf5:Group[@Name='CompletenessOmission']/hdf5:Attribute[@Name='evaluationMethodType']/hdf5:Data/hdf5:DataFromFile"/>
                    <xsl:with-param name="id" select="concat($dq, '_CO')"/>
                    <xsl:with-param name="value" select="string(subsequence($coval, position(), 1))"/>
                    <xsl:with-param name="valueType" select="$this/hdf5:Group[@Name='CompletenessOmission']/hdf5:Attribute[@Name='unitOfMeasure']/hdf5:Data/hdf5:DataFromFile"/>
                    <xsl:with-param name="valueUnit" select="$this/hdf5:Group[@Name='CompletenessOmission']/hdf5:Attribute[@Name='unitOfMeasure']/hdf5:Data/hdf5:DataFromFile"/>
                </xsl:call-template>
            </gmd:report>
            <gmd:report>
                <xsl:call-template name="dqReport">
                    <xsl:with-param name="tag">gmd:DQ_DomainConsistency</xsl:with-param>
                    <xsl:with-param name="nameOfMeasure" select="$this/hdf5:Group[@Name='DomainConsistency']/hdf5:Attribute[@Name='nameOfMeasure']/hdf5:Data/hdf5:DataFromFile"/>
                    <xsl:with-param name="measureDescription" select="$this/hdf5:Group[@Name='DomainConsistency']/hdf5:Attribute[@Name='measureDescription']/hdf5:Data/hdf5:DataFromFile"/>
                    <xsl:with-param name="evaluation" select="$this/hdf5:Group[@Name='DomainConsistency']/hdf5:Attribute[@Name='evaluationMethodType']/hdf5:Data/hdf5:DataFromFile"/>
                    <xsl:with-param name="id" select="concat($dq, '_DC')"/>
                    <!--<xsl:with-param name="value" select="string(subsequence($dcval, position(), 1))"/>-->
                    <xsl:with-param name="value" select="$this/hdf5:Group[@Name='DomainConsistency']/hdf5:Attribute[@Name='value']/hdf5:Data/hdf5:DataFromFile"/>
                    <xsl:with-param name="valueType" select="$this/hdf5:Group[@Name='DomainConsistency']/hdf5:Attribute[@Name='unitOfMeasure']/hdf5:Data/hdf5:DataFromFile"/>
                    <xsl:with-param name="valueUnit" select="$this/hdf5:Group[@Name='DomainConsistency']/hdf5:Attribute[@Name='unitOfMeasure']/hdf5:Data/hdf5:DataFromFile"/>
                </xsl:call-template>
            </gmd:report>
    </xsl:template>
    
    <xsl:template name="line">
        <xsl:param name="root"/>
            <gmd:lineage>
                <gmd:LI_Lineage>
                    <gmd:processStep>
                        <gmi:LE_ProcessStep>
                            <xsl:call-template name="type">
                                <xsl:with-param name="tag">gmd:description</xsl:with-param>
                                <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                <xsl:with-param name="value" select="$root/hdf5:Group[@Name='ProcessStep']/hdf5:Attribute[@Name='processDescription']/hdf5:Data/hdf5:DataFromFile"/>
                            </xsl:call-template>
                            <xsl:call-template name="type">
                                <xsl:with-param name="tag">gmd:dateTime</xsl:with-param>
                                <xsl:with-param name="type">gco:DateTime</xsl:with-param>
                                <xsl:with-param name="value" select="$root/hdf5:Group[@Name='ProcessStep']/hdf5:Attribute[@Name='stepDateTime']/hdf5:Data/hdf5:DataFromFile"/>
                            </xsl:call-template>
                            <gmd:processor>
                                <xsl:call-template name="responsible">
                                    <xsl:with-param name="name" select="$root/hdf5:Group[@Name='ProcessStep']/hdf5:Attribute[@Name='processor']/hdf5:Data/hdf5:DataFromFile"/>
                                    <xsl:with-param name="role" select="'processor'"/>
                                </xsl:call-template>
                            </gmd:processor>
                            <gmi:processingInformation>
                                <eos:EOS_Processing>
                                    <gmi:identifier>
                                        <gmd:MD_Identifier>
                                            <xsl:call-template name="type">
                                                <xsl:with-param name="tag">gmd:code</xsl:with-param>
                                                <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                                <xsl:with-param name="value" select="$root/hdf5:Group[@Name='ProcessStep']/hdf5:Attribute[@Name='identifier']/hdf5:Data/hdf5:DataFromFile"/>
                                            </xsl:call-template>
                                        </gmd:MD_Identifier>
                                    </gmi:identifier>
                                    <gmi:softwareReference>
                                        <xsl:call-template name="citation">
                                            <xsl:with-param name="title" select="$root/hdf5:Group[@Name='ProcessStep']/hdf5:Attribute[@Name='softwareTitle']/hdf5:Data/hdf5:DataFromFile"/>
                                            <xsl:with-param name="date" select="$root/hdf5:Group[@Name='ProcessStep']/hdf5:Attribute[@Name='softwareDate']/hdf5:Data/hdf5:DataFromFile"/>
                                            <xsl:with-param name="dateType" select="'publication'"/>
                                            <xsl:with-param name="edition" select="$root/hdf5:Group[@Name='ProcessStep']/hdf5:Attribute[@Name='SWVersionID']/hdf5:Data/hdf5:DataFromFile"/>
                                        </xsl:call-template>
                                    </gmi:softwareReference>
                                    <gmi:procedureDescription gco:nilReason="unknown"/>
                                    <gmi:documentation>
                                        <xsl:call-template name="citation">
                                            <xsl:with-param name="edition" select="$root/hdf5:Group[@Name='ProcessStep']/hdf5:Attribute[@Name='documentVersion']/hdf5:Data/hdf5:DataFromFile"/>
                                            <xsl:with-param name="dateType" select="'publication'"/>
                                            <xsl:with-param name="date" select="$root/hdf5:Group[@Name='ProcessStep']/hdf5:Attribute[@Name='documentDate']/hdf5:Data/hdf5:DataFromFile"/>
                                            <xsl:with-param name="title" select="$root/hdf5:Group[@Name='ProcessStep']/hdf5:Attribute[@Name='documentation']/hdf5:Data/hdf5:DataFromFile"/>
                                        </xsl:call-template>
                                    </gmi:documentation>
                                    <xsl:call-template name="type">
                                        <xsl:with-param name="tag">gmi:runTimeParameters</xsl:with-param>
                                        <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                        <xsl:with-param name="value" select="$root/hdf5:Group[@Name='ProcessStep']/hdf5:Attribute[@Name='runTimeParameters']/hdf5:Data/hdf5:DataFromFile"/>
                                    </xsl:call-template>
                                    <gmi:algorithm>
                                        <gmi:LE_Algorithm>
                                            <gmi:citation>
                                                <xsl:call-template name="citation">
                                                    <xsl:with-param name="title" select="$root/hdf5:Group[@Name='ProcessStep']/hdf5:Attribute[@Name='ATBDTitle']/hdf5:Data/hdf5:DataFromFile"/>
                                                    <xsl:with-param name="date" select="$root/hdf5:Group[@Name='ProcessStep']/hdf5:Attribute[@Name='ATBDDate']/hdf5:Data/hdf5:DataFromFile"/>
                                                    <xsl:with-param name="dateType" select="'publication'"/>
                                                    <xsl:with-param name="edition" select="$root/hdf5:Group[@Name='ProcessStep']/hdf5:Attribute[@Name='ATBDVersion']/hdf5:Data/hdf5:DataFromFile"/>
                                                </xsl:call-template>
                                            </gmi:citation>
                                            <xsl:call-template name="type">
                                                <xsl:with-param name="tag">gmi:description</xsl:with-param>
                                                <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                                <xsl:with-param name="value" select="$root/hdf5:Group[@Name='ProcessStep']/hdf5:Attribute[@Name='algorithmDescription']/hdf5:Data/hdf5:DataFromFile"/>
                                            </xsl:call-template>
                                        </gmi:LE_Algorithm>
                                    </gmi:algorithm>
                                    <eos:otherProperty>
                                        <gco:Record>
                                            <eos:EOS_AdditionalAttributes>
                                                <xsl:call-template name="eos">
                                                    <xsl:with-param name="name">timeVariableEpoch</xsl:with-param>
                                                    <xsl:with-param name="code">string</xsl:with-param>
                                                    <xsl:with-param name="value" select="$root/hdf5:Group[@Name='ProcessStep']/hdf5:Attribute[@Name='timeVariableEpoch']/hdf5:Data/hdf5:DataFromFile"/>
                                                </xsl:call-template>
                                                <xsl:call-template name="eos">
                                                    <xsl:with-param name="name">epochJulianDate</xsl:with-param>
                                                    <xsl:with-param name="code">float</xsl:with-param>
                                                    <xsl:with-param name="value" select="$root/hdf5:Group[@Name='ProcessStep']/hdf5:Attribute[@Name='epochJulianDate']/hdf5:Data/hdf5:DataFromFile"/>
                                                </xsl:call-template>
                                                <xsl:call-template name="eos">
                                                    <xsl:with-param name="name">epochUTCDateTime</xsl:with-param>
                                                    <xsl:with-param name="code">dateTime</xsl:with-param>
                                                    <xsl:with-param name="value" select="$root/hdf5:Group[@Name='ProcessStep']/hdf5:Attribute[@Name='epochUTCDate']/hdf5:Data/hdf5:DataFromFile"/>
                                                </xsl:call-template>
                                            </eos:EOS_AdditionalAttributes>
                                        </gco:Record>
                                    </eos:otherProperty>
                                </eos:EOS_Processing>
                            </gmi:processingInformation>
                        </gmi:LE_ProcessStep>
                    </gmd:processStep>
                    <xsl:for-each select="$root/hdf5:Group[@Name='Source']/hdf5:Group">
                        <xsl:variable name="name" select="tokenize(normalize-space(hdf5:Attribute[@Name='fileName']/hdf5:Data/hdf5:DataFromFile),' ')"/>
                        <xsl:variable name="source" select="."/>
                        <xsl:variable name="date" select="tokenize(normalize-space(hdf5:Attribute[@Name='creationDate']/hdf5:Data/hdf5:DataFromFile),' ')"/>
                        <xsl:variable name="v" select="tokenize(normalize-space(hdf5:Attribute[@Name='version']/hdf5:Data/hdf5:DataFromFile),' ')"/>
                        <xsl:variable name="doi" select="tokenize(normalize-space($source/hdf5:Attribute[@Name='DOI']/hdf5:Data/hdf5:DataFromFile),' ')"/>
                        <xsl:for-each select="$name">
                            <gmd:source>
                                <gmi:LE_Source>
                                    <xsl:call-template name="type">
                                        <xsl:with-param name="tag">gmd:description</xsl:with-param>
                                        <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                        <xsl:with-param name="value" select="$source/hdf5:Attribute[@Name='description']/hdf5:Data/hdf5:DataFromFile"/>
                                    </xsl:call-template>
                                    <gmd:sourceCitation>
                                        <xsl:call-template name="citation">
                                            <xsl:with-param name="file" select="."/>
                                            <xsl:with-param name="date" select="subsequence($date, position(), 1)"/>
                                            <xsl:with-param name="dateType" select="'creation'"/>
                                            <xsl:with-param name="edition" select="subsequence($v, position(), 1)"/>
                                            <xsl:with-param name="identifier" select="$source/hdf5:Attribute[@Name='identifier']/hdf5:Data/hdf5:DataFromFile"/>
                                            <xsl:with-param name="identifierCode" select="'http://gmao.gsfc.nasa.gov'"/>
                                            <xsl:with-param name="identifierDesc" select="'A short name used by the Soil Moisture Active Passive (SMAP) mission to identify the Level 4 Radar product.'"/>
                                            <xsl:with-param name="DOI" select="subsequence($doi, position(), 1)"/>
                                            <xsl:with-param name="DOIDesc" select="'A Digital Object Identifier (DOI) that provides a persistent interoperable means to locate the SMAP Level 4 Radar data product.'"/>
                                        </xsl:call-template>
                                    </gmd:sourceCitation>
                                </gmi:LE_Source>
                            </gmd:source>
                        </xsl:for-each>
                    </xsl:for-each>
                </gmd:LI_Lineage>
            </gmd:lineage>
    </xsl:template>
    
    <xsl:template name="miAcquisitionInformation">
        <xsl:param name="objxid"/>
        <xsl:for-each select="//hdf5:Group[@OBJ-XID=$objxid]">
            <gmi:MI_AcquisitionInformation>
                <gmi:platform>
                    <gmi:MI_Platform>
                        <gmi:citation>
                            <xsl:call-template name="citation">
                                <xsl:with-param name="title" select="hdf5:Group[@Name='platformDocument']/hdf5:Attribute[@Name='title']/hdf5:Data/hdf5:DataFromFile"/>
                                <xsl:with-param name="date" select="hdf5:Group[@Name='platformDocument']/hdf5:Attribute[@Name='publicationDate']/hdf5:Data/hdf5:DataFromFile"/>
                                <xsl:with-param name="dateType">publication</xsl:with-param>
                                <xsl:with-param name="edition" select="hdf5:Group[@Name='platformDocument']/hdf5:Attribute[@Name='edition']/hdf5:Data/hdf5:DataFromFile"/>
                            </xsl:call-template>
                        </gmi:citation>
                        <gmi:identifier>
                            <gmd:MD_Identifier>
                            <xsl:call-template name="type">
                                <xsl:with-param name="tag">gmd:code</xsl:with-param>
                                <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                <xsl:with-param name="value" select="hdf5:Group[@Name='platform']/hdf5:Attribute[@Name='identifier']/hdf5:Data/hdf5:DataFromFile"/>
                            </xsl:call-template>
                            <xsl:call-template name="type">
                                <xsl:with-param name="tag">gmd:codeSpace</xsl:with-param>
                                <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                <xsl:with-param name="value">http://smap.jpl.nasa.gov</xsl:with-param>
                            </xsl:call-template>
                            </gmd:MD_Identifier>
                        </gmi:identifier>
                        <xsl:call-template name="type">
                            <xsl:with-param name="tag">gmi:description</xsl:with-param>
                            <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                            <xsl:with-param name="value" select="hdf5:Group[@Name='platform']/hdf5:Attribute[@Name='description']/hdf5:Data/hdf5:DataFromFile"/>
                        </xsl:call-template>
                        <gmi:instrument>
                            <gmi:MI_Instrument>
                                <gmi:citation>
                                    <xsl:call-template name="citation">
                                        <xsl:with-param name="title" select="hdf5:Group[@Name='radarDocument']/hdf5:Attribute[@Name='title']/hdf5:Data/hdf5:DataFromFile"/>
                                        <xsl:with-param name="date" select="hdf5:Group[@Name='radarDocument']/hdf5:Attribute[@Name='publicationDate']/hdf5:Data/hdf5:DataFromFile"/>
                                        <xsl:with-param name="dateType">publication</xsl:with-param>
                                        <xsl:with-param name="edition" select="hdf5:Group[@Name='radarDocument']/hdf5:Attribute[@Name='edition']/hdf5:Data/hdf5:DataFromFile"/>
                                    </xsl:call-template>
                                </gmi:citation>
                                <gmi:identifier>
                                    <gmd:MD_Identifier>
                                    <xsl:call-template name="type">
                                        <xsl:with-param name="tag">gmd:code</xsl:with-param>
                                        <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                        <xsl:with-param name="value" select="hdf5:Group[@Name='radar']/hdf5:Attribute[@Name='identifier']/hdf5:Data/hdf5:DataFromFile"/>
                                    </xsl:call-template>
                                    </gmd:MD_Identifier>
                                </gmi:identifier>
                                <xsl:call-template name="type">
                                    <xsl:with-param name="tag">gmi:type</xsl:with-param>
                                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                    <xsl:with-param name="value" select="hdf5:Group[@Name='radar']/hdf5:Attribute[@Name='type']/hdf5:Data/hdf5:DataFromFile"/>
                                </xsl:call-template>
                                <xsl:call-template name="type">
                                    <xsl:with-param name="tag">gmi:description</xsl:with-param>
                                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                    <xsl:with-param name="value" select="hdf5:Group[@Name='radar']/hdf5:Attribute[@Name='description']/hdf5:Data/hdf5:DataFromFile"/>
                                </xsl:call-template>
                            </gmi:MI_Instrument>
                        </gmi:instrument>
                        <gmi:instrument>
                            <gmi:MI_Instrument>
                                <gmi:citation>
                                    <xsl:call-template name="citation">
                                        <xsl:with-param name="title" select="hdf5:Group[@Name='radiometerDocument']/hdf5:Attribute[@Name='title']/hdf5:Data/hdf5:DataFromFile"/>
                                        <xsl:with-param name="date" select="hdf5:Group[@Name='radiometerDocument']/hdf5:Attribute[@Name='publicationDate']/hdf5:Data/hdf5:DataFromFile"/>
                                        <xsl:with-param name="dateType">publication</xsl:with-param>
                                        <xsl:with-param name="edition" select="hdf5:Group[@Name='radiometerDocument']/hdf5:Attribute[@Name='edition']/hdf5:Data/hdf5:DataFromFile"/>
                                    </xsl:call-template>
                                </gmi:citation>
                                <gmi:identifier>
                                    <gmd:MD_Identifier>
                                    <xsl:call-template name="type">
                                        <xsl:with-param name="tag">gmd:code</xsl:with-param>
                                        <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                        <xsl:with-param name="value" select="hdf5:Group[@Name='radiometer']/hdf5:Attribute[@Name='identifier']/hdf5:Data/hdf5:DataFromFile"/>
                                    </xsl:call-template>
                                    </gmd:MD_Identifier>
                                </gmi:identifier>
                                <xsl:call-template name="type">
                                    <xsl:with-param name="tag">gmi:type</xsl:with-param>
                                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                    <xsl:with-param name="value" select="hdf5:Group[@Name='radiometer']/hdf5:Attribute[@Name='type']/hdf5:Data/hdf5:DataFromFile"/>
                                </xsl:call-template>
                                <xsl:call-template name="type">
                                    <xsl:with-param name="tag">gmi:description</xsl:with-param>
                                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                    <xsl:with-param name="value" select="hdf5:Group[@Name='radiometer']/hdf5:Attribute[@Name='description']/hdf5:Data/hdf5:DataFromFile"/>
                                </xsl:call-template>
                            </gmi:MI_Instrument>
                        </gmi:instrument>
                    </gmi:MI_Platform>
                </gmi:platform>
            </gmi:MI_AcquisitionInformation>
        </xsl:for-each>
    </xsl:template>
    
    <xsl:template name="mdGeoreferenceable">
        <xsl:param name="objxid"/>
        <xsl:for-each select="//hdf5:Group[@OBJ-XID=$objxid]">
            <gmd:MD_Georeferenceable>
                <xsl:variable name="dimensionCnt" select="count(hdf5:Group)"/>
                <gmd:numberOfDimensions>
                    <gco:Integer><xsl:value-of select="$dimensionCnt"/></gco:Integer>
                </gmd:numberOfDimensions>
                <xsl:for-each select="hdf5:Group">
                    <xsl:variable name="dimName" select="@Name"/>
                    <xsl:call-template name="mdDimension">
                        <xsl:with-param name="name" select="$dimName"/>
                        <xsl:with-param name="size" select="translate(normalize-space(hdf5:Attribute[@Name='dimensionSize']/hdf5:Data/hdf5:DataFromFile),'&quot;','')"/>
                        <xsl:with-param name="resolution" select="translate(normalize-space(hdf5:Attribute[@Name='resolution']/hdf5:Data/hdf5:DataFromFile),'&quot;','')"/>
                    </xsl:call-template>
                </xsl:for-each>
                <xsl:call-template name="codeList">
                    <xsl:with-param name="tag">gmd:cellGeometry</xsl:with-param>
                    <xsl:with-param name="type">gmd:MD_CellGeometryCode</xsl:with-param>
                    <xsl:with-param name="value">area</xsl:with-param>
                </xsl:call-template>
                <gmd:transformationParameterAvailability gco:nilReason="missing"/>
                <gmd:controlPointAvailability>
                    <gco:Boolean>false</gco:Boolean>
                </gmd:controlPointAvailability>
                <gmd:orientationParameterAvailability>
                    <gco:Boolean>false</gco:Boolean>
                </gmd:orientationParameterAvailability>
                <gmd:georeferencedParameters>
                    <gco:Record xsi:type="gco:CharacterString_PropertyType">
                        <gco:CharacterString>SMAP Fixed Earth Grids, SMAP Science Document no:  033, May 11, 2009</gco:CharacterString>
                    </gco:Record>
                </gmd:georeferencedParameters>
                <!-- <gmi:geolocationInformation gco:nilReason="unknown"/> -->
                <!-- <gmi:geolocationInformation>
                    <gmi:MI_GCPCollection>
                        <gmi:collectionIdentification gco:nilReason="unknown"/>
                        <gmi:collectionName gco:nilReason="unknown"/>
                        <gmi:coordinateReferenceSystem>
                            <gmd:MD_ReferenceSystem>
                                <gmd:referenceSystemIdentifier>
                                    <gmd:RS_Identifier>
                                        <gmd:code gco:nilReason="unknown"/>
                                    </gmd:RS_Identifier>
                                </gmd:referenceSystemIdentifier>
                            </gmd:MD_ReferenceSystem>
                        </gmi:coordinateReferenceSystem>
                        <gmi:gcp>
                            <gmi:MI_GCP>
                                <gmi:geographicCoordinates gco:nilReason="unknown"/>
                            </gmi:MI_GCP>
                        </gmi:gcp>
                    </gmi:MI_GCPCollection>
                </gmi:geolocationInformation> -->
            </gmd:MD_Georeferenceable>
        </xsl:for-each>
    </xsl:template>

    <xsl:template name="extent_series">
        <xsl:param name="objxid"/>
        <xsl:for-each select="//hdf5:Group[@OBJ-XID=$objxid]">
            <gmd:EX_Extent>
                <xsl:call-template name="type">
                    <xsl:with-param name="tag">gmd:description</xsl:with-param>
                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                    <xsl:with-param name="value" select="hdf5:Attribute[@Name='description']/hdf5:Data/hdf5:DataFromFile"/>
                </xsl:call-template>
                <gmd:geographicElement>
                    <gmd:EX_GeographicBoundingBox>
                        <gmd:extentTypeCode>
                            <gco:Boolean>1</gco:Boolean>
                        </gmd:extentTypeCode>
                        <!-- need to be type Angle, but need ISO 19103 -->
                        <xsl:call-template name="type">
                            <xsl:with-param name="tag">gmd:westBoundLongitude</xsl:with-param>
                            <xsl:with-param name="type">gco:Decimal</xsl:with-param>
                            <xsl:with-param name="value" select="translate(normalize-space(hdf5:Attribute[@Name='westBoundLongitude']/hdf5:Data/hdf5:DataFromFile),'&quot;','')"/>
                        </xsl:call-template>
                        <xsl:call-template name="type">
                            <xsl:with-param name="tag">gmd:eastBoundLongitude</xsl:with-param>
                            <xsl:with-param name="type">gco:Decimal</xsl:with-param>
                            <xsl:with-param name="value" select="translate(normalize-space(hdf5:Attribute[@Name='eastBoundLongitude']/hdf5:Data/hdf5:DataFromFile),'&quot;','')"/>
                        </xsl:call-template>
                        <xsl:call-template name="type">
                            <xsl:with-param name="tag">gmd:southBoundLatitude</xsl:with-param>
                            <xsl:with-param name="type">gco:Decimal</xsl:with-param>
                            <xsl:with-param name="value" select="translate(normalize-space(hdf5:Attribute[@Name='southBoundLatitude']/hdf5:Data/hdf5:DataFromFile),'&quot;','')"/>
                        </xsl:call-template>
                        <xsl:call-template name="type">
                            <xsl:with-param name="tag">gmd:northBoundLatitude</xsl:with-param>
                            <xsl:with-param name="type">gco:Decimal</xsl:with-param>
                            <xsl:with-param name="value" select="translate(normalize-space(hdf5:Attribute[@Name='northBoundLatitude']/hdf5:Data/hdf5:DataFromFile),'&quot;','')"/>
                        </xsl:call-template>
                    </gmd:EX_GeographicBoundingBox>
                </gmd:geographicElement>
                <gmd:temporalElement>
                    <gmd:EX_TemporalExtent>
                        <gmd:extent>
                            <gml:TimePeriod gml:id="swathTemporalExtent">
                                <xsl:call-template name="tag">
                                    <xsl:with-param name="tag">gml:beginPosition</xsl:with-param>
                                    <xsl:with-param name="value" select="'2015-01-31T00:00:00.000Z'"/>
                                </xsl:call-template>
                                <xsl:call-template name="tag">
                                    <xsl:with-param name="tag">gml:endPosition</xsl:with-param>
                                    <xsl:with-param name="value" select="'2020-12-31T23:59:59.999Z'"/>
                                </xsl:call-template>
                            </gml:TimePeriod>
                        </gmd:extent>
                    </gmd:EX_TemporalExtent>
                </gmd:temporalElement>
            </gmd:EX_Extent>
        </xsl:for-each>
    </xsl:template>
    
    <xsl:template name="extent">
        <xsl:param name="objxid"/>
        <xsl:for-each select="//hdf5:Group[@OBJ-XID=$objxid]">
            <gmd:EX_Extent>
                <xsl:call-template name="type">
                    <xsl:with-param name="tag">gmd:description</xsl:with-param>
                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                    <xsl:with-param name="value" select="hdf5:Attribute[@Name='description']/hdf5:Data/hdf5:DataFromFile"/>
                </xsl:call-template>
                <gmd:geographicElement>
                    <gmd:EX_GeographicBoundingBox>
                        <gmd:extentTypeCode>
                            <gco:Boolean>1</gco:Boolean>
                        </gmd:extentTypeCode>
                        <!-- need to be type Angle, but need ISO 19103 -->
                        <xsl:call-template name="type">
                            <xsl:with-param name="tag">gmd:westBoundLongitude</xsl:with-param>
                            <xsl:with-param name="type">gco:Decimal</xsl:with-param>
                            <xsl:with-param name="value" select="translate(normalize-space(hdf5:Attribute[@Name='westBoundLongitude']/hdf5:Data/hdf5:DataFromFile),'&quot;','')"/>
                        </xsl:call-template>
                        <xsl:call-template name="type">
                            <xsl:with-param name="tag">gmd:eastBoundLongitude</xsl:with-param>
                            <xsl:with-param name="type">gco:Decimal</xsl:with-param>
                            <xsl:with-param name="value" select="translate(normalize-space(hdf5:Attribute[@Name='eastBoundLongitude']/hdf5:Data/hdf5:DataFromFile),'&quot;','')"/>
                        </xsl:call-template>
                        <xsl:call-template name="type">
                            <xsl:with-param name="tag">gmd:southBoundLatitude</xsl:with-param>
                            <xsl:with-param name="type">gco:Decimal</xsl:with-param>
                            <xsl:with-param name="value" select="translate(normalize-space(hdf5:Attribute[@Name='southBoundLatitude']/hdf5:Data/hdf5:DataFromFile),'&quot;','')"/>
                        </xsl:call-template>
                        <xsl:call-template name="type">
                            <xsl:with-param name="tag">gmd:northBoundLatitude</xsl:with-param>
                            <xsl:with-param name="type">gco:Decimal</xsl:with-param>
                            <xsl:with-param name="value" select="translate(normalize-space(hdf5:Attribute[@Name='northBoundLatitude']/hdf5:Data/hdf5:DataFromFile),'&quot;','')"/>
                        </xsl:call-template>
                    </gmd:EX_GeographicBoundingBox>
                </gmd:geographicElement>
                <gmd:temporalElement>
                    <gmd:EX_TemporalExtent>
                        <gmd:extent>
                            <gml:TimePeriod gml:id="swathTemporalExtent">
                                <xsl:call-template name="tag">
                                    <xsl:with-param name="tag">gml:beginPosition</xsl:with-param>
                                    <xsl:with-param name="value" select="hdf5:Attribute[@Name='rangeBeginningDateTime']/hdf5:Data/hdf5:DataFromFile"/>
                                </xsl:call-template>
                                <xsl:call-template name="tag">
                                    <xsl:with-param name="tag">gml:endPosition</xsl:with-param>
                                    <xsl:with-param name="value" select="hdf5:Attribute[@Name='rangeEndingDateTime']/hdf5:Data/hdf5:DataFromFile"/>
                                </xsl:call-template>
                            </gml:TimePeriod>
                        </gmd:extent>
                    </gmd:EX_TemporalExtent>
                </gmd:temporalElement>
            </gmd:EX_Extent>
        </xsl:for-each>
    </xsl:template>
    
    <!--
    ===========================================================================
    secondary class templates
    ===========================================================================
    -->

    <xsl:template name="dqReport">
        <xsl:param name="tag"/>
        <xsl:param name="nameOfMeasure"/>
        <xsl:param name="measureDescription"/>
        <xsl:param name="evaluation"/>
        <xsl:param name="value"/>
        <xsl:param name="valueType"/>
        <xsl:param name="valueUnit"/>
        <xsl:param name="first"/>
        <xsl:param name="id"/>
        <xsl:element name="{$tag}">
            <xsl:call-template name="type">
                <xsl:with-param name="tag">gmd:nameOfMeasure</xsl:with-param>
                <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                <xsl:with-param name="value" select="$nameOfMeasure"/>
            </xsl:call-template>
            <xsl:call-template name="type">
                <xsl:with-param name="tag">gmd:measureDescription</xsl:with-param>
                <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                <xsl:with-param name="value" select="$measureDescription"/>
            </xsl:call-template>
            <xsl:call-template name="codeList">
                <xsl:with-param name="tag">gmd:evaluationMethodType</xsl:with-param>
                <xsl:with-param name="type">gmd:DQ_EvaluationMethodTypeCode</xsl:with-param>
                <xsl:with-param name="value" select="$evaluation"/>
            </xsl:call-template>
            <gmd:result>
                <!-- very unsure about this part (pulled from JPL) -->
                <gmd:DQ_QuantitativeResult>
                    <gmd:valueUnit>
                        <gmx:ML_DerivedUnit>
                            <xsl:attribute name="gml:id">
                                <xsl:value-of select="translate(normalize-space($id),'&quot;','')"/>
                            </xsl:attribute>
                            <gml:identifier codeSpace="http://jpl.nasa.gov/undefined">*undefined*</gml:identifier>
                            <gml:derivationUnitTerm>
                                <xsl:attribute name="uom">
                                    <xsl:value-of select="translate(normalize-space($valueUnit),'&quot;','')"/>
                                </xsl:attribute>
                            </gml:derivationUnitTerm>
                            <gmx:alternativeExpression/>
                        </gmx:ML_DerivedUnit>
                    </gmd:valueUnit>
                    <gmd:value>
                        <gco:Record xsi:type="gco:Real_PropertyType">
                            <xsl:choose>
                                <xsl:when test="$value and normalize-space($value) and translate(normalize-space($value),'&quot;','')">
                                    <xsl:choose>
                                        <xsl:when test="contains($value, 'TBD') or contains($value, 'missing')">
                                            <xsl:call-template name="tag">
                                                <xsl:with-param name="value" select="'-999'"/>
                                                <xsl:with-param name="tag" select="'gco:Real'"/>
                                            </xsl:call-template>
                                        </xsl:when>
                                        <xsl:otherwise>
                                            <xsl:call-template name="tag">
                                                <xsl:with-param name="value" select="$value"/>
                                                <xsl:with-param name="tag" select="'gco:Real'"/>
                                            </xsl:call-template>
                                        </xsl:otherwise>
                                    </xsl:choose>
                                </xsl:when>
                                <xsl:otherwise>
                                    <xsl:call-template name="tag">
                                        <xsl:with-param name="value" select="'-999'"/>
                                        <xsl:with-param name="tag" select="'gco:Real'"/>
                                    </xsl:call-template>
                                </xsl:otherwise>
                            </xsl:choose>
                        </gco:Record>
                    </gmd:value>
                </gmd:DQ_QuantitativeResult>
            </gmd:result>
        </xsl:element>
    </xsl:template>
    
    <xsl:template name="mdDimension">
        <xsl:param name="name"/>
        <xsl:param name="size"/>
        <xsl:param name="resolution"/>
        <gmd:axisDimensionProperties>
            <gmd:MD_Dimension>
                <xsl:choose>
                    <xsl:when test="contains($name, 'at')">
                        <xsl:call-template name="codeList">
                            <xsl:with-param name="tag">gmd:dimensionName</xsl:with-param>
                            <xsl:with-param name="type">gmd:MD_DimensionNameTypeCode</xsl:with-param>
                            <xsl:with-param name="value" select="'row'"/>
                        </xsl:call-template>
                    </xsl:when>
                    <xsl:when test="contains($name, 'ong')">
                        <xsl:call-template name="codeList">
                            <xsl:with-param name="tag">gmd:dimensionName</xsl:with-param>
                            <xsl:with-param name="type">gmd:MD_DimensionNameTypeCode</xsl:with-param>
                            <xsl:with-param name="value" select="'column'"/>
                        </xsl:call-template>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:call-template name="codeList">
                            <xsl:with-param name="tag">gmd:dimensionName</xsl:with-param>
                            <xsl:with-param name="type">gmd:MD_DimensionNameTypeCode</xsl:with-param>
                            <xsl:with-param name="value" select="$name"/>
                        </xsl:call-template>
                    </xsl:otherwise>
                </xsl:choose>
                <gmd:dimensionSize>
                     <gco:Integer><xsl:value-of select="$size"/></gco:Integer>
                </gmd:dimensionSize>
                <gmd:resolution>
                      <gco:Length>
                          <xsl:attribute name="uom">
                              <xsl:value-of select="translate(substring-after($resolution,' '), ' ', '_')"/>
                          </xsl:attribute>
                          <xsl:value-of select="substring-before($resolution,' ')"/>
                      </gco:Length>
                 </gmd:resolution>
            </gmd:MD_Dimension>
        </gmd:axisDimensionProperties>
    </xsl:template>
    
    <xsl:template name="citation">
        <xsl:param name="title"/>
        <xsl:param name="file"/>
        <xsl:param name="date"/>
        <xsl:param name="dateType"/>
        <xsl:param name="edition"/>
        <xsl:param name="organization"/>
        <xsl:param name="role"/>
        <xsl:param name="identifier"/>
        <xsl:param name="identifierCode"/>
        <xsl:param name="identifierDesc"/>
        <xsl:param name="ECS"/>
        <xsl:param name="ECSCode"/>
        <xsl:param name="ECSDesc"/>
        <xsl:param name="DOI"/>
        <xsl:param name="DOICode"/>
        <xsl:param name="DOIDesc"/>
        <xsl:param name="UUID"/>
        <xsl:param name="UUIDDesc"/>
        <xsl:param name="series"/>
        <xsl:param name="other"/>
        <gmd:CI_Citation>
            <xsl:if test="$file">
                <xsl:call-template name="type">
                    <xsl:with-param name="tag">gmd:title</xsl:with-param>
                    <xsl:with-param name="type">gmx:FileName</xsl:with-param>
                    <xsl:with-param name="value" select="$file"/>
                </xsl:call-template>
            </xsl:if>
            <xsl:if test="$title">
                <xsl:call-template name="type">
                    <xsl:with-param name="tag">gmd:title</xsl:with-param>
                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                    <xsl:with-param name="value" select="$title"/>
                </xsl:call-template>
            </xsl:if>
            <xsl:choose>
                <xsl:when test="contains($date, 'missing')">
                    <gmd:date gco:nilReason="missing"/>
                </xsl:when>
                <xsl:when test="$date">
                    <gmd:date>
                        <xsl:call-template name="date">
                            <xsl:with-param name="date" select="$date"/>
                            <xsl:with-param name="type" select="$dateType"/>
                        </xsl:call-template>
                    </gmd:date>
                </xsl:when>
                <xsl:otherwise>
                    <gmd:date gco:nilReason="missing"/>
                </xsl:otherwise>
            </xsl:choose>
            <xsl:if test="$edition">
            <xsl:call-template name="type">
                <xsl:with-param name="tag">gmd:edition</xsl:with-param>
                <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                <xsl:with-param name="value" select="$edition"/>
            </xsl:call-template>
            </xsl:if>
            <xsl:if test="$identifier">
            <gmd:identifier>
                <gmd:MD_Identifier>
                    <xsl:call-template name="type">
                        <xsl:with-param name="tag">gmd:code</xsl:with-param>
                        <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                        <xsl:with-param name="value" select="$identifier"/>
                    </xsl:call-template>
                    <xsl:call-template name="type">
                        <xsl:with-param name="tag">gmd:codeSpace</xsl:with-param>
                        <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                        <xsl:with-param name="value" select="$identifierCode"/>
                    </xsl:call-template>
                    <xsl:call-template name="type">
                        <xsl:with-param name="tag">gmd:description</xsl:with-param>
                        <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                        <xsl:with-param name="value" select="$identifierDesc"/>
                    </xsl:call-template>
                </gmd:MD_Identifier>
            </gmd:identifier>
            </xsl:if>
            <xsl:if test="$ECS">
            <gmd:identifier>
                <gmd:MD_Identifier>
                    <xsl:call-template name="type">
                        <xsl:with-param name="tag">gmd:code</xsl:with-param>
                        <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                        <xsl:with-param name="value" select="$ECS"/>
                    </xsl:call-template>
                    <xsl:call-template name="type">
                        <xsl:with-param name="tag">gmd:codeSpace</xsl:with-param>
                        <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                        <xsl:with-param name="value" select="$ECSCode"/>
                    </xsl:call-template>
                    <xsl:call-template name="type">
                        <xsl:with-param name="tag">gmd:description</xsl:with-param>
                        <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                        <xsl:with-param name="value" select="$ECSDesc"/>
                    </xsl:call-template>
                </gmd:MD_Identifier>
            </gmd:identifier>
            </xsl:if>
            <xsl:if test="$UUID">
            <gmd:identifier>
                <gmd:MD_Identifier>
                    <xsl:call-template name="type">
                        <xsl:with-param name="tag">gmd:code</xsl:with-param>
                        <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                        <xsl:with-param name="value" select="$UUID"/>
                    </xsl:call-template>
                    <xsl:call-template name="type">
                        <xsl:with-param name="tag">gmd:codeSpace</xsl:with-param>
                        <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                        <xsl:with-param name="value">urn:uuid</xsl:with-param>
                    </xsl:call-template>
                    <xsl:call-template name="type">
                        <xsl:with-param name="tag">gmd:description</xsl:with-param>
                        <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                        <xsl:with-param name="value" select="$UUIDDesc"/>
                    </xsl:call-template>
                </gmd:MD_Identifier>
            </gmd:identifier>
            </xsl:if>
            <xsl:if test="$DOI">
            <gmd:identifier>
                <gmd:MD_Identifier>
                    <gmd:code>
                        <xsl:call-template name="DOI">
                            <xsl:with-param name="value" select="$DOI"/>
                        </xsl:call-template>
                    </gmd:code>
                    <xsl:call-template name="type">
                        <xsl:with-param name="tag">gmd:codeSpace</xsl:with-param>
                        <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                        <xsl:with-param name="value" select="$DOICode"/>
                    </xsl:call-template>
                    <xsl:call-template name="type">
                        <xsl:with-param name="tag">gmd:description</xsl:with-param>
                        <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                        <xsl:with-param name="value" select="$DOIDesc"/>
                    </xsl:call-template>
                </gmd:MD_Identifier>
            </gmd:identifier>
            </xsl:if>
            <xsl:if test="$organization">
            <gmd:citedResponsibleParty>
                <xsl:call-template name="responsible">
                    <xsl:with-param name="name" select="$organization"/>
                    <xsl:with-param name="role" select="$role"/>
                </xsl:call-template>
            </gmd:citedResponsibleParty>
            <gmd:citedResponsibleParty>
                <xsl:call-template name="responsible">
                    <xsl:with-param name="name" select="'Global Modeling and Assimilation Office'"/>
                    <xsl:with-param name="role" select="'originator'"/>
                </xsl:call-template>
            </gmd:citedResponsibleParty>
            </xsl:if>
            <xsl:if test="$series">
            <xsl:call-template name="codeList">
                <xsl:with-param name="tag">gmd:presentationForm</xsl:with-param>
                <xsl:with-param name="type">gmd:CI_PresentationFormCode</xsl:with-param>
                <xsl:with-param name="value">documentDigital</xsl:with-param>
            </xsl:call-template>
            <gmd:series>
                <gmd:CI_Series>
                    <xsl:call-template name="type">
                        <xsl:with-param name="tag">gmd:name</xsl:with-param>
                        <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                        <xsl:with-param name="value" select="$series"/>
                    </xsl:call-template>
                </gmd:CI_Series>
            </gmd:series>
            </xsl:if>
            <xsl:if test="$other">
                <xsl:call-template name="type">
                    <xsl:with-param name="tag">gmd:otherCitationDetails</xsl:with-param>
                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                    <xsl:with-param name="value" select="$other"/>
                </xsl:call-template>
            </xsl:if>
        </gmd:CI_Citation>
    </xsl:template>
    
    <!--
    ===========================================================================
    type templates
    ===========================================================================
    -->
    
    <xsl:template name="type">
        <xsl:param name="tag"/>
        <xsl:param name="type"/>
        <xsl:param name="value"/> 
        <xsl:element name="{$tag}">
            <xsl:choose>
                <xsl:when test="$value and normalize-space($value) and translate(normalize-space($value),'&quot;','')">
                    <xsl:element name="{$type}">
                        <xsl:value-of select="translate(normalize-space($value),'&quot;','')"/>
                    </xsl:element>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:attribute name="gco:nilReason">
                        <xsl:value-of select="'missing'"/>
                    </xsl:attribute>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:element>
    </xsl:template>
    
    <xsl:template name="tag">
        <xsl:param name="tag"/>
        <xsl:param name="value"/>
        <xsl:element name="{$tag}">
            <xsl:choose>
                <xsl:when test="$value">
                    <xsl:value-of select="translate(normalize-space($value),'&quot;','')"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:attribute name="gco:nilReason">
                        <xsl:value-of select="'missing'"/>
                    </xsl:attribute>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:element>
    </xsl:template>
    
    <xsl:template name="codeList">
        <xsl:param name="tag"/>
        <xsl:param name="type"/>
        <xsl:param name="value"/>
        <xsl:param name="location" select="'http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml'"/>
        <xsl:element name="{$tag}">
            <xsl:element name="{$type}">
                <xsl:attribute name="codeList">
                    <xsl:value-of select="$location"/>
                    <xsl:value-of select="'#'"/>
                    <xsl:value-of select="substring-after($type,':')"/>
                </xsl:attribute>
                <xsl:attribute name="codeListValue">
                    <xsl:value-of select="translate(translate(normalize-space($value),'&quot;',''),'\','&quot;')"/>
                </xsl:attribute>
                <xsl:value-of select="translate(translate(normalize-space($value),'&quot;',''),'\','&quot;')"/>
            </xsl:element>
        </xsl:element>
    </xsl:template>
    
    <xsl:template name="responsible">
        <xsl:param name="name"/>
        <xsl:param name="role"/>
        <gmd:CI_ResponsibleParty>
            <xsl:call-template name="type">
                <xsl:with-param name="tag">gmd:organisationName</xsl:with-param>
                <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                <xsl:with-param name="value" select="$name"/>
            </xsl:call-template>
            <xsl:if test="translate(normalize-space($name),'&quot;','') = 'GSFC GMAO'">
                <gmd:contactInfo>
                    <gmd:CI_Contact>
                        <gmd:address>
                            <gmd:CI_Address>
                                <xsl:call-template name="type">
                                    <xsl:with-param name="tag">gmd:deliveryPoint</xsl:with-param>
                                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                    <xsl:with-param name="value">Global Modeling and Assimilation Office, Code 610.1</xsl:with-param>
                                </xsl:call-template>
                                <xsl:call-template name="type">
                                    <xsl:with-param name="tag">gmd:city</xsl:with-param>
                                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                    <xsl:with-param name="value">Greenbelt</xsl:with-param>
                                </xsl:call-template>
                                <xsl:call-template name="type">
                                    <xsl:with-param name="tag">gmd:administrativeArea</xsl:with-param>
                                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                    <xsl:with-param name="value">Maryland</xsl:with-param>
                                </xsl:call-template>
                                <xsl:call-template name="type">
                                    <xsl:with-param name="tag">gmd:postalCode</xsl:with-param>
                                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                    <xsl:with-param name="value">20771</xsl:with-param>
                                </xsl:call-template>
                                <xsl:call-template name="type">
                                    <xsl:with-param name="tag">gmd:country</xsl:with-param>
                                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                    <xsl:with-param name="value">United States</xsl:with-param>
                                </xsl:call-template>
                                <xsl:call-template name="type">
                                    <xsl:with-param name="tag">gmd:electronicMailAddress</xsl:with-param>
                                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                    <xsl:with-param name="value" select="'www@gmao.gsfc.nasa.gov'"/>
                                </xsl:call-template>
                            </gmd:CI_Address>
                        </gmd:address>
                        <gmd:onlineResource>
                            <gmd:CI_OnlineResource>
                                <gmd:linkage>
                                    <gmd:URL>http://gmao.gsfc.nasa.gov</gmd:URL>
                                </gmd:linkage>
                            </gmd:CI_OnlineResource>
                        </gmd:onlineResource>
                    </gmd:CI_Contact>
                </gmd:contactInfo>
            </xsl:if>
            <xsl:call-template name="codeList">
                <xsl:with-param name="tag">gmd:role</xsl:with-param>
                <xsl:with-param name="type">gmd:CI_RoleCode</xsl:with-param>
                <xsl:with-param name="value" select="$role"/>
            </xsl:call-template>
        </gmd:CI_ResponsibleParty>
    </xsl:template>
    
    <xsl:template name="DOI">
        <xsl:param name="value"/>
        <gmx:Anchor xlink:actuate="onRequest" >
            <xsl:choose>
                <xsl:when test="starts-with(translate(normalize-space($value),'&quot;',''), 'http://dx.doi.org/')">
                    <xsl:attribute name="xlink:href">
                        <xsl:value-of select="translate(normalize-space($value),'&quot;','')"/>
                    </xsl:attribute>
                    <xsl:value-of select="'doi:'"/>
                    <xsl:value-of select="translate(normalize-space(substring-after($value, 'http://dx.doi.org/')),'&quot;','')"/>
                </xsl:when>
                <xsl:when test="$value">
                    <xsl:attribute name="xlink:href">
                        <xsl:value-of select="'http://dx.doi.org/'"/>
                        <xsl:value-of select="translate(normalize-space($value),'&quot;','')"/>
                    </xsl:attribute>
                    <xsl:value-of select="'doi:'"/>
                    <xsl:value-of select="translate(normalize-space($value),'&quot;','')"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:attribute name="gco:nilReason">
                        <xsl:value-of select="'missing'"/>
                    </xsl:attribute>
                </xsl:otherwise>
            </xsl:choose>
        </gmx:Anchor>
    </xsl:template>
    
    <xsl:template name="date">
        <xsl:param name="date"/>
        <xsl:param name="type"/>
        <xsl:choose>
            <xsl:when test="contains($date, 'T' ) ">
                <gmd:CI_Date>
                    <xsl:call-template name="type">
                        <xsl:with-param name="tag">gmd:date</xsl:with-param>
                        <xsl:with-param name="type">gco:DateTime</xsl:with-param>
                        <xsl:with-param name="value" select="$date"/>
                    </xsl:call-template>
                    <xsl:if test="$date">
                    <xsl:call-template name="codeList">
                        <xsl:with-param name="tag">gmd:dateType</xsl:with-param>
                        <xsl:with-param name="type">gmd:CI_DateTypeCode</xsl:with-param>
                        <xsl:with-param name="value" select="translate(normalize-space($type),'&quot;','')"/>
                    </xsl:call-template>
                    </xsl:if>
                </gmd:CI_Date>
            </xsl:when>
            <xsl:otherwise>
                <gmd:CI_Date>
                    <xsl:call-template name="type">
                        <xsl:with-param name="tag">gmd:date</xsl:with-param>
                        <xsl:with-param name="type">gco:Date</xsl:with-param>
                        <xsl:with-param name="value" select="$date"/>
                    </xsl:call-template>
                    <xsl:if test="$date">
                    <xsl:call-template name="codeList">
                        <xsl:with-param name="tag">gmd:dateType</xsl:with-param>
                        <xsl:with-param name="type">gmd:CI_DateTypeCode</xsl:with-param>
                        <xsl:with-param name="value" select="translate(normalize-space($type),'&quot;','')"/>
                    </xsl:call-template>
                    </xsl:if>
                </gmd:CI_Date>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    <xsl:template name="eos">
        <xsl:param name="name"/>
        <xsl:param name="code"/>
        <xsl:param name="value"/>
        <eos:additionalAttribute>
            <eos:EOS_AdditionalAttribute>
                <eos:reference>
                    <eos:EOS_AdditionalAttributeDescription>
                        <eos:type>
                            <eos:EOS_AdditionalAttributeTypeCode codeList="http://cdn.earthdata.nasa.gov/iso/resources/Codelist/eosCodelists.xml#EOS_AdditionalAttributeTypeCode" codeListValue="processingInformation">processingInformation</eos:EOS_AdditionalAttributeTypeCode>
                        </eos:type>
                        <eos:identifier>
                            <gmd:MD_Identifier>
                                <xsl:call-template name="type">
                                    <xsl:with-param name="tag">gmd:code</xsl:with-param>
                                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                    <xsl:with-param name="value" select="concat('uuid for ', $name)"/>
                                </xsl:call-template>
                                <xsl:call-template name="type">
                                    <xsl:with-param name="tag">gmd:codeSpace</xsl:with-param>
                                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                                    <xsl:with-param name="value" select="'http://smap.jpl.nasa.gov'"/>
                                </xsl:call-template>
                            </gmd:MD_Identifier>
                        </eos:identifier>
                        <xsl:call-template name="type">
                            <xsl:with-param name="tag">eos:name</xsl:with-param>
                            <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                            <xsl:with-param name="value" select="$name"/>
                        </xsl:call-template>
                        <eos:dataType>
                            <xsl:element name="eos:EOS_AdditionalAttributeDataTypeCode">
                                <xsl:attribute name="codeList">
                                    <xsl:value-of select="'http://cdn.earthdata.nasa.gov/iso/resources/Codelist/eosCodelists.xml#EOS_AdditionalAttributeDataTypeCode'"/>
                                </xsl:attribute>
                                <xsl:attribute name="codeListValue">
                                    <xsl:value-of select="translate(normalize-space($code),'&quot;','')"/>
                                </xsl:attribute>
                                <xsl:value-of select="translate(normalize-space($code),'&quot;','')"/>
                            </xsl:element>
                        </eos:dataType>
                    </eos:EOS_AdditionalAttributeDescription>
                </eos:reference>
                <xsl:call-template name="type">
                    <xsl:with-param name="tag">eos:value</xsl:with-param>
                    <xsl:with-param name="type">gco:CharacterString</xsl:with-param>
                    <xsl:with-param name="value" select="$value"/>
                </xsl:call-template>
            </eos:EOS_AdditionalAttribute>
        </eos:additionalAttribute>
    </xsl:template>

</xsl:stylesheet>
