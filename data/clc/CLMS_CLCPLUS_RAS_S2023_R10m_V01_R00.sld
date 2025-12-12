<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" version="1.0.0" xmlns:ogc="http://www.opengis.net/ogc" xmlns:sld="http://www.opengis.net/sld">
  <UserLayer>
    <sld:LayerFeatureConstraints>
      <sld:FeatureTypeConstraint/>
    </sld:LayerFeatureConstraints>
    <sld:UserStyle>
      <sld:Name>CLMS_CLCPLUS_RAS_S2023_R10m_V00_R00</sld:Name>
      <sld:FeatureTypeStyle>
        <sld:Rule>
          <sld:RasterSymbolizer>
            <sld:ChannelSelection>
              <sld:GrayChannel>
                <sld:SourceChannelName>1</sld:SourceChannelName>
              </sld:GrayChannel>
            </sld:ChannelSelection>
            <sld:ColorMap type="values">
              <sld:ColorMapEntry quantity="1" label="Sealed" color="#ff0000"/>
              <sld:ColorMapEntry quantity="2" label="Woody needle leaved trees" color="#228b22"/>
              <sld:ColorMapEntry quantity="3" label="Woody broadleaved deciduous trees" color="#80ff00"/>
              <sld:ColorMapEntry quantity="4" label="Woody broadleaved evergreen trees" color="#00ff08"/>
              <sld:ColorMapEntry quantity="5" label="Low-growing woody plants" color="#804000"/>
              <sld:ColorMapEntry quantity="6" label="Permanent herbaceous" color="#ccf24d"/>
              <sld:ColorMapEntry quantity="7" label="Periodically herbaceous" color="#ffff80"/>
              <sld:ColorMapEntry quantity="8" label="Lichens and mosses" color="#ff80ff"/>
              <sld:ColorMapEntry quantity="9" label="Non and sparsely vegetated" color="#bfbfbf"/>
              <sld:ColorMapEntry quantity="10" label="Water" color="#0080ff"/>
              <sld:ColorMapEntry quantity="11" label="Snow and ice" color="#00ffff"/>
              <sld:ColorMapEntry quantity="253" label="Coastal seawater buffer" color="#bfdfff"/>
              <sld:ColorMapEntry quantity="254" label="Outside area" color="#e6e6e6"/>
              <sld:ColorMapEntry quantity="255" label="No data" color="#000000"/>
            </sld:ColorMap>
          </sld:RasterSymbolizer>
        </sld:Rule>
      </sld:FeatureTypeStyle>
    </sld:UserStyle>
  </UserLayer>
</StyledLayerDescriptor>
