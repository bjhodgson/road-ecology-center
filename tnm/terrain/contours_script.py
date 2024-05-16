import csv
import xml.etree.ElementTree as ET
import xml.dom.minidom

csv_file_name = "R98_S_CZoneBufferR1km_Erase_points20m.csv"
csv_file = csv_file_name

# Read CSV data and organize it by terrain lines
def read_csv(csv_file):
    contour_zones = {}
    with open(csv_file, newline='') as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            contour_zone_id = row['ORIG_FID']
            if contour_zone_id not in contour_zones:
                contour_zones[contour_zone_id] = {'name': 'Contour Zone-' + contour_zone_id, 'points': []}
            point_data = {
                'name': 'Point-' + row['OBJECTID'],
                'pointNumber': row['OBJECTID'],
                'OrderingNumber': row['OBJECTID'],
                'theX': row['Longitude'],
                'theY': row['Latitude'],
                #'theZ': row['Z']  # Assuming theZ is always 0 in your CSV data
            }
            contour_zones[contour_zone_id]['points'].append(point_data)
    return contour_zones

# Create XML structure
def create_xml_structure(contour_zones_data):
    root_element = ET.Element("root")
    contour_zones_element = ET.SubElement(root_element, "terrainLines")
    
    for contour_zone_data in contour_zones_data.values():
        contour_zone_element = ET.SubElement(contour_zones_element, "terrainLine")
        name_element = ET.SubElement(contour_zone_element, "name")
        name_element.text = contour_zone_data['name']
        
        comments_element = ET.SubElement(contour_zone_element, "comments")
        comments_element.text = "Object Notes"  # Add your object notes here
        
        points_element = ET.SubElement(contour_zone_element, "points")
        
        for point_data in contour_zone_data['points']:
            point_element = ET.SubElement(points_element, "point")
            for key, value in point_data.items():
                sub_element = ET.SubElement(point_element, key)
                sub_element.text = str(value)
            
            comments_element = ET.SubElement(point_element, "comments")
            comments_element.text = "Point Notes"  # Add your point notes here
    
    return root_element

# Convert CSV data to XML
def csv_to_xml(csv_file, xml_file):
    contour_zones_data = read_csv(csv_file)
    xml_root = create_xml_structure(contour_zones_data)
    xml_string = ET.tostring(xml_root, encoding='utf-8', method='xml')
    xml_pretty_string = xml.dom.minidom.parseString(xml_string).toprettyxml(indent="  ")
    with open(xml_file, 'w') as xmlfile:
        xmlfile.write(xml_pretty_string)

# Specify output file path
xml_file_path = r'D:\Documents\road-ecology-center\tnm\terrain\contourzone_data.xml'

# Convert CSV to XML
csv_to_xml(csv_file, xml_file_path)

print("XML data has been written successfully.")
