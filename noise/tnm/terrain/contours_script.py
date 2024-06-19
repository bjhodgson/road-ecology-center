import csv
import xml.etree.ElementTree as ET
import xml.dom.minidom

csv_file_name = r"H:\I8_study_area\output_data\Section10\Section10_contours.csv"
csv_file = csv_file_name

# Read CSV data and organize it by contour zones
def read_csv(csv_file):
    contour_zones = {}
    with open(csv_file, newline='') as csvfile:
        reader = csv.DictReader(csvfile)
        
        for row in reader:
            contour_zone_id = row['ORIG_FID']  # Use 'ORIG_FID' as the contour zone identifier
            if contour_zone_id not in contour_zones:
                contour_zones[contour_zone_id] = {'name': 'Contour Zone-' + contour_zone_id, 'points': []}
            point_data = {
                'name': 'Point-' + row['OBJECTID'],  # Ensure 'OBJECTID' is the correct column name
                'pointNumber': row['OBJECTID'],
                'OrderingNumber': row['OBJECTID'],
                'theX': row['Longitude'],  # Ensure 'Longitude' is the correct column name
                'theY': row['Latitude'],  # Ensure 'Latitude' is the correct column name
                'theZ': '0'  # Assuming theZ is always 0 in your CSV data
            }
            contour_zones[contour_zone_id]['points'].append(point_data)
    return contour_zones

# Create XML structure
def create_xml_structure(contour_zones_data):
    root_element = ET.Element("root")
    contour_zones_element = ET.SubElement(root_element, "contourZones")
    
    for contour_zone_data in contour_zones_data.values():
        contour_zone_element = ET.SubElement(contour_zones_element, "contourZone")
        name_element = ET.SubElement(contour_zone_element, "name")
        name_element.text = contour_zone_data['name']
        
        spacing_element = ET.SubElement(contour_zone_element, "spacing")
        spacing_element.text = "60"  # Add your spacing value here
        
        precision_element = ET.SubElement(contour_zone_element, "precision")
        precision_element.text = "1"  # Add your precision value here
        
        receiver_height_element = ET.SubElement(contour_zone_element, "receiverHeight")
        receiver_height_element.text = "1.5"  # Add your receiver height value here
        
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
xml_file_path = r'H:\I8_study_area\output_data\Section10\contour_data.xml'

# Convert CSV to XML
csv_to_xml(csv_file, xml_file_path)

print("XML data has been written successfully.")
