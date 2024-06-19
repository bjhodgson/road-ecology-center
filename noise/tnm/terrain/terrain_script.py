import csv
import xml.etree.ElementTree as ET
import xml.dom.minidom

csv_file_name = r"H:\I8_study_area\output_data\clip_test_TableToExcel.csv"
csv_file = csv_file_name

# Read CSV data and organize it by terrain lines
def read_csv(csv_file):
    terrain_lines = {}
    with open(csv_file, newline='') as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            terrain_line_id = row['ORIG_FID']
            if terrain_line_id not in terrain_lines:
                terrain_lines[terrain_line_id] = {'name': 'Terrain Line-' + terrain_line_id, 'points': []}
            point_data = {
                'name': 'Point-' + row['OBJECTID'],
                'pointNumber': row['OBJECTID'],
                'OrderingNumber': row['OBJECTID'],
                'theX': row['Longitude'],
                'theY': row['Latitude'],
                'theZ': row['Z']  # Assuming theZ is always 0 in your CSV data
            }
            terrain_lines[terrain_line_id]['points'].append(point_data)
    return terrain_lines

# Create XML structure
def create_xml_structure(terrain_lines_data):
    root_element = ET.Element("root")
    terrain_lines_element = ET.SubElement(root_element, "terrainLines")
    
    for terrain_line_data in terrain_lines_data.values():
        terrain_line_element = ET.SubElement(terrain_lines_element, "terrainLine")
        name_element = ET.SubElement(terrain_line_element, "name")
        name_element.text = terrain_line_data['name']
        
        comments_element = ET.SubElement(terrain_line_element, "comments")
        comments_element.text = "Object Notes"  # Add your object notes here
        
        points_element = ET.SubElement(terrain_line_element, "points")
        
        for point_data in terrain_line_data['points']:
            point_element = ET.SubElement(points_element, "point")
            for key, value in point_data.items():
                sub_element = ET.SubElement(point_element, key)
                sub_element.text = str(value)
            
            comments_element = ET.SubElement(point_element, "comments")
            comments_element.text = "Point Notes"  # Add your point notes here
    
    return root_element

# Convert CSV data to XML
def csv_to_xml(csv_file, xml_file):
    terrain_lines_data = read_csv(csv_file)
    xml_root = create_xml_structure(terrain_lines_data)
    xml_string = ET.tostring(xml_root, encoding='utf-8', method='xml')
    xml_pretty_string = xml.dom.minidom.parseString(xml_string).toprettyxml(indent="  ")
    with open(xml_file, 'w') as xmlfile:
        xmlfile.write(xml_pretty_string)

# Specify output file path
xml_file_path = r'H:\I8_study_area\output_data\terrain_data.xml'

# Convert CSV to XML
csv_to_xml(csv_file, xml_file_path)

print("XML data has been written successfully.")
