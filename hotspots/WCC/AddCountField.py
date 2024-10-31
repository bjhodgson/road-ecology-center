from qgis.core import QgsProject, QgsField
from PyQt5.QtCore import QVariant

# Define the layer and field names
layer_name = 'wcc-97-581-129-559'  # Replace with the name of your layer
source_field = 'total_nc'
new_field = 'count'

# Get the layer from the project
layer = QgsProject.instance().mapLayersByName(layer_name)[0]

if layer is None:
    print(f"Layer '{layer_name}' not found.")
else:
    # Start an editing session
    layer.startEditing()
    
    # Check if the field already exists
    if new_field in [field.name() for field in layer.fields()]:
        print(f"Field '{new_field}' already exists.")
    else:
        # Add the new field with Integer type
        layer.dataProvider().addAttributes([QgsField(new_field, QVariant.Int)])
        layer.updateFields()
    
    # Update the new field with values from the source field
    field_index = layer.fields().indexFromName(source_field)
    new_field_index = layer.fields().indexFromName(new_field)
    
    for feature in layer.getFeatures():
        # Ensure to handle possible conversion errors
        try:
            feature[new_field_index] = int(feature[field_index])
        except ValueError:
            feature[new_field_index] = None  # or handle as needed
        layer.updateFeature(feature)
    
    # Commit changes
    layer.commitChanges()
    print(f"Field '{new_field}' has been created and populated with integer values from '{source_field}'.")
