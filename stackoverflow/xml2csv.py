import xml.etree.ElementTree as ET
import xmltodict
import csv
from collections import defaultdict

tree = ET.parse('myxml_1.xml')
xml_data = tree.getroot()
# xmlstr = ET.tostring(xml_data, encoding='utf-8', method='xml')
# data_dict = dict(xmltodict.parse(xmlstr))

with open('test1.csv', 'w') as f:
    w = csv.writer(f, delimiter="|", quotechar='"')
    values = defaultdict(list)
    columns = []
    for abc in xml_data.iter("abc"):
        for child in abc:
            if child.tag not in columns:
                columns.append(child.tag)
            values[child.tag].append(child.text)

    w.writerow(columns)
    for abc in xml_data.iter("abc"):
        row = []
        for col in columns:
            elem = abc.find(col)
            
            row.append(elem.text if elem else "x")
        w.writerow(row)

    # w.writerow(data_dict.keys())
    # w.writerow(data_dict.values())
