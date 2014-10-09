'''
Created on Mar 31, 2010

@author: root
'''
#encoding = utf-8

import xml.etree.ElementTree as ET

testxml = '''
        <vm alias="vim1">
            <cpu cores="11" />
            <mem capacity="10" />
            <customer uuid="sdfasdfsad25sdfgs2s" />
            <status is_running="True" />
        </vm>
        '''

try:
    ET.parse("info.xml")
    print("This is a good xml doc")
except Exception, e:
    print("This may be a bad doc")
    print("Error:",e)
    
    
from xml.dom.minidom import parse, parseString   
 
data = """
    <goods>
        <shirt quality="A">
            <size>170</size>
        </shirt>
    </goods>
    """
    
dom = parseString(data)
print dom.toxml()
