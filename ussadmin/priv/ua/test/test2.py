from common.bridge import moni
from common.rrdtool import updater, grapher, fetcher

from common.utils import current_dir

import json

import os
import ConfigParser

cp = ConfigParser.ConfigParser()
cp.read('ua.conf')
rrds_dir = cp.get('rrd_info', 'rrds_dir')
imgs_dir = cp.get('rrd_info', 'imgs_dir')

rrds_dir_path = os.path.join(os.path.dirname(current_dir()), rrds_dir)
imgs_dir_path = os.path.join(os.path.dirname(current_dir()), imgs_dir)


if __name__ == '__main__':
#    print json.dumps(moni.node(id=2))

    for i in range(50):
        updater.update(rrds_dir=rrds_dir_path)
        import time
        time.sleep(3)
        print 'update one loop'

    
    grapher.graph(rrds_dir_path, imgs_dir_path)

#    fetcher.fetch(rrds_dir, node)