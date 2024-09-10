"""convertRecordPlots.py converts all record plots in a Madrigal database to png format, 0 based numbers
"""

# standard python imports
import os.path
import glob
import re
import subprocess


def convertDirectory(plotDir):
    """convertDirectory converts all plots to png
    """
    cmdTemplate = 'gs -dNOPAUSE -dBATCH -q -sDEVICE=png256 -r120 -g1230x1230 -sOutputFile=%s %s'
    reStr = '[0-9][0-9][0-9][0-9][0-9]'
    pngFiles = glob.glob(os.path.join(plotDir, '*[0-9][0-9][0-9][0-9][0-9]*.png'))
    if len(pngFiles) > 0:
        # already converted
        return
    # loop through possible file types
    image_exts = ('jpg', 'jpeg', 'eps')
    for image_ext in image_exts:
        plotFiles = glob.glob(os.path.join(plotDir, '*[0-9][0-9][0-9][0-9][0-9]*.%s' % (image_ext)))
        if len(plotFiles)> 0:
            print(('converting *.%s files in %s' % (image_ext, plotDir)))
            plotFiles.sort()
            for i, plotFile in enumerate(plotFiles):
                basename = os.path.basename(plotFile)
                items = re.split(reStr, basename)
                targetBasename = items[0] + '%05i' % (i) + items[1]
                targetBasename = targetBasename[:targetBasename.rfind('.')+1] + 'png'
                target = os.path.join(plotDir, targetBasename)
                cmd = cmdTemplate % (target, plotFile)
                # temp only
                print(cmd)
                subprocess.check_call(cmd.split())
    
    
# temp only
convertDirectory('/Users/brideout/madroot3/experiments/2013/mlh/16mar13/plots/mlh130316g.001/records')