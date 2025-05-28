# $Id: Makefile.gnu 7722 2024-09-10 15:32:13Z brideout $
#
# ----------------------------------------------------------------------
#       Makefile for madrigal - now simply builds distribution
# ----------------------------------------------------------------------


all: distribution


distribution:
	@echo "*** Making compressed tar files of madrigal distribution ***"
	rm -f madrigal.tar.gz
	cp metadata/siteTab.txt metadata/siteTab.txt.original
	cp metadata/instTab.txt metadata/instTab.txt.original
	cp metadata/instType.txt metadata/instType.txt.original
	cp metadata/typeTab.txt metadata/typeTab.txt.original
	tar cvfh madrigal.tar -T MANIFEST
	gzip madrigal.tar
	rm -f metadata_update.tar.gz
	tar cvf metadata_update.tar metadata/typeTab.txt metadata/instTab.txt metadata/siteTab.txt
	gzip metadata_update.tar
	rm -f experiments.tar.gz
	tar cvf experiments.tar -T MANIFEST_EXP
	gzip experiments.tar
	rm -f geofil.tar.gz
	tar cvf geofil.tar -T MANIFEST_GEO
	gzip geofil.tar

