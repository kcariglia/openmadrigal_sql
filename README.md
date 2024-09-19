# The Madrigal scientific database for geospace research

Madrigal is an upper atmospheric science database used by groups throughout the world. Madrigal is a robust, World Wide Web based system capable of managing and serving archival and real-time data, in a variety of formats, from a wide range of upper atmospheric science instruments. Data at each Madrigal site is locally controlled and can be updated at any time, but shared metadata between Madrigal sites allow searching of all Madrigal sites at once from any Madrigal site.

The Madrigal database is designed to hold data about the upper atmosphere. One of the strengths of the Madrigal is that most of the measured parameters it contains are defined in a community standard, whose metadata can be found under the Access metadata tab. With the release of Madrigal 3.0, the file format is now Hdf2 with the restrictions defined in the CEDAR Madrigal Hdf5 file format. This standard defines many frames of reference used commonly in atmospheric science, such as geodetic coordinates, geomagnetic coordinates, or radar (azimuth, elevation, and range) coordinates. Upper atmospheric data that can fit into one of these coordinate systems is a good candidate for Madrigal. Madrigal is not presently designed to handle spacecraft ephemeris in order to determine location, and to date Madrigal has only been used to hold spacecraft data that is independent of position (such as solar wind) or can be converted to one of the coordinate systems discussed above (such as total electron concentration in geodetic coordinates from GPS satellites).

Detailed instructions for installation can be found at http://cedar.openmadrigal.org/docs/name/admin.html

For support, contact brideout@mit.edu, cariglia@mit.edu, or pje@mit.edu.

To cite Madrigal use:

Rideout W., Cariglia K. Openmadrigal URL: http://cedar.openmadrigal.org