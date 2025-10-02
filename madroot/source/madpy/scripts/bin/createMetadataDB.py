"""
Script that checks if metadataDB exists.
If not, creates and populates the SQLite metadata database.
"""

import datetime
import os, os.path
import sys
import sqlite3
import traceback

import madrigal.metadata

madDB = madrigal.metadata.MadrigalDB()

def populateinstParmTab(cur):
    tblTemplate = """CREATE TABLE instParmTab (
        kinst INTEGER,
        parm TEXT,
        FOREIGN KEY (kinst) REFERENCES instTab(kinst),
        FOREIGN KEY (parm) REFERENCES parmCodes(mnem)
    );"""
    cur.execute(tblTemplate)


def populateTypeTab(cur):
    tblTemplate = """CREATE TABLE typeTab (
        kindat TEXT,
        desc TEXT
    );"""
    cur.execute(tblTemplate)

    template = """INSERT INTO typeTab VALUES(?, ?)"""

    with open(os.path.join(madDB.getMetadataDir(), "typeTab.txt"), "r") as f:
        for line in f:
            line = line.rstrip()
            line = line.split(',')
            cur.execute(template, (line[0], line[1]))


def populateParmCodes(cur):
    tblTemplate = """CREATE TABLE parmCodes (
        code INTEGER,
        desc TEXT,
        units TEXT,
        mnem TEXT,
        format TEXT,
        width INTEGER,
        category INTEGER,
        html BOOLEAN,
        errhtml BOOLEAN,
        PRIMARY KEY (mnem),
        FOREIGN KEY (category) REFERENCES madCatTab(code)
    );"""
    cur.execute(tblTemplate)

    template = """INSERT INTO parmCodes VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?)"""

    with open(os.path.join(madDB.getMetadataDir(), "parmCodes.txt"), "r") as f:
        for line in f:
            if (line[0] == '#'):
                continue
            line = line.rstrip()
            line = line.split(',')
            cur.execute(template, (line[0], line[1], line[2], line[3],
                                   line[4], line[5], line[6], line[7],
                                   line[8]))


def populateMadCatTab(cur):
    tblTemplate = """CREATE TABLE madCatTab (
        code INTEGER,
        catname TEXT,
        mincode INTEGER,
        maxcode INTEGER,
        PRIMARY KEY (code)
    );"""
    cur.execute(tblTemplate)

    template = """INSERT INTO madCatTab VALUES(?, ?, ?, ?)"""

    with open(os.path.join(madDB.getMetadataDir(), "madCatTab.txt"), "r") as f:
        for line in f:
            line = line.rstrip()
            line = line.split(',')
            cur.execute(template, (line[0], line[1], line[2], line[3]))



def populateInstType(cur):
    tblTemplate = """CREATE TABLE instType (
        category INTEGER,
        desc TEXT,
        PRIMARY KEY (category)
    );"""
    cur.execute(tblTemplate)

    template = """INSERT INTO instType VALUES(?, ?)"""

    with open(os.path.join(madDB.getMetadataDir(), "instType.txt"), "r") as f:
        for line in f:
            line = line.rstrip()
            line = line.split(',')
            cur.execute(template, (line[0], line[1]))


def populateInstTab(cur):
    tblTemplate = """CREATE TABLE instTab (
        kinst INTEGER,
        mnem TEXT,
        name TEXT,
        lat REAL,
        lon REAL,
        alt REAL,
        cname TEXT,
        cadr1 TEXT,
        cadr2 TEXT,
        cadr3 TEXT,
        ccity TEXT,
        cstate TEXT,
        ccode TEXT,
        ccountry TEXT,
        cphone TEXT,
        cemail TEXT,
        category INTEGER NOT NULL,
        PRIMARY KEY (kinst),
        FOREIGN KEY(category) REFERENCES instType(category)
    );"""
    cur.execute(tblTemplate)

    template = """INSERT INTO instTab VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"""

    with open(os.path.join(madDB.getMetadataDir(), "instTab.txt"), "r") as f:
        for line in f:
            line = line.rstrip()
            line = line.split(',')
            cur.execute(template, (line[0], line[1], line[2], line[3],
                                   line[4], line[5], line[6], line[7], 
                                   line[8], line[9], line[10], line[11],
                                   line[12], line[13], line[14], line[15],
                                   line[16]))


def populateSiteTab(cur):
    tblTemplate = """CREATE TABLE siteTab (
        id INTEGER NOT NULL,
        name TEXT,
        server TEXT,
        docdir TEXT,
        cgidir TEXT,
        servlet TEXT,
        cname TEXT,
        cadr1 TEXT,
        cadr2 TEXT,
        cadr3 TEXT,
        ccity TEXT,
        cstate TEXT,
        ccode TEXT,
        ccountry TEXT,
        cphone TEXT,
        cemail TEXT,
        version REAL,
        PRIMARY KEY (id)
    );"""
    cur.execute(tblTemplate)

    template = """INSERT INTO siteTab VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"""

    with open(os.path.join(madDB.getMetadataDir(), "siteTab.txt"), "r") as f:
        for line in f:
            line = line.rstrip()
            line = line.split(',')
            if len(line) == 16:
                # no site version, use default (3.0)
                line.append('3.0')
            if len(line) == 17:
                cur.execute(template, (line[0], line[1], line[2], line[3],
                                        line[4], line[5], line[6], line[7], 
                                        line[8], line[9], line[10], line[11],
                                        line[12], line[13], line[14], line[15],
                                        line[16]))



def populateFileTab(cur):
    tblTemplate = """CREATE TABLE fileTab (
        fname TEXT NOT NULL,
        eid INTEGER NOT NULL,
        kindat INTEGER NOT NULL,
        category INTEGER NOT NULL,
        fsize INTEGER NOT NULL,
        catrec BOOLEAN NOT NULL,
        headrec BOOLEAN NOT NULL,
        amoddate TEXT NOT NULL,
        amodtime TEXT NOT NULL,
        status TEXT NOT NULL,
        permission BOOLEAN NOT NULL,
        fanalyst TEXT,
        fanalystemail TEXT,
        idx INTEGER,
        PRIMARY KEY (idx),
        FOREIGN KEY (eid) REFERENCES expTab(id) ON UPDATE CASCADE,
        FOREIGN KEY (kindat) REFERENCES typeTab(kindat),
        CHECK (category > 0 AND category <= 4)
    );"""
    cur.execute(tblTemplate)

    template = """INSERT INTO fileTab(fname, eid, kindat, category, fsize, catrec, headrec, amoddate, amodtime, status, permission, fanalyst, fanalystemail) VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"""

    if os.access(os.path.join(madDB.getMetadataDir(), "fileTab.txt"), os.R_OK):
        fileMetadata = open(os.path.join(madDB.getMetadataDir(), "fileTab.txt"), "r")
        fileText = fileMetadata.read()
        fileMetadata.close()

        fileLines = fileText.split('\n')
        splitList = [line.split(',') for line in fileLines]
        # append empty strings if no file analyst info
        noAnalyst = [(line + ["", ""]) for line in splitList if (len(line) == 11)]
        otherLines = [line for line in splitList if (len(line) == 13)]
        fileList = otherLines + noAnalyst
        fileData = [tuple(line) for line in fileList if ((len(line) > 1))]
        
        cur.executemany(template, fileData)
            

def populateExpTab(cur):
    tblTemplate = """CREATE TABLE expTab (
        id INTEGER,
        url TEXT UNIQUE NOT NULL,
        name TEXT NOT NULL,
        sid INTEGER NOT NULL,
        sdt TEXT NOT NULL,
        edt TEXT NOT NULL,
        kinst INTEGER NOT NULL,
        security INTEGER NOT NULL,
        pi TEXT,
        piemail TEXT,
        idx INTEGER,
        PRIMARY KEY (idx),
        CHECK (security >= -1 AND security < 4),
        FOREIGN KEY(sid) REFERENCES siteTab(id),
        FOREIGN KEY(kinst) REFERENCES instTab(kinst)
    );
    """
    cur.execute(tblTemplate)

    template = """INSERT INTO expTab('id', 'url', 'name', 'sid', 'sdt', 'edt', 'kinst', 'security', 'pi', 'piemail') VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"""

    if os.access(os.path.join(madDB.getMetadataDir(), 'expTab.txt'), os.R_OK):
        expFile = open(os.path.join(madDB.getMetadataDir(), 'expTab.txt'), 'r')
        expText = expFile.read()
        expFile.close()

        expLines = expText.split('\n')
        splitList = [line.split(',') for line in expLines]
        # append empty strings if no pi info
        noPiLines = [(line + ["", ""]) for line in splitList if (len(line) == 10)]
        otherLines = [line for line in splitList if (len(line) == 12)]
        expList = otherLines + noPiLines
        expData = [tuple([line[0], line[1], line[2], line[3],
                        line[4] + f'{line[5]:>06}', line[6] + f'{line[7]:>06}',
                        line[8], line[9], line[10], line[11]]) for line in expList if ((len(line) > 1))]
        
        cur.executemany(template, expData)


if __name__ == "__main__":
    s = datetime.datetime.now()
    metaDB = os.path.join(madDB.getMetadataDir(), "metadata.db")

    if os.access(metaDB, os.R_OK):
        print("Testing if metadata.db exists... yes")
        sys.exit()
    else:
        print("Testing if metadata.db exists... no")
    
    try:
        # will create metaDB if it doesnt exist, connect if it does
        con = sqlite3.connect(metaDB)

        cur = con.cursor()

        populateinstParmTab(cur)
        populateTypeTab(cur)
        populateParmCodes(cur)
        populateMadCatTab(cur)
        populateInstType(cur)
        populateInstTab(cur)
        populateSiteTab(cur)
        populateFileTab(cur)
        populateExpTab(cur)
        
        con.commit()
        con.close()

        os.chmod(metaDB, 0o664)
    except Exception as e:
        if os.access(metaDB, os.R_OK):
            print("Error occured during creation of metadata.db, aborting")
            # if an error occurs during the initial creation of metaDB, remove it
            os.remove(metaDB)
            traceback.print_exception(e)
            sys.exit()

    e = datetime.datetime.now()
    delta = e - s
    print("metadata.db creation took {} s".format(delta.seconds))