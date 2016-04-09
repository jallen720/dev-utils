import sys
import shutil
import os
import fileinput
import devUtils.dirUtils as dirUtils


def checkMakeToDirs(unitName, fromSubdir, toSubdir):
    def checkMakeToDir(toDir):
        if not os.path.isdir(toDir):
            os.makedirs(toDir)

    for rootDir, extensions in dirUtils.ROOT_DIR_EXTENSIONS.items():
        for extension in extensions:
            if os.path.isfile(rootDir + fromSubdir + unitName + extension):
                checkMakeToDir(rootDir + toSubdir)


def getHeaderPaths(unitName, fromSubdir, toSubdir):
    INCLUDE_ROOT_DIR = "include/"

    headerPaths = {
        "from" : [],
        "to"   : [],
    }

    for headerExtension in dirUtils.ROOT_DIR_EXTENSIONS[INCLUDE_ROOT_DIR]:
        fileName       = unitName + headerExtension
        fromHeaderPath = fromSubdir + fileName
        toHeaderPath   = toSubdir + fileName

        if os.path.isfile(INCLUDE_ROOT_DIR + fromHeaderPath):
            headerPaths["from"].append(fromHeaderPath)
            headerPaths["to"].append(toHeaderPath)

    return headerPaths


def shouldCheckSourceFile(filePath, fromHeaderPaths):
    def needsIncludesUpdated():
        with open(filePath, "r") as fileHandle:
            for line in fileHandle:
                for fromHeaderPath in fromHeaderPaths:
                    if fromHeaderPath in line:
                        return True

        return False

    return len(fromHeaderPaths) > 0 and needsIncludesUpdated()


def updateIncludes(unitName, fromSubdir, toSubdir):
    headerPaths = getHeaderPaths(unitName, fromSubdir, toSubdir)

    def forEachSourceFile(fileCB):
        for rootDir in dirUtils.ROOT_DIR_EXTENSIONS:
            for root, subdirs, fileNames in os.walk(rootDir):
                for fileName in fileNames:
                    fileCB(os.path.join(root, fileName))

    def getUpdatedLine(line):
        updatedLine = line

        for i in range(len(headerPaths["from"])):
            fromHeaderPath = headerPaths["from"][i]
            toHeaderPath   = headerPaths["to"][i]

            if fromHeaderPath in line:
                updatedLine = line.replace(fromHeaderPath, toHeaderPath)
                break

        return updatedLine

    def checkUpdateIncludes(filePath):
        if shouldCheckSourceFile(filePath, headerPaths["from"]):
            with fileinput.FileInput(filePath, inplace = True) as fileHandle:
                for line in fileHandle:
                    print(getUpdatedLine(line), end = "")

    forEachSourceFile(checkUpdateIncludes)


def moveFile(fileName, fromDir, toDir):
    shutil.move(fromDir + fileName, toDir + fileName)
    print("moved \"" + fromDir + fileName + "\" -> \"" + toDir + fileName + "\"")


def moveFiles(unitName, fromSubdir, toSubdir):
    for rootDir, extensions in dirUtils.ROOT_DIR_EXTENSIONS.items():
        for extension in extensions:
            if os.path.isfile(rootDir + fromSubdir + unitName + extension):
                moveFile(
                    unitName + extension,
                    rootDir  + fromSubdir,
                    rootDir  + toSubdir)


def checkRemoveEmptyFromSubdir(fromSubdir):
    for rootDir in dirUtils.ROOT_DIR_EXTENSIONS:
        for root, subdirs, files in os.walk(rootDir + fromSubdir):
            if not files and not subdirs:
                os.rmdir(rootDir + fromSubdir)


def moveUnit(unitName, fromSubdir, toSubdir):
    checkMakeToDirs(unitName, fromSubdir, toSubdir)
    updateIncludes (unitName, fromSubdir, toSubdir)
    moveFiles      (unitName, fromSubdir, toSubdir)
    checkRemoveEmptyFromSubdir(fromSubdir)
