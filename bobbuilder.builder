{
    "projectInformation": {
        "productName": "BOB Builder",
        "internalName": "BOB",
        "companyName": "Little Earth Solutions",
        "fileDescription": "BOB Builder Tools",
        "legalCopyright": "Copyright Little Earth Solutions %year%",
        "companyURL": "http:\/\/www.littleearthsolutions.net\/",
        "stagingVersion": {
            "filename": "",
            "fileVersionMajor": "1",
            "fileVersionMinor": "0",
            "fileVersionRelease": "8",
            "fileVersionBuild": "0",
            "productVersionMajor": "1",
            "productVersionMinor": "0",
            "productVersionRelease": "0",
            "productVersionBuild": "0"
        },
        "productionVersion": {
            "filename": "",
            "fileVersionMajor": "1",
            "fileVersionMinor": "0",
            "fileVersionRelease": "8",
            "fileVersionBuild": "0",
            "productVersionMajor": "1",
            "productVersionMinor": "0",
            "productVersionRelease": "0",
            "productVersionBuild": "0"
        },
        "developmentVersion": {
            "filename": "",
            "fileVersionMajor": "1",
            "fileVersionMinor": "0",
            "fileVersionRelease": "8",
            "fileVersionBuild": "0",
            "productVersionMajor": "1",
            "productVersionMinor": "0",
            "productVersionRelease": "0",
            "productVersionBuild": "0"
        }
    },
    "logFolder": "build",
    "variables": [
    ],
    "buildFolders": [
        {
            "folder": "build",
            "cleanupEnabled": true
        }
    ],
    "defaultProjectGroups": "[ALL]",
    "defaultInstallScriptGroups": "[ALL]",
    "defaultTestProjectGroups": "[ALL]",
    "defaultBuildCompleteScripts": "[ALL]",
    "testProjectGroups": [
        {
            "enabled": true,
            "group": "Tests",
            "projects": [
                {
                    "params": "-exit:continue",
                    "command": "",
                    "folder": "%projectFolder%build\\tests\\bin\\$(Platform)\\$(Config)\\",
                    "postBuild": null,
                    "project": "source\\tests\\bobtests.dproj",
                    "platforms": "Win32;Win64",
                    "configs": "Release",
                    "properties": "",
                    "enabled": true,
                    "staging": null,
                    "production": null
                }
            ]
        }
    ],
    "projectGroups": [
        {
            "enabled": true,
            "group": "Projects",
            "projects": [
                {
                    "project": "source\\builder\\bobbuilder.dproj",
                    "platforms": "Win32;Win64",
                    "configs": "Release",
                    "properties": "",
                    "enabled": true,
                    "staging": null,
                    "production": null
                },
                {
                    "project": "source\\builder\\bobbuildergui.dproj",
                    "platforms": "Win32;Win64",
                    "configs": "Release",
                    "properties": "",
                    "enabled": true,
                    "staging": null,
                    "production": null
                },
                {
                    "project": "source\\codesign\\bobcodesign.dproj",
                    "platforms": "Win32;Win64",
                    "configs": "Release",
                    "properties": "",
                    "enabled": true,
                    "staging": null,
                    "production": null
                },
                {
                    "project": "source\\compare\\bobcompare.dproj",
                    "platforms": "Win32;Win64",
                    "configs": "Release",
                    "properties": "",
                    "enabled": true,
                    "staging": null,
                    "production": null
                },
                {
                    "project": "source\\library\\boblibrary.dproj",
                    "platforms": "Win32;Win64",
                    "configs": "Release",
                    "properties": "",
                    "enabled": true,
                    "staging": null,
                    "production": null
                },
                {
                    "project": "source\\maptojdbg\\bobmaptojdbg.dproj",
                    "platforms": "Win32;Win64",
                    "configs": "Release",
                    "properties": "",
                    "enabled": true,
                    "staging": null,
                    "production": null
                },
                {
                    "project": "source\\md5\\bobmd5.dproj",
                    "platforms": "Win32;Win64",
                    "configs": "Release",
                    "properties": "",
                    "enabled": true,
                    "staging": null,
                    "production": null
                },
                {
                    "project": "source\\resource\\bobresource.dproj",
                    "platforms": "Win32;Win64",
                    "configs": "Release",
                    "properties": "",
                    "enabled": true,
                    "staging": null,
                    "production": null
                },
                {
                    "project": "source\\wixwrapper\\bobwixwrapper.dproj",
                    "platforms": "Win32;Win64",
                    "configs": "Release",
                    "properties": "",
                    "enabled": true,
                    "staging": null,
                    "production": null
                }
            ]
        }
    ],
    "installScriptGroups": [
        {
            "enabled": true,
            "group": "Installers",
            "installScripts": [
                {
                    "enabled": true,
                    "scriptName": "BOB - Installer",
                    "scriptType": "iss",
                    "filename": "installer\\BOB - Installer.iss",
                    "params": "",
                    "staging": false,
                    "production": null
                },
                {
                    "enabled": true,
                    "scriptName": "Build MSI",
                    "scriptType": "exe",
                    "filename": "bobwixwrapper.exe",
                    "params": "\/EXEFILENAME:\"%projectFolder%build\\installer\\*.exe\" \/MSIFILENAME:\"%%exefilepath%%\\MSI\\%%msifilename%%\" \/COMPANYURL:%companyURL%",
                    "staging": false,
                    "production": null
                },
                {
                    "enabled": false,
                    "scriptName": "Code Signing Installers",
                    "scriptType": "exe",
                    "filename": "bobcodesign.exe",
                    "params": "\/MODE:SIGN \/PROFILE:LES \/FILENAME:\"%projectFolder%build\\installer\\*.exe;%projectFolder%build\\installer\\*.msi\"",
                    "staging": false,
                    "production": null
                },
                {
                    "enabled": true,
                    "scriptName": "Generate Installer MD5",
                    "scriptType": "exe",
                    "filename": "bobmd5.exe",
                    "params": "\/MODE:GENERATE \/SOURCE:\"%projectFolder%build\\installer\\*.exe;%projectFolder%build\\installer\\*.msi\"",
                    "staging": false,
                    "production": null
                }
            ]
        }
    ],
    "postCleanupScripts": [
    ],
    "preBuildScripts": [
        {
            "enabled": true,
            "scriptName": "Version resource template",
            "scriptSource": "bobresource.exe \/SOURCE:\"%projectFolder%\\resources\\versioning\\version_global.template\" \/DESTINATION:\"%projectFolder%\\build\\resource\\version_global.rc\"",
            "staging": null,
            "production": null
        }
    ],
    "postBuildScripts": [
        {
            "enabled": true,
            "scriptName": "Convert MAP to JDBG",
            "scriptSource": "bobmaptojdbg.exe \"%projectFolder%build\\\"",
            "staging": null,
            "production": null
        },
        {
            "enabled": false,
            "scriptName": "Code Signing Binaries",
            "scriptSource": "bobcodesign.exe \/MODE:SIGN \/PROFILE:LES \/FILENAME:\"%projectFolder%build\\client\\DelphiCITest*.exe\"",
            "staging": null,
            "production": null
        }
    ],
    "buildCompleteScripts": [
    ],
    "checkActiveProcesses": [
    ],
    "reviewFiles": [
        {
            "filename": "resources\\common\\all\\BOB Builder - Release Notes.rtf",
            "openWith": ""
        },
        {
            "filename": "resources\\common\\all\\BOB Builder - Support Details.rtf",
            "openWith": ""
        },
        {
            "filename": "resources\\common\\all\\BOB Builder - License.rtf",
            "openWith": ""
        }
    ]
}
