{
    "projectInformation": {
        "productName": "Delphi-CI-Builder",
        "internalName": "Delphi-CI-Builder (%CI_BUILD_TAG%)",
        "companyName": "Little Earth Solutions",
        "fileDescription": "Awesome example",
        "legalCopyright": "Copyright %year%",
        "companyURL": "http:\/\/www.littleearthsolutions.net\/",
        "stagingVersion": {
            "filename": "",
            "fileVersionMajor": "1",
            "fileVersionMinor": "1",
            "fileVersionRelease": "9",
            "fileVersionBuild": "%CI_VER_BUILD_NUMBER%",
            "productVersionMajor": "1",
            "productVersionMinor": "1",
            "productVersionRelease": "1",
            "productVersionBuild": "%CI_VER_BUILD_NUMBER%"
        },
        "productionVersion": {
            "filename": "",
            "fileVersionMajor": "1",
            "fileVersionMinor": "1",
            "fileVersionRelease": "9",
            "fileVersionBuild": "%CI_VER_BUILD_NUMBER%",
            "productVersionMajor": "1",
            "productVersionMinor": "1",
            "productVersionRelease": "1",
            "productVersionBuild": "%CI_VER_BUILD_NUMBER%"
        },
        "developmentVersion": {
            "filename": "",
            "fileVersionMajor": "1",
            "fileVersionMinor": "1",
            "fileVersionRelease": "9",
            "fileVersionBuild": "666",
            "productVersionMajor": "1",
            "productVersionMinor": "1",
            "productVersionRelease": "1",
            "productVersionBuild": "666"
        }
    },
    "logFolder": "build",
    "variables": [
        {
            "variableName": "DEPLOY_FOLDER",
            "variableValue": "%SystemDrive%\\deploy\\example\\",
            "development": null,
            "staging": null,
            "production": null
        }
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
                    "project": "source\\tests\\DelphiCIDUnitTests.dproj",
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
                    "project": "source\\client\\DelphiCITest.dproj",
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
                    "scriptName": "Example - Client - User - Install",
                    "scriptType": "iss",
                    "filename": "installer\\Example - Client - User - Install.iss",
                    "params": "",
                    "staging": false,
                    "production": null
                },
                {
                    "enabled": true,
                    "scriptName": "Example - Client - System - Install",
                    "scriptType": "iss",
                    "filename": "installer\\Example - Client - System - Install.iss",
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
                    "enabled": true,
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
            "enabled": true,
            "scriptName": "Code Signing Binaries",
            "scriptSource": "bobcodesign.exe \/MODE:SIGN \/PROFILE:LES \/FILENAME:\"%projectFolder%build\\client\\DelphiCITest*.exe\"",
            "staging": null,
            "production": null
        }
    ],
    "buildCompleteScripts": [
        {
            "enabled": true,
            "scriptName": "Deploy - Client User",
            "scriptSource": "xcopy \"%projectFolder%build\\installer\\client\\user\\*\" \"%DEPLOY_FOLDER%\\\" \/S \/Y",
            "staging": false,
            "production": null
        },
        {
            "enabled": true,
            "scriptName": "Deploy - Client System",
            "scriptSource": "xcopy \"%projectFolder%build\\installer\\client\\system\\*\" \"%DEPLOY_FOLDER%\\\" \/S \/Y",
            "staging": false,
            "production": null
        }
    ],
    "checkActiveProcesses": [
    ],
    "reviewFiles": [
        {
            "filename": "resources\\common\\all\\Example - Release Notes.rtf",
            "openWith": ""
        }
    ]
}
