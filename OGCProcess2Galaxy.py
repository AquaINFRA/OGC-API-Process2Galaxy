import json
import xml.etree.ElementTree as ET
import urllib.request 
import warnings
import sys
import xml.dom.minidom as md

#OGC Process Description types to Galaxy Parameter types
typeMapping = {
  "array": "?",
  "boolean": "boolean",
  "integer": "integer",
  "number": "float",
  "object": "data",
  "string": "text"
}

#conformance classes 
confClasses = [
"http://www.opengis.net/spec/ogcapi-processes-1/1.0/conf/core",
"http://www.opengis.net/spec/ogcapi-processes-1/1.0/conf/ogc-process-description",
"http://www.opengis.net/spec/ogcapi-processes-1/1.0/conf/json"
]

def contains_ref(json_obj):
    if isinstance(json_obj, dict):
        if "$ref" in json_obj:
            return True
        for value in json_obj.values():
            if contains_ref(value):
                return True
    elif isinstance(json_obj, list):
        for item in json_obj:
            if contains_ref(item):
                return True
    return False

def OGCAPIProcesses2Galaxy(configFile: str) -> None:
    """
    Function to convert processes form a given list of 
    OGC API Processes instances to Galaxy tools. 

    Parameters:
    configFile (str): Path to config file which specifies the instances as well as 
    included and excluded services. Example: 
    [
        {
            "server_url": "https://someOGCAPIProcessesInstace/api/",
            "included_services": ["*"],
            "excluded_services": ["process1", "process45"]
        }
    ]

    Returns:
    None: Function has no return value. Converted processes are stores as .xml-files.
    """

    #add tool
    tool = ET.Element("tool") 
    tool.set('id', "generic_ogc_processes_wrapper")
    tool.set('name', "Generic OGC Processes Wrapper")
    tool.set('version', "0.1.0")

    #add description
    description = ET.Element("description") 
    description.text = "executes OGC API Processes"
    tool.append(description) 

    #add exit code
    stdio = ET.Element("stdio") 
    exitCode = ET.Element("exit_code")
    exitCode.set("range", "1:") 
    stdio.append(exitCode) 
    tool.append(stdio)

    #add requriements
    requirements = ET.Element("requirements")
    
    requirement1 = ET.Element("requirement")
    requirement1.set("type", "package")
    requirement1.set("version", "4.1.2")
    requirement1.text = "R"

    requirement2 = ET.Element("requirement")
    requirement2.set("type", "package")
    requirement2.set("version", "0.2.3")
    requirement2.text = "httr2"

    requirement3 = ET.Element("requirement")
    requirement3.set("type", "package")
    requirement3.set("version", "1.2.0")
    requirement3.text = "getopt"

    requirement4 = ET.Element("requirement")
    requirement4.set("type", "package")
    requirement4.set("version", "1.8.7")
    requirement4.text = "jsonlite"

    requirements.append(requirement1)
    requirements.append(requirement2)
    requirements.append(requirement3)
    requirements.append(requirement4)

    tool.append(requirements)

    #add command
    command = ET.Element("command")
    command.set("detect_errors", "exit_code")
    commands = []

    #add inputs
    inputs = ET.Element("inputs")

    #load config
    with open(configFile) as configFile:
        configJSON = json.load(configFile)

    conditional_server = ET.Element("conditional")
    conditional_server.set("name", "conditional_server")
    select_server = ET.Element("param")
    select_server.set("name", "select_server")
    select_server.set("type", "select")
    select_server.set("label", "Select server")

    index_i = 0
    for api in configJSON: 
        index_i += 1
        #check conformance
        with urllib.request.urlopen(api["server_url"] + "conformance") as conformanceURL:
            conformanceData = json.load(conformanceURL)
            
            for confClass in confClasses:
                if confClass not in confClasses:
                    msg = "Specified API available via:" + baseURL + " does not conform to " + confClass + "." + "This may lead to issues when converting its processes to Galaxy tools."
                    warnings.warn(msg, Warning)

        server = ET.Element("option")
        server.text = api["server_url"]
        server.set("value", api["server_url"])
        select_server.append(server)

        #make sure select-server dropdown is at the beginning
        if index_i == len(configJSON):
            conditional_server.append(select_server)

        when_server = ET.Element("when")
        when_server.set("value", api["server_url"])

        conditional_process = ET.Element("conditional")
        conditional_process.set("name", "conditional_process")
        select_process = ET.Element("param")
        select_process.set("name", "select_process")
        select_process.set("type", "select")
        select_process.set("label", "Select process")

        with urllib.request.urlopen(api["server_url"] + "processes") as processesURL:
            processesData = json.load(processesURL)
            
            when_list_processes = []
            for process in processesData["processes"][0:70]: #only get 50 processes!
                
                #command information for process
                processCommand = {"server": api["server_url"], "process": process["id"]}

                #check if process is excluded
                if(process["id"] in api["excluded_services"]):
                    continue

                #check if process is included
                if(process["id"] in api["included_services"] or ("*" in api["included_services"] and len(api["included_services"]) == 1)):
                    if len(processesData["processes"]) == 0:
                        msg = "Specified API available via:" + baseURL + " does not provide any processes."
                        #warnings.warn(msg, Warning)

                    with urllib.request.urlopen(api["server_url"] + "processes/" + process["id"]) as processURL:
                        process = json.load(processURL)
                        processElement = ET.Element("option")
                        processElement.text = process["id"] + ": " + process["title"]
                        processElement.set("value", process["id"])
                        select_process.append(processElement)

                        when_process = ET.Element("when")
                        when_process.set("value", process["id"])
                        
                        #inputs for commands 
                        inputCommand = []

                        #iterate over process params
                        for param in process["inputs"]:
                            inputCommand.append(param)
                            process_input = ET.Element("param")

                            #set param name
                            process_input.set("name", param) 
                            
                            #set param title
                            if "title" in process["inputs"][param].keys():
                                    process_input.set("label", param)
                            else:
                                msg = "Parameter " + param + " of process " + process["id"] + " has not title attribute."
                                #warnings.warn(msg, Warning)
                                process_input.set("name", "?")

                            #set optional/required title
                            if "nullable" in process["inputs"][param]["schema"].keys():
                                if process["inputs"][param]["schema"]["nullable"]:
                                    process_input.set("optional", "true")
                                else:
                                    process_input.set("optional", "false")
                            else:
                                process_input.set("optional", "false")
                                msg = "Parameter " + param + " of process " + process["id"] + " has no nullable information."
                                #warnings.warn(msg, Warning)

                            #set default
                            if "default" in process["inputs"][param]["schema"].keys():
                                process_input.set("value", str(process["inputs"][param]["schema"]["default"]))
                            else:
                                msg = "Parameter " + param + " of process " + process["id"] + " has no default value."
                                #warnings.warn(msg, Warning)
                            
                            #set param description
                            if "description" in process["inputs"][param].keys():
                                process_input.set("help", process["inputs"][param]["description"])
                            else:
                                msg = "Parameter " + param + " of process " + process["id"] + " has not description attribute."
                                #warnings.warn(msg, Warning)
                                process_input.set("help", "No description provided!")

                            #set param type
                            if 'type' in process["inputs"][param]["schema"].keys(): #simple schema
                                if process["inputs"][param]["schema"]["type"] in typeMapping.keys():
                                    process_input.set("type", typeMapping[process["inputs"][param]["schema"]["type"]])
                                    if process["inputs"][param]["schema"]["type"] == "boolean":
                                        process_input.set("truevalue", "True") # Galaxy uses this for bools
                                        process_input.set("falsevalue", "False")
                                    if "enum" in process["inputs"][param]["schema"]: #create dropdown if enum exists
                                        process_input.set("type", "select")
                                        for enum in process["inputs"][param]["schema"]["enum"]:
                                            option = ET.Element("option")
                                            option.set("value", enum)
                                            option.text = enum
                                            process_input.append(option)
                            elif contains_ref(process["inputs"][param]["extended-schema"]):
                                process_input.set("type", typeMapping["string"])
                                # text input should be replace with data input to allow building workflows
                                #process_input.set("type", typeMapping["object"])
                                #process_input.set("format", "txt")
                            #elif 'oneOf' in process["inputs"][param]["schema"].keys(): #simple schema
                            #    isComplex = True
                            #    paramFormats = ""
                            #    for format in process["inputs"][param]["schema"]["oneOf"]:
                            #        if format["type"] == "string":
                            #            process_input.set("type", typeMapping["string"])
                            #        if format["type"] == "object":
                            #            process_input.set("type", typeMapping["object"])
                            else:
                                msg = "Parameter " + param + " of process " + process["id"] + " has no simple shema."
                                #warnings.warn(msg, Warning)

                            when_process.append(process_input)
                        
                        #add inputs to command information for process
                        processCommand["inputs"] = inputCommand
                        commands.append(processCommand)

                        for output in process["outputs"]:
                            outputName = output
                            outputLabel = process["outputs"][output]["title"]
                            if "extended-schema" in process["outputs"][output]:
                                process_output = ET.Element("param")
                                process_output.set("type", "select")
                                process_output.set("name", outputName + "_out") #_out needed to avoid duplicates
                                process_output.set("label", outputName)
                                process_output.set("help", outputLabel)
                                for enum in process["outputs"][output]["extended-schema"]["oneOf"][0]["allOf"][1]["properties"]["type"]["enum"]:
                                    output_option = ET.Element("option")
                                    output_option.set("value", enum)
                                    output_option.text=enum
                                    process_output.append(output_option)
                                when_process.append(process_output)

                        when_list_processes.append(when_process)

        conditional_process.append(select_process)
        for when_process in when_list_processes:
            conditional_process.append(when_process)    
        when_server.append(conditional_process)
    conditional_server.append(when_server)

    #export tool 
    inputs.append(conditional_server)
    tool.append(inputs)

    #add outputs
    outputs = ET.Element("outputs")
    collection = ET.Element("collection")
    collection.set("name", "output_data")
    collection.set("type", "list")
    collection.set("label", "Output")
    discover_datasets = ET.Element("discover_datasets")
    discover_datasets.set("pattern", "__name_and_ext__")
    collection.append(discover_datasets)
    outputs.append(collection)
    tool.append(outputs)

    #add help
    helpElement = ET.Element("help")
    helpElement.text= "Please help"
    tool.append(helpElement)

    #add citation
    citations = ET.Element("citations")
    citation1 = ET.Element("citation")
    citation1.set("type", "bibtex")
    citation1.text = "@Manual{httr2, title = {httr2: Perform HTTP Requests and Process the Responses}, author = {Hadley Wickham}, year = {2023}, note = {R package version 1.0.0, https://github.com/r-lib/httr2}, url = {https://httr2.r-lib.org},}"
    citation2 = ET.Element("citation")
    citation2.set("type", "doi")
    citation2.text = "10.48550/arXiv.1403.2805"
    citations.append(citation1)
    citations.append(citation2)
    tool.append(citations)

    #add command
    commandText = "<![CDATA["

    for i in range(0, len(commands)):
        if i == 0:
            commandText += "\n#if $conditional_server.select_process == \"" + commands[i]["process"] + "\":\n"
            commandText += "\tRscript '$__tool_directory__/generic.R'\n"
            commandText += "\t\t--server '$select_server' \n"
            commandText += "\t\t--process '$select_process'"
            for y in commands[i]["inputs"]:
                commandText += "\n\t\t--"+ y + " \'$" + y + "\'"
            commandText += "\n\t\t--outputData '$output_data'"
        else:
            commandText += "\n#elif $conditional_server.select_process == \"" + commands[i]["process"] + "\":\n"
            commandText += "\tRscript '$__tool_directory__/generic.R'\n"
            commandText += "\t\t--server '$select_server' \n"
            commandText += "\t\t--process '$select_process'"
            for y in commands[i]["inputs"]:
                commandText += "\n\t\t--"+ y + " \'$" + y + "\'"
            commandText += "\n\t\t--outputData '$output_data'"
    commandText += "\n#end if\n]]>"
    command.text = commandText
    tool.append(command)
    with open ("generic.xml", "wb") as toolFile: 
        toolString = ET.tostring(tool, encoding='unicode')
        toolString = toolString.replace("&lt;", "<")
        toolString = toolString.replace("&gt;", ">")

        toolString = md.parseString(toolString)
        toolString = toolString.toprettyxml()

        f = open("generic.xml", "a")
        f.write(toolString)
        f.close()
    print("--> generic.xml exported")

if __name__ == "__main__":
    configFile = " ".join( sys.argv[1:] )
    OGCAPIProcesses2Galaxy(configFile)
    