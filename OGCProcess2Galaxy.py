import json
import xml.etree.ElementTree as ET
import urllib.request 
import warnings
import sys
import xml.dom.minidom as md

#OGC Process Description types to galaxy parameter types
typeMapping = {
  "array": "text",
  "boolean": "boolean",
  "integer": "integer",
  "number": "float",
  "object": "data",
  "string": "text"
}

#Recognized media types
mediaTypes = ["image/tiff", 
              "image/jpeg", 
              "image/png", 
              "text/xml", 
              "text/plain", 
              "application/octet-stream"]

#Conformance classes 
confClasses = [
"http://www.opengis.net/spec/ogcapi-processes-1/1.0/conf/core",
"http://www.opengis.net/spec/ogcapi-processes-1/1.0/conf/ogc-process-description",
"http://www.opengis.net/spec/ogcapi-processes-1/1.0/conf/json"
]

def contains_ref(json_obj):
    try:
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
    except:
        return False

def distinct_subarray(arr):
    subarray = []
    seen = set()
    for item in arr:
        if item not in seen:
            subarray.append(item)
            seen.add(item)
    return subarray

def OGCAPIProcesses2Galaxy(configFile: str) -> None:
    """
    Function to convert processes form a given list of 
    OGC API Processes instances to Galaxy tools. 

    Parameters:
    configFile (str): Path to config file which specifies the instances as well as 
    included and excluded services. Example: 
    {
        "servers":[
            {
                "server_url": "https://exampleEndpoint",
                "included_services": ["*"],
                "filter": "",
                "excluded_services": []
            }
        ],
        "id":"exampleToolID",
        "title":"Example title",
        "version":"0.1.0",
        "description": "Some description text.",
        "help": "Some help text."
    }

    Returns:
    None: Function has no return value. Converted processes are stores as .xml-files.
    """

    #load config
    with open(configFile) as configFile:
        configJSON = json.load(configFile)

    #add tool
    tool = ET.Element("tool") 
    tool.set('id', configJSON["id"])
    tool.set('name', configJSON["title"])
    tool.set('version', configJSON["version"])

    #add description
    description = ET.Element("description")
    description.text = configJSON["description"]
    tool.append(description)
    
    #add help
    help = ET.Element("help")
    help.text = configJSON["help"]

    #add macro
    macros = ET.Element("macros")
    importMacro = ET.Element("import")
    importMacro.text = "macros.xml"
    macros.append(importMacro)
    tool.append(macros)

    #add requirements
    requirements = ET.Element("expand")
    requirements.set("macro", "requirements")
    tool.append(requirements)

    #add command
    command = ET.Element("command")
    command.set("detect_errors", "exit_code")
    commands = []

    #add inputs
    inputs = ET.Element("inputs")

    index_i = 0
    for api in configJSON["servers"]: 
        #get server 
        server = api["server_url"]

        index_i += 1

        #check conformance
        with urllib.request.urlopen(api["server_url"] + "conformance") as conformanceURL:
            #Retrieve conformance data
            conformanceData = json.load(conformanceURL)
            
            #Set conformance to True
            conformance = True

            #Iterate over conformance classes
            for confClass in confClasses:
                #Ceck if conformance class is implemented
                if confClass not in conformanceData["conformsTo"]:
                    #Create warnning and set conformance to False if certain conformance class is not implemented
                    msg = "Specified API available via:" + api["server_url"] + " does not conform to " + confClass + ". This may lead to issues when converting its processes to Galaxy tools."
                    warnings.warn(msg, Warning)
                    conformance = False               

            #Set help text for tool
            if conformance:
                help.text = configJSON["help"]
            else:
                #If API might not be complient with OGC processes API add notification to help text
                help.text = configJSON["help"] + " Take note that the service provided by this does not implement all nesseracy OGC API Processes conformance classes and might thus not behave as expected!"

        #Create process selectors
        conditional_process = ET.Element("conditional")
        conditional_process.set("name", "conditional_process")
        select_process = ET.Element("param")
        select_process.set("name", "select_process")
        select_process.set("type", "select")
        select_process.set("label", "Select process")

        with urllib.request.urlopen(api["server_url"] + "processes" + api["filter"]) as processesURL:
            #retrieve process data
            processesData = json.load(processesURL)
            
            when_list_processes = []
            #Iterate over processes
            for process in processesData["processes"]: #only get 50 processes!
                
                #command information for process
                processCommand = {"server": api["server_url"], "process": process["id"]}

                #check if process is excluded
                if(process["id"] in api["excluded_services"]):
                    continue

                #check if process is included
                if(process["id"] in api["included_services"] or ("*" in api["included_services"] and len(api["included_services"]) == 1)):
                    with urllib.request.urlopen(api["server_url"] + "processes/" + process["id"]) as processURL:
                        #Retrieve process data
                        process = json.load(processURL)
                        processElement = ET.Element("option")

                        #Set title
                        if("title" in process.keys()):
                            processElement.text = process["id"] + ": " + process["title"]
                        else:
                            processElement.text = process["id"]  

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
                            process_input.set("name", param.replace(".", "_")) 
                            
                            #set param title
                            if "title" in process["inputs"][param].keys():
                                #process_input.set("label", param)
                                process_input.set("label", process["inputs"][param]["title"])
                            else:
                                process_input.set("label", param)

                            #check if param is optional
                            optional = False
                            if "nullable" in process["inputs"][param]["schema"].keys():    
                                if process["inputs"][param]["schema"]["nullable"]:
                                    process_input.set("optional", "true")
                                    optional = True
                            if "minOccurs" in process["inputs"][param].keys():
                                if process["inputs"][param]["minOccurs"] == 0:
                                    process_input.set("optional", "true")
                                    optional = True
                            if optional == False:
                                process_input.set("optional", "false")

                            #set default
                            if "default" in process["inputs"][param]["schema"].keys():
                                process_input.set("value", str(process["inputs"][param]["schema"]["default"]))
                            
                            #set param description
                            if "description" in process["inputs"][param].keys():
                                process_input.set("help", process["inputs"][param]["description"])
                            else:
                                if "Description" in process["inputs"][param].keys():
                                    process_input.set("help", process["inputs"][param]["Description"])
                                else:
                                    process_input.set("help", "")

                            #Retrive simple or extented schema
                            if("extended-schema" in process["inputs"][param].keys()):
                                schema = process["inputs"][param]["extended-schema"]
                            else: 
                                schema = process["inputs"][param]["schema"]
                            
                            #If multiple schemas are possible
                            if ("oneOf" in schema.keys() and len(process["inputs"][param]["schema"]["oneOf"]) > 0): 
                                #Use the first one 
                                print(process["inputs"][param]["schema"]["oneOf"])
                                print(process["inputs"][param])
                                schema = process["inputs"][param]["schema"]["oneOf"][0]
                            
                            #Set param type
                            if 'type' in schema.keys(): #simple schema
                                if schema["type"] in typeMapping.keys():
                                    process_input.set("type", typeMapping[schema["type"]])
                                    if "format" in schema.keys():
                                        if schema["format"] == "binary":
                                            process_input.set("type", "data")
                                            process_input.set("format", "txt")
                                    if "contentMediaType" in schema.keys():
                                        if schema["contentMediaType"] in mediaTypes:
                                            process_input.set("type", "data")
                                            process_input.set("format", "txt")
                                    if schema["type"] == "boolean":
                                        process_input.set("truevalue", "True") # Galaxy uses this for bools
                                        process_input.set("falsevalue", "False")
                                    if "enum" in schema: #create dropdown if enum exists
                                        enums = distinct_subarray(schema["enum"])
                                        process_input.set("type", "select")
                                        for enum in enums:
                                            option = ET.Element("option")
                                            option.set("value", enum)
                                            option.text = enum
                                            process_input.append(option)
                                    if schema["type"] == "array":
                                        if 'items' in schema.keys():
                                            if 'type' in schema["items"].keys():
                                                process_input.set("name", param +  "_Array_" + typeMapping[schema["items"]["type"]])
                                                process_input.set("type", "text")
                                                process_input.set("help", "Please provide comma-seperated values of type " + typeMapping[schema["items"]["type"]] + " here.")
                                            elif 'oneOf' in schema["items"].keys():
                                                for i in schema["items"]["oneOf"]:
                                                    if 'type' in i.keys():
                                                        if i["type"] == "object":
                                                            process_input.set("type", "data")
                                                            process_input.set("format", "txt")
                            when_process.append(process_input)
                        
                        #Add inputs to command information for process
                        processCommand["inputs"] = inputCommand
                        
                        outputFormatCommands = []

                        for output in process["outputs"]:
                            outputName = output
                            processOutput = process["outputs"][output]
                            if "extended-schema" in processOutput:
                                process_output = ET.Element("param")
                                process_output.set("type", "select")
                                process_output.set("name", outputName.replace(".", "_") + "_outformat") #_out needed to avoid duplicates
                                process_output.set("label", outputName + "_outformat")
                                process_output.set("help", "Output format")
                                outputFormatCommands.append(outputName + "_outformat")
                                enums = distinct_subarray(processOutput["extended-schema"]["oneOf"][0]["allOf"][1]["properties"]["type"]["enum"])
                                for enum in enums:
                                    output_option = ET.Element("option")
                                    output_option.set("value", enum)
                                    output_option.text=enum
                                    process_output.append(output_option)
                                when_process.append(process_output)
                        processCommand["outputs"] = outputFormatCommands
                        commands.append(processCommand)
                        when_list_processes.append(when_process)

        conditional_process.append(select_process)
        for when_process in when_list_processes:
            conditional_process.append(when_process)    

    #add command
    commandText = "<![CDATA["
    commandText += "\n\tRscript '$__tool_directory__/generic.R'\n"
    commandText += "\t\t--server '" + server + "'\n"
    commandText += "\t\t--outputData '$output_data'"
    commandText += "\n]]>"
    command.text = commandText
    tool.append(command)

    #add configfiles
    configfiles = ET.Element("configfiles")
    configfiles_inputs = ET.Element("inputs")
    configfiles_inputs.set("name", "inputs")
    configfiles_inputs.set("filename", "inputs.json")
    configfiles_inputs.set("data_style", "paths")
    configfiles.append(configfiles_inputs)
    tool.append(configfiles)

    #add inputs 
    inputs.append(conditional_process)
    tool.append(inputs)

    #add outputs
    outputs = ET.Element("outputs")
    dataOutput = ET.Element("data")
    dataOutput.set("name", "output_data")
    dataOutput.set("format", "txt")
    dataOutput.set("label", "$select_process")
    outputs.append(dataOutput)
    tool.append(outputs)

    #add tests
    tests = ET.Element("expand")
    tests.set("macro", "tests")
    tool.append(tests)

    tool.append(help)

    #add help
    #help = ET.Element("expand")
    #help.set("macro", "help")
    #tool.append(help)

    #add citation
    citations = ET.Element("expand")
    citations.set("macro", "citations")
    tool.append(citations)

    #Export .xml file
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
    