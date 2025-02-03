import json
import xml.etree.ElementTree as ET
import urllib.request 
import warnings
import sys
import xml.dom.minidom as md

#execute: python3 OGCProcess2Galaxy.py aqua.json

# OGC Process Description types to Galaxy parameter types
TYPE_MAPPING = {
    "array": "text",
    "boolean": "boolean",
    "integer": "integer",
    "number": "float",
    "object": "data",
    "string": "text"
}

# Recognized media types
MEDIA_TYPES = {
    "image/tiff", "image/jpeg", "image/png",
    "text/xml", "text/plain", "application/octet-stream"
}

# Conformance classes
CONF_CLASSES = {
    "http://www.opengis.net/spec/ogcapi-processes-1/1.0/conf/core",
    "http://www.opengis.net/spec/ogcapi-processes-1/1.0/conf/ogc-process-description",
    "http://www.opengis.net/spec/ogcapi-processes-1/1.0/conf/json"
}

def distinct_subarray(arr):
    """ Return distinct elements while preserving order. """
    seen = set()
    return [item for item in arr if not (item in seen or seen.add(item))]

def create_tool_element(config):
    """ Create the root 'tool' XML element. """
    tool = ET.Element("tool", id=config["id"], name=config["title"], version=config["version"], profile=config.get("profile", ""))
    
    ET.SubElement(tool, "description").text = config["description"]
    ET.SubElement(tool, "help").text = config["help"]

    macros = ET.SubElement(tool, "macros")
    ET.SubElement(macros, "import").text = "macros.xml"
    
    creator = ET.SubElement(tool, "creator")
    organization = ET.SubElement(creator, "organization", name=config["creator"]["name"], url=config["creator"]["url"])

    ET.SubElement(tool, "expand", macro="requirements")

    return tool

def save_xml(tool, filename="generic.xml"):
    """ Save the generated XML file with proper formatting. """
    xml_string = ET.tostring(tool, encoding="unicode")
    xml_string = xml_string.replace("&lt;", "<")
    xml_string = xml_string.replace("&gt;", ">")
    formatted_xml = md.parseString(xml_string).toprettyxml()

    with open(filename, "w", encoding="utf-8") as f:
        f.write(formatted_xml)
    
    print(f"--> {filename} exported")

def OGCAPIProcesses2Galaxy(config_file):
    """ Main function to convert OGC API processes to Galaxy tools. """
    with open(config_file, encoding="utf-8") as f:
        config = json.load(f)

    tool = create_tool_element(config)

    #add command
    command = ET.Element("command")
    command.set("detect_errors", "exit_code")
    commands = []

    #add inputs
    inputs = ET.Element("inputs")

    index_i = 0
    for api in config["servers"]: 
        #get server 
        #server = api["server_url"]

        index_i += 1

        #check conformance
        with urllib.request.urlopen(api["server_url"] + "conformance") as conformanceURL:
            #Retrieve conformance data
            conformanceData = json.load(conformanceURL)
            
            #Set conformance to True
            conformance = True

            #Iterate over conformance classes
            for confClass in CONF_CLASSES:
                #Ceck if conformance class is implemented
                if confClass not in conformanceData["conformsTo"]:
                    #Create warnning and set conformance to False if certain conformance class is not implemented
                    msg = "Specified API available via:" + api["server_url"] + " does not conform to " + confClass + ". This may lead to issues when converting its processes to Galaxy tools."
                    warnings.warn(msg, Warning)
                    conformance = False               

            #Set help text for tool
            if conformance:
                help.text = config["help"]
            else:
                #If API might not be complient with OGC processes API add notification to help text
                help.text = config["help"] + " Take note that the service provided by this does not implement all nesseracy OGC API Processes conformance classes and might thus not behave as expected!"

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
                            processElement.text = process["id"] + ": " + process["title"].strip()
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
                                process["inputs"][param]["description"] = process["inputs"][param]["description"].replace('"', "'")
                                process_input.set("help", process["inputs"][param]["description"])
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
                                #print(process["inputs"][param]["schema"]["oneOf"])
                                #print(process["inputs"][param])
                                schema = process["inputs"][param]["schema"]["oneOf"][0]
                            
                            #Set param type
                            if 'type' in schema.keys(): #simple schema
                                if schema["type"] in TYPE_MAPPING.keys():
                                    process_input.set("type", TYPE_MAPPING[schema["type"]])
                                    for input in config["input_data_params"]:
                                        if process["id"] == input["process"] and param == input["param_name"]:
                                            process_input.set("type", "data")
                                            process_input.set("format", "txt")
                                            process_input.set("help", process["inputs"][param]["description"] + " (URL must be stored in a .txt file)")
                                    if "format" in schema.keys():
                                        print(schema["format"])
                                        if schema["format"] == "binary":
                                            process_input.set("type", "data")
                                            process_input.set("format", "txt")
                                    if "contentMediaType" in schema.keys():
                                        if schema["contentMediaType"] in MEDIA_TYPES:
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
                                    if "minimum" in schema:
                                        min = schema["minimum"]
                                        process_input.set("min", str(min))
                                    if "maximum" in schema:
                                        max = schema["maximum"]
                                        process_input.set("max", str(max))
                                    if schema["type"] == "array":
                                        if 'items' in schema.keys():
                                            if 'type' in schema["items"].keys():
                                                process_input.set("name", param +  "_Array_" + TYPE_MAPPING[schema["items"]["type"]])
                                                process_input.set("type", "text")
                                                process_input.set("help", "Please provide comma-seperated values of type " + TYPE_MAPPING[schema["items"]["type"]] + " here.")
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
    command.text = f"<![CDATA[\n\tRscript '$__tool_directory__/{config['script']}'\n\t\t--outputData '$output_data'\n    ]]>"
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

    ET.SubElement(tool, "outputs").append(ET.Element("data", name="output_data", format="txt", label="$select_process"))
    ET.SubElement(tool, "expand", macro="tests")
    ET.SubElement(tool, "expand", macro="citations")

    save_xml(tool)

if __name__ == "__main__":
    if len(sys.argv) > 1:
        OGCAPIProcesses2Galaxy(sys.argv[1])
    else:
        print("Usage: python script.py <config_file>")
    