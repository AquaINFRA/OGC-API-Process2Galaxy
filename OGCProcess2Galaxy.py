import json
import xml.etree.ElementTree as ET
import urllib.request 
import warnings
import sys
import xml.dom.minidom as md

# OGC Process Description types to Galaxy parameter types
TYPE_MAPPING = {
    "array": "text",
    "boolean": "boolean",
    "integer": "integer",
    "number": "float",
    "object": "text",
    "string": "text"
}

# Recognized media types
MEDIA_TYPES = {"image/tiff", "image/jpeg", "image/png", "text/xml", "text/plain", "application/octet-stream", "application/zip", "application/json", "application/csv"}

# Conformance classes
CONF_CLASSES = {
    "http://www.opengis.net/spec/ogcapi-processes-1/1.0/conf/core",
    "http://www.opengis.net/spec/ogcapi-processes-1/1.0/conf/ogc-process-description",
    "http://www.opengis.net/spec/ogcapi-processes-1/1.0/conf/json"
}


# Utility Functions
def distinct_subarray(arr):
    """Return distinct elements while preserving order."""
    seen = set()
    return [item for item in arr if not (item in seen or seen.add(item))]


def load_json(url):
    """Load JSON data from a URL with error handling."""
    try:
        with urllib.request.urlopen(url) as response:
            return json.load(response)
    except Exception as e:
        print(f"Error loading JSON from {url}: {e}")
        return None


def save_xml(tool, filename="generic.xml"):
    """Save the generated XML file with proper formatting."""
    xml_string = ET.tostring(tool, encoding="unicode")
    formatted_xml = md.parseString(xml_string).toprettyxml(indent="    ")
    formatted_xml = formatted_xml.replace("&lt;!", "<!")
    formatted_xml = formatted_xml.replace("]&gt;", "]>")

    formatted_xml = "\n".join(line for line in formatted_xml.splitlines() if not line.strip().startswith("<?xml"))

    with open(filename, "w", encoding="utf-8") as f:
        f.write(formatted_xml)
    
    print(f"--> {filename} exported")


def validate_conformance(server_url):
    """Check if the server conforms to required OGC standards."""
    conformance_data = load_json(f"{server_url}conformance")

    if not conformance_data or "conformsTo" not in conformance_data:
        warnings.warn(f"Could not verify conformance for {server_url}.", Warning)
        return False

    non_compliant = [cls for cls in CONF_CLASSES if cls not in conformance_data["conformsTo"]]
    for conf_class in non_compliant:
        warnings.warn(f"{server_url} does not conform to {conf_class}. This may cause issues.", Warning)

    return not non_compliant


# XML Element Builders
def create_tool_element(config):
    """Create the root 'tool' XML element."""
    tool = ET.Element("tool", id=config["id"], name=config["title"], version=config["version"], profile=config.get("profile", ""))
    
    ET.SubElement(tool, "description").text = config["description"]
    #ET.SubElement(tool, "help").text = config["help"]
    ET.SubElement(ET.SubElement(tool, "macros"), "import").text = "macros.xml"
    
    creator = ET.SubElement(tool, "creator")
    ET.SubElement(creator, "organization", name=config["creator"]["name"], url=config["creator"]["url"])

    ET.SubElement(tool, "expand", macro="requirements")
    return tool


def create_process_input(param_name, param_details, config, process_id):
    """Create input parameter element for a process."""
    process_input = ET.Element("param", name=param_name.replace(".", "_"))

    # Set label and help text
    process_input.set("label", param_details.get("title", param_name))
    # Check if optional
    optional = param_details.get("schema", {}).get("nullable", False) or param_details.get("minOccurs", 1) == 0
    process_input.set("optional", str(optional).lower())
    # Set default value
    if "default" in param_details.get("schema", {}):
        if (str(param_details["schema"]["type"])=="boolean"):
            process_input.set("checked", str(param_details["schema"]["default"]))
        else:
            process_input.set("value", str(param_details["schema"]["default"]))
    #if not optional and "default" not in param_details.get("schema", {}):
    #if not optional and param_details.get("default") in (None, ""):
    if not optional:
        if(str(param_details["schema"]["type"])=="boolean"):
            process_input.set("checked", "false")
        if str(param_details["schema"]["type"]) in ("string", "object", "array"):
            process_input.set("value", "")
            validator = ET.Element("validator")
            validator.set("type", "empty_field")
            validator.set("message", "You must provide a value.")
            enum_values = param_details.get("schema", {}).get("enum")
            if not enum_values:
                process_input.append(validator)
        if(str(param_details["schema"]["type"])=="integer") and "default" not in param_details.get("schema", {}):
            process_input.set("value", "0")
        if(str(param_details["schema"]["type"])=="number") and "default" not in param_details.get("schema", {}):
            process_input.set("value", "0.0")            
    description = param_details.get('description', '').replace('"', "'")
    process_input.set("help", f"{description}")

    # Determine schema
    schema = param_details.get("extended-schema", param_details.get("schema"))
    if "oneOf" in schema:
        schema = schema["oneOf"][0]

    # Set input type
    schema_type = schema.get("type")
    if schema_type in TYPE_MAPPING:
        process_input.set("type", TYPE_MAPPING[schema_type])

    # Handle input data parameters
    if any(process_id == inp["process"] and param_name == inp["param_name"] for inp in config["input_data_params"]):
        process_input.set("type", "data")
        process_input.set("format", "txt")
        description = param_details.get('description', '').replace('"', "'")
        process_input.set("help", f"{description} (URL must be stored in a .txt file)")
        process_input.attrib.pop("value", None)
        for child in list(process_input):
            if child.tag == "validator":
                process_input.remove(child)

    # Handle formats and enums
    handle_formats_and_enums(process_input, schema, schema_type, process_id, param_name, config)

    return process_input

def handle_formats_and_enums(process_input, schema, schema_type, process_id, param_name, config):
    """Handle media types, enums, booleans, and array-specific settings."""
    #if schema.get("format") == "binary" or schema.get("contentMediaType") in MEDIA_TYPES:
    #   process_input.set("type", "data")
    #    process_input.set("format", "txt")

    if schema_type == "boolean":
        process_input.set("truevalue", "True")
        process_input.set("falsevalue", "False")

    if "enum" in schema:
        process_input.set("type", "select")
        process_input.attrib.pop("value", None) 
        for enum_value in distinct_subarray(schema["enum"]):
            option = ET.Element("option", value=enum_value)
            option.text = enum_value
            process_input.append(option)

    if "minimum" in schema:
        process_input.set("min", str(schema["minimum"]))
    if "maximum" in schema:
        process_input.set("max", str(schema["maximum"]))

    if schema_type == "array":# and 'items' in schema:
        process_input.set("name", f"{process_input.attrib['name']}_array")
        #handle_array_inputs(process_input, schema["items"])
        #handle_array_inputs(process_input)
    
    #if schema_type == "object":# and 'items' in schema:
    #    if not any(process_id == inp["process"] and param_name == inp["param_name"] for inp in config["input_data_params"]):
    #        process_input.set("name", f"{process_input.attrib['name']}_object")


#def handle_array_inputs(process_input):#, items_schema):
#    """Handle array type inputs."""
#    process_input.set("name", f"{process_input.attrib['name']}_array_string")
    #if 'type' in items_schema:
    #    process_input.set("name", f"{process_input.attrib['name']}_Array_{TYPE_MAPPING[items_schema['type']]}")
    #    process_input.set("help", f"Please provide comma-separated values of type {TYPE_MAPPING[items_schema['type']]} here.")
    #elif 'oneOf' in items_schema:
    #    for item_option in items_schema["oneOf"]:
    #        if item_option.get('type') == "object":
    #            process_input.set("type", "data")
    #            process_input.set("format", "txt")


# Main Processing Function
def OGCAPIProcesses2Galaxy(config_file):
    """Main function to convert OGC API processes to Galaxy tools."""
    with open(config_file, encoding="utf-8") as f:
        config = json.load(f)

    tool = create_tool_element(config)
    command = ET.Element("command", detect_errors="exit_code")
    inputs = ET.Element("inputs")

    for server in config["servers"]:
        #if not validate_conformance(server["server_url"]):
        #    continue

        conditional_process = ET.Element("conditional", name="conditional_process")
        select_process = ET.Element("param", name="select_process", type="select", label="Select process")

        processes_data = load_json(f"{server['server_url']}processes{server['filter']}")
        if not processes_data:
            continue

        when_list_processes = process_server_processes(server, processes_data, select_process, config)
        conditional_process.append(select_process)
        for when_process in when_list_processes:
            conditional_process.append(when_process)

        inputs.append(conditional_process)

    command.text = f"<![CDATA[\n    Rscript '$__tool_directory__/{config['script']}'\n        --outputData '$output_data'\n    ]]>"
    tool.append(command)
    
    
    #add configfiles
    configfiles = ET.Element("configfiles")
    configfiles_inputs = ET.Element("inputs", name="inputs", filename="inputs.json", data_style="paths")
    configfiles.append(configfiles_inputs)
    tool.append(configfiles)

    tool.append(inputs)
    ET.SubElement(tool, "outputs").append(ET.Element("data", name="output_data", format="txt", label="$select_process"))
    ET.SubElement(tool, "expand", macro="tests")
    ET.SubElement(tool, "help").text = config["help"]
    ET.SubElement(tool, "expand", macro="citations")

    save_xml(tool)


def process_server_processes(server, processes_data, select_process, config):
    """Process each server's processes."""
    when_list_processes = []

    # Sort processes alphabetically by ID
    sorted_processes = sorted(
        processes_data["processes"],
        key=lambda p: p["id"]
    )

    for process in sorted_processes:
        if process["id"] in server.get("excluded_services", []):
            continue
        if not is_included_process(server, process):
            continue

        process_details = load_json(
            f"{server['server_url']}processes/{process['id']}"
        )
        if not process_details:
            continue

        add_process_option(select_process, process_details)

        when_process = ET.Element("when", value=process["id"])

        for param in process_details["inputs"]:
            process_input = create_process_input(
                param,
                process_details["inputs"][param],
                config,
                process["id"]
            )
            when_process.append(process_input)

        when_list_processes.append(when_process)

    return when_list_processes



def is_included_process(server, process):
    """Check if the process should be included based on the server config."""
    included_services = server.get("included_services", [])
    return process["id"] in included_services or ("*" in included_services and len(included_services) == 1)


def add_process_option(select_process, process):
    """Add a process option to the select element."""
    process_option = ET.Element("option", value=process["id"])
    process_option.text = f"{process['id']}: {process.get('title', '').strip()}" if "title" in process else process["id"]
    select_process.append(process_option)


# Entry Point
if __name__ == "__main__":
    if len(sys.argv) > 1:
        OGCAPIProcesses2Galaxy(sys.argv[1])
    else:
        print("Usage: python script.py <config_file>")
