# Integrating OGC API Processes into Galaxy 

## Background

The Galaxy platform is an open-source, web-based environment for analyzing scientific data, especially in life-science fields but also more and more in the geosciences. It lets researchers run complex computational workflows through a user-friendly interface without needing to write code. Galaxy also emphasizes reproducibility and collaboration by making analyses, tools, and datasets easy to share and rerun.

OGC API – Processes is a standard from the Open Geospatial Consortium for exposing computational processes (like models, transformations, or analyses) through a web API. It allows clients to discover available processes, inspect their inputs and outputs, and execute them using simple HTTP requests. The goal is to make geospatial processing interoperable, reusable, and easy to integrate into web and cloud-based workflows.

Both Galaxy tools and OGC API – Processes follow an input–processing–output logic. In each case, users provide inputs, the system runs a computational process or tool, and the results are returned as outputs in the form of data, figures or similar. This shared logic makes both platforms modular, reproducible, and easy to integrate into larger automated workflows. So how can we integrate both approches? 

## Concept

We developed a wrapper tool that bridges OGC API – Processes and the Galaxy platform by translating remote geospatial processes into native Galaxy tools. The tool automatically generates a Galaxy XML definition from OGC API process descriptions, using the declared inputs and outputs to build the Galaxy user interface. Once wrapped, these remote processes behave like standard Galaxy tools, meaning they can be chained together into Galaxy workflows, while execution happens on the OGC API server. We're using lightweight .txt files containing links to input datasets, which are sent to the remote processes instead of transferring the actual data avoiding unnecessary data transfer. Also the results of a process are returned to Galaxy as links which can then be sent to the next process in the chain. 

## How to run

Step 1: `git clone https://github.com/AquaINFRA/OGC-API-Process2Galaxy.git` 

Step 2: `cd OGC-API-Process2Galaxy`

Step 3: `npm install`

Step 4: Open the configuration file and provide the remote server URL and relevant metadata. You can also specify which OGC API processes should be included or excluded; using an asterisk (*) includes all available processes. It is important to define the parameters that represent input data (input_data_params), as these are mapped to Galaxy input fields and allow users to supply .txt files containing URLs to the input datasets.

Step 5: Run `python3 OGCProcess2Galaxy.py config.json`
This generates a Galaxy XML file that defines the user interface for the wrapped processes. Together with the accompanying R script, this XML file forms a complete Galaxy tool that can be installed and executed within the Galaxy platform (see https://github.com/galaxyecology/tools-ecology/tree/master/tools/aquainfra_ogc_api_processes).

## Integration into the Galaxy platform

Step 1: Test the tool locally in a Galaxy environment, for example using Planemo, to ensure it works as expected.

Step 2: Integrate the tool into the galaxyecology/tools-ecology repository: https://github.com/galaxyecology/tools-ecology

Step 3: Create a pull request and verify that all automated tests pass. Galaxy developers will review the PR and merge it if approved. Once merged, the tool will become available on the Galaxy platform following the next regular deployment cycle (usually on Saturdays).

## Limitations

The processes are not executed on the Galaxy platform itself but on a remote server. Therefore, an external service (e.g., a pygeoapi instance) hosting the OGC API – Processes is required. This server must provide sufficient computational resources, including CPU and storage; otherwise, performance may degrade or the service may fail when multiple users execute processes concurrently. Storage capacity is another important consideration, as all results (e.g., datasets, figures, and intermediate outputs) are stored on the remote server.

Since the processes run on a remote server, it is possible for them to be modified or updated independently of Galaxy. This means that even if a workflow in Galaxy is rerun with the same inputs, the underlying behavior or results of a process may differ without the platform detecting the change. Users should be aware that reproducibility is therefore dependent not only on Galaxy but also on the stability and versioning of the remote OGC API – Processes. Developers can help maintain trust and reproducibility by providing access to the GitHub or GitLab repositories hosting the OGC API – Processes. This allows users to verify whether any updates or changes have been made to the processes, helping to resolve doubts about workflow consistency. 

## Performance

When wrapping a large number of processes, the generated Galaxy XML file can become very large. While the tool performed well with approximately 70 processes, usability degraded significantly when scaling to around 700 processes, with the Galaxy interface becoming slow and difficult to use. In practice, the performance limit likely lies between these two extremes. For servers exposing a large number of processes, we recommend splitting them into multiple Galaxy tools to maintain responsiveness and usability.