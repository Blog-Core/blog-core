# blog-core admin API

## GET api/collection/Name

Retrieves all collection `Name` entries. Gives the subset of properties.
Only those properties are retrieved that are listed in the `list`
property of the collection type.

    {
        "status": "success",
        "data": [
            {
                "$id":"d2f1b828-8063-446c-a1b7-4a151911fd27",
                "name":"title",
                "value":"Untitled blog"
            }
        ]
    }

## GET /api/types

Retrieves all currently registered document types.
Example output:

    {
        "status": "success",
        "data": [
            {
                "name":"config",
                "def": {
                    "description":"Configuration options.",
                    "detail": ["name", "value" ],
                    "edit": ["name", "value" ],
                    "list": ["name", "value" ],
                    "properties": {
                        "name": {"type":"line"},
                        "value": {"type":"line"}
                    },
                    "title":"name"
                }
            }
        ]
    }

Accessing with curl:

    curl --header 'X-Key: b4184b55-0af1-451a-aed7-c056263ee3d3' \
        -X GET http://localhost:18008/api/collection/config
