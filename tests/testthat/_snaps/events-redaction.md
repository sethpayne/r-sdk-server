# redacted event JSON is stable

    Code
      cat(jsonlite::toJSON(buf$drain(), auto_unbox = TRUE, pretty = TRUE))
    Output
      [
        {
          "kind": "identify",
          "creationDate": 1000,
          "context": {
            "key": "u",
            "kind": "user",
            "name": "Ada",
            "address": {
              "city": "c"
            },
            "_meta": {
              "redactedAttributes": [
                "email",
                "/address/street"
              ]
            }
          }
        },
        {
          "kind": "custom",
          "key": "evt",
          "creationDate": 1001,
          "context": {
            "key": "u",
            "kind": "user",
            "name": "Ada",
            "address": {
              "city": "c"
            },
            "_meta": {
              "redactedAttributes": [
                "email",
                "/address/street"
              ]
            }
          },
          "data": {
            "n": 1
          },
          "metricValue": 2.5
        }
      ]

