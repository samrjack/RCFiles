
module {
  "name": "ddb",
  "description": "Convert to and from the json format used by dynamoDB."
};

# For example usage, see https://github.com/stanch/jq-modules/tree/master

# This is pulled from https://stackoverflow.com/questions/28593471/how-to-simplify-aws-dynamodb-query-json-output-from-the-command-line
def from_ddb:
  # DynamoDB string type
  (objects | .S)

  # DynamoDB binary type
  // (objects | .B)

  # DynamoDB number type
  // (objects | .N | strings | tonumber)

  # DynamoDB boolean type
  // (objects | .BOOL)

  # DynamoDB map type, recursion on each item
  // (objects | .M | objects | with_entries(.value |= from_ddb))

  # DynamoDB list type, recursion on each item
  // (objects | .L | arrays | map(from_ddb))

  # DynamoDB typed list type SS, string set
  // (objects | .SS | arrays | map(from_ddb))

  # DynamoDB typed list type NS, number set
  // (objects | .NS | arrays | map(tonumber))

  # DynamoDB typed list type BS, binary set
  // (objects | .BS | arrays | map(from_ddb))

  # managing others DynamoDB output entries: "Count", "Items", "ScannedCount" and "ConsumedCapcity"
  // (objects | with_entries(.value |= from_ddb))
  // (arrays | map(from_ddb))

  # leaves values
  // .
  ;

def to_ddb:
  
  ## Sets - ignoring them since they're hard to distinguish from lists and basic lists are
  #         used more often in my case

  # DynamoDB typed list type BS, binary set
  # // (objects | .BS | arrays | map(from_ddb))

  # DynamoDB typed list type NS, number set
  # // (objects | .NS | arrays | map(tonumber))

  # DynamoDB typed list type SS, string set
  # // (objects | .SS | arrays | map(from_ddb))

  # DynamoDB list type, recursion on each item
  (arrays | {"L": map(to_ddb)}) 

  # DynamoDB map type, recursion on each item
  # DynamoDB boolean type (objects | {"M": with_entries(.value |= to_ddb)})
  // (booleans | {"BOOL": .})

  # DynamoDB number type
  // (numbers | {"N": tostring})

  # DynamoDB binary type; commented out until I see the conversion and usage results in jq
  # Update: these are going to be nearly indistinguishable from normal strings so for now I'll leave these out. If I can figure out how to know for sure that something is a binary, then I can add it back in.
  # // (ddb_binarys | {"B": .}) # need to define ddb_binary filter function

  # DynamoDB string type
  // (strings | {"S": .})

  # leaves values - if it got this far there may be something weird in the JSON but not much I can do about it.
  // .
  ;

