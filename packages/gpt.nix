{
  curl,
  writers,
  jq,
  apiKeyCommand ? "pass api-keys/openai.com",
  model ? "gpt-3.5-turbo",
}:
writers.writeDashBin "gpt" ''
  json=$(jq --slurp --raw-input '{model:"${model}", messages: [{role: "user", content: .}]}')
  ${curl}/bin/curl -sSL https://api.openai.com/v1/chat/completions \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer $(pass api-keys/openai.com)" \
    -d "$json" \
    | ${jq}/bin/jq -r '.choices[] | .message.content'
''
