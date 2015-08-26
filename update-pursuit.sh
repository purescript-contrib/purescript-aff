./node_modules/.bin/psc-publish > .pursuit.json
curl -X POST \
  http://pursuit.purescript.org/packages \
  -d @.pursuit.json \
  -H 'Accept: application/json' \
  -H "Authorization: token $PURSUIT_TOKEN" \
  -v
