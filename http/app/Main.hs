{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString as B
import Data.ByteString.Char8 as BC
import Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple

myToken :: BC.ByteString
myToken = "YuPvFBZUkUZPabJJRQbjEEWOGvqyzAdv"

noaaHost :: BC.ByteString
noaaHost = "www.ncdc.noaa.gov"

apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest host path token =
  setRequestHost host $
    setRequestHeader "token" [token] $
      setRequestPath path $
        setRequestSecure True $
          setRequestPort 443 $
            defaultRequest

request :: Request
request = buildRequest noaaHost apiPath myToken

main :: IO ()
main = do
  response <- httpLBS request
  let status = getResponseStatusCode response
  if status == 200
    then do
      print "Saving response to file..."
      let body = getResponseBody response
      L.writeFile "data.json" body
    else print "Request failed"
