module Test.SimpleApp (tests) where

import Prelude

import Effect.Class (liftEffect)
import Node.Express.App (App, get)
import Node.Express.Response (send)
import Node.Express.Types (Method(..), Request, Response)
import Test.Express (getBody, getStatus, mkRequest, mkResponse, mkTestApplication, sendRequest, mountApp)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

simpleApp :: App
simpleApp = do
  get "/" (send "hi")

req :: Request
req = mkRequest { method: show GET, url: "/" }

res :: Response
res = mkResponse unit

tests :: Spec Unit
tests = 
  describe "SimpleApp" do
    describe "GET /" do
      it "returns hi" do
        app <- liftEffect $ mountApp simpleApp <$> mkTestApplication
        _ <- liftEffect $ sendRequest app req res
        getStatus res `shouldEqual` 200
        getBody res `shouldEqual` "hi"