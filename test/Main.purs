module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Person.InMemoryInterpreters (createInMemoryPersonRepo)
import Test.Person.Repository (personRepoTests)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  personRepoTests createInMemoryPersonRepo
