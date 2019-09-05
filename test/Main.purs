module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.SimpleApp as SimpleApp
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Todo.Repository (todoRepoTests)
import Todo.InMemoryInterpreters (createInMemoryTodoRepo)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  todoRepoTests createInMemoryTodoRepo
  SimpleApp.tests
