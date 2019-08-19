module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Todo.Repository (todoRepoTests)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Todo.InMemoryInterpreters (createInMemoryTodoRepo)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  todoRepoTests createInMemoryTodoRepo
