module Main (main) where

import Effect (Effect)
import Effect.Console (logShow)
import Prelude (Unit, bind, discard)
import Todo.InMemoryInterpreters (createInMemoryTodoRepo)
import Todo.Repository (Description(..))

main :: Effect Unit
main = do
  repo <- createInMemoryTodoRepo
  p <- repo.create (Description("Matt"))
  logShow p
  mp <- repo.get p.id
  logShow mp
  _ <- repo.update p.id \pp -> { id: pp.id, description: Description("Matthew") }
  ps <- repo.get p.id
  logShow ps
