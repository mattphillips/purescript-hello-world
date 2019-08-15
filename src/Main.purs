module Main (main) where

import Effect (Effect)
import Effect.Console (logShow)
import Person.Repository (Name(..))
import Prelude (Unit, bind, discard, unit)
import Person.InMemoryInterpreters (createInMemoryPersonRepo)

main :: Effect Unit
main = do
  repo <- createInMemoryPersonRepo
  p <- repo.create { id: unit, name: Name("Matt") }
  logShow p
  mp <- repo.get p.id
  logShow mp
  _ <- repo.update p.id \pp -> { id: pp.id, name: Name("Matthew") }
  ps <- repo.getByName (Name("Matthew"))
  logShow ps
