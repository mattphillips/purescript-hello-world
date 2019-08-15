module Test.Person.Repository where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (length)
import Effect (Effect)
import Effect.Class (liftEffect)
import Id (genId, toString)
import Person.Repository (Name(..), PersonRepo)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

-- TODO: this shouldn't hard coded to Effect as the m to PersonRepo
-- TODO: should this take an effect of a personRepo? can't it just be given the repo?
personRepoTests :: Effect (PersonRepo Effect) -> Spec Unit
personRepoTests getRepo = 
  describe "PersonRepository" do
    describe "create" do
      it "returns new person with generated id" do
        repo <- liftEffect getRepo
        {id, name: (Name name)} <- liftEffect (repo.create { name: Name("matt"), id: unit})
        name `shouldEqual` "matt"
        toString id `shouldSatisfy` (length >>> (_ > 0))

    describe "get" do
      it "returns nothing when given id does not exist" do
        repo <- liftEffect getRepo
        id <- liftEffect genId
        person <- liftEffect $ repo.get id
        person `shouldEqual` Nothing
      
      it "returns person when given id exists" do
        repo <- liftEffect getRepo
        p <- liftEffect (repo.create { name: Name("matt"), id: unit})
        person <- liftEffect (repo.get p.id)
        person `shouldEqual` Just({ name: Name("matt"), id: p.id })
