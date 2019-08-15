module Test.Person.Repository where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (length)
import Effect (Effect)
import Effect.Class (liftEffect)
import Id (genId, toString)
import Person.Repository (Name(..), PersonRepo)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldEqual, shouldSatisfy)

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

    describe "getByName" do
      it "returns empty array when given name does not exist" do
        repo <- liftEffect getRepo
        people <- liftEffect $ repo.getByName (Name("matt"))
        people `shouldEqual` []
      
      it "returns all people with given name" do
        repo <- liftEffect getRepo
        p1 <- liftEffect (repo.create { name: Name("matt"), id: unit})
        _ <- liftEffect (repo.create { name: Name("tom"), id: unit})
        p2 <- liftEffect (repo.create { name: Name("matt"), id: unit})
        people <- liftEffect $ repo.getByName (Name("matt"))
        people `shouldContain` p1
        people `shouldContain` p2

    describe "update" do
      it "returns nothing when given id does not exist" do
        repo <- liftEffect getRepo
        id <- liftEffect genId
        person <- liftEffect $ repo.update id \pp -> { id: pp.id, name: Name("Matt") }
        person `shouldEqual` Nothing
      
      it "returns updated person when given id exists" do
        repo <- liftEffect getRepo
        p <- liftEffect (repo.create { name: Name("matt"), id: unit})
        _ <- liftEffect $ repo.update p.id \pp -> { id: pp.id, name: Name("Matt") }
        person <- liftEffect (repo.get p.id)
        person `shouldEqual` Just({ name: Name("Matt"), id: p.id })
