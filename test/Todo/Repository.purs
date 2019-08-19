module Test.Todo.Repository where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (length)
import Effect (Effect)
import Effect.Class (liftEffect)
import Id (genId, toString)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Todo.Repository (Description(..), TodoRepo)

-- TODO: this shouldn't hard coded to Effect as the m to TodoRepo
-- TODO: should this take an effect of a todoRepo? can't it just be given the repo?
todoRepoTests :: Effect (TodoRepo Effect) -> Spec Unit
todoRepoTests getRepo = 
  describe "TodoRepository" do
    describe "create" do
      it "returns new todo with generated id" do
        repo <- liftEffect getRepo
        t <- liftEffect (repo.create  (Description("say hi")))
        t.description `shouldEqual` Description("say hi")
        toString t.id `shouldSatisfy` (length >>> (_ > 0))

    describe "get" do
      it "returns nothing when given id does not exist" do
        repo <- liftEffect getRepo
        id <- liftEffect genId
        todo <- liftEffect $ repo.get id
        todo `shouldEqual` Nothing
      
      it "returns todo when given id exists" do
        repo <- liftEffect getRepo
        t <- liftEffect (repo.create (Description("say hi")))
        todo <- liftEffect (repo.get t.id)
        todo `shouldEqual` Just(t)

    describe "update" do
      it "returns nothing when given id does not exist" do
        repo <- liftEffect getRepo
        id <- liftEffect genId
        todo <- liftEffect $ repo.update id \pp -> { id: pp.id, description: Description("Say hello") }
        todo `shouldEqual` Nothing
      
      it "returns updated todo when given id exists" do
        repo <- liftEffect getRepo
        p <- liftEffect (repo.create (Description("say hi")))
        _ <- liftEffect $ repo.update p.id \pp -> { id: pp.id, description: Description("Say hello") }
        todo <- liftEffect (repo.get p.id)
        todo `shouldEqual` Just({ description: Description("Say hello"), id: p.id })
