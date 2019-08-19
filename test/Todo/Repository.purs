module Test.Todo.Repository where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (length)
import Effect (Effect)
import Effect.Class (liftEffect)
import Id (genId, toString)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldEqual, shouldNotContain, shouldSatisfy)
import Todo.Repository (Description(..), Status(..), Todo, TodoRepo)

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
        t.status `shouldEqual` Active
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

    describe "getByStatus" do
      it "returns empty array when no todos match given status" do
        repo <- liftEffect getRepo
        activeTodos <- liftEffect $ repo.getByStatus Active
        completedTodos <- liftEffect $ repo.getByStatus Completed
        activeTodos `shouldEqual` []
        completedTodos `shouldEqual` []

      it "returns all active todos" do
        repo <- liftEffect getRepo
        t1 <- liftEffect (repo.create (Description("one")))
        t2 <- liftEffect (repo.create (Description("two")))
        t3 <- liftEffect (repo.create (Description("three")))
        activeTodos <- liftEffect $ repo.getByStatus Active
        activeTodos `shouldContain` t1
        activeTodos `shouldContain` t2
        activeTodos `shouldContain` t3

      it "returns all completed todos" do
        repo <- liftEffect getRepo
        t1 <- liftEffect (repo.create (Description("one")))
        t2 <- liftEffect (repo.create (Description("two")))
        t3 <- liftEffect (repo.create (Description("three")))
        _ <- liftEffect $ repo.update t1.id setCompleted
        _ <- liftEffect $ repo.update t3.id setCompleted
        completedTodos <- liftEffect $ repo.getByStatus Completed
        completedTodos `shouldContain` {id: t1.id, description: t1.description, status: Completed}
        completedTodos `shouldNotContain` t2
        completedTodos `shouldContain` {id: t3.id, description: t3.description, status: Completed}

    describe "update" do
      it "returns nothing when given id does not exist" do
        repo <- liftEffect getRepo
        id <- liftEffect genId
        todo <- liftEffect $ repo.update id \pp -> { id: pp.id, description: Description("Say hello"), status: pp.status }
        todo `shouldEqual` Nothing
      
      it "returns updated todo when given id exists" do
        repo <- liftEffect getRepo
        p <- liftEffect (repo.create (Description("say hi")))
        _ <- liftEffect $ repo.update p.id \pp -> { id: pp.id, description: Description("Say hello"), status: Completed }
        todo <- liftEffect (repo.get p.id)
        todo `shouldEqual` Just({ description: Description("Say hello"), id: p.id, status: Completed  })

    describe "delete" do
      it "removes todo from store" do
        repo <- liftEffect getRepo
        t <- liftEffect (repo.create  (Description("say hi")))
        todo <- liftEffect (repo.get t.id)
        todo `shouldEqual` Just(t)
        _ <- liftEffect (repo.delete t.id)
        deletedTodo <- liftEffect (repo.get t.id)
        deletedTodo `shouldEqual` Nothing

setCompleted :: ∀ a. Todo a -> Todo a
setCompleted t = { id: t.id, description: t.description, status: Completed }