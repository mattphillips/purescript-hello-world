module Test.Todo.Repository where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (length)
import Effect (Effect)
import Effect.Class (liftEffect)
import Id (genId, toString)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldEqual, shouldNotContain, shouldSatisfy)
import Todo (Description(..), IsComplete(..), Todo(..), getId, updateTodo)
import Todo.Repository (TodoRepo)

-- TODO: this shouldn't hard coded to Effect as the m to TodoRepo
-- TODO: should this take an effect of a todoRepo? can't it just be given the repo?
todoRepoTests :: Effect (TodoRepo Effect) -> Spec Unit
todoRepoTests getRepo = 
  describe "TodoRepository" do
    describe "create" do
      it "returns new todo with generated id" do
        repo <- liftEffect getRepo
        (Todo t) <- liftEffect (repo.create (Description("say hi")))
        t.description `shouldEqual` Description("say hi")
        t.isComplete `shouldEqual` (IsComplete false)
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
        todo <- liftEffect (repo.get $ getId t)
        todo `shouldEqual` Just(t)

    describe "getAll" do
      it "returns empty array when no todos exist" do
        repo <- liftEffect getRepo
        todos <- liftEffect repo.getAll
        todos `shouldEqual` []

      it "returns all todos in array" do
        repo <- liftEffect getRepo
        t1 <- liftEffect (repo.create (Description("one")))
        t2 <- liftEffect (repo.create (Description("two")))
        t3 <- liftEffect (repo.create (Description("three")))
        todos <- liftEffect repo.getAll
        todos `shouldContain` t1
        todos `shouldContain` t2
        todos `shouldContain` t3

    describe "filter" do
      it "returns all active todos" do
        repo <- liftEffect getRepo
        t1 <- liftEffect (repo.create (Description("one")))
        t2 <- liftEffect (repo.create (Description("two")))
        t3 <- liftEffect (repo.create (Description("three")))
        activeTodos <- liftEffect $ repo.filter {isComplete: (IsComplete false)}
        activeTodos `shouldContain` t1
        activeTodos `shouldContain` t2
        activeTodos `shouldContain` t3

      it "returns all completed todos" do
        repo <- liftEffect getRepo
        (Todo t1) <- liftEffect (repo.create (Description("one")))
        t2 <- liftEffect (repo.create (Description("two")))
        (Todo t3) <- liftEffect (repo.create (Description("three")))
        _ <- liftEffect $ repo.update t1.id setCompleted
        _ <- liftEffect $ repo.update t3.id setCompleted
        completedTodos <- liftEffect $ repo.filter {isComplete: (IsComplete true)}
        completedTodos `shouldContain` Todo {id: t1.id, description: t1.description, isComplete: (IsComplete true)}
        completedTodos `shouldNotContain` t2
        completedTodos `shouldContain` Todo {id: t3.id, description: t3.description, isComplete: (IsComplete true)}

    describe "update" do
      it "returns nothing when given id does not exist" do
        repo <- liftEffect getRepo
        id <- liftEffect genId
        todo <- liftEffect $ repo.update id (updateTodo \t -> t { description = Description("Say hello") })
        todo `shouldEqual` Nothing
      
      it "returns updated todo when given id exists" do
        repo <- liftEffect getRepo
        t <- liftEffect (repo.create (Description("say hi")))
        let id = getId t
        _ <- liftEffect $ repo.update id (updateTodo \tt -> tt { description = Description("Say hello"), isComplete = (IsComplete true)})
        todo <- liftEffect (repo.get id)
        todo `shouldEqual` Just(Todo {description: Description("Say hello"), id: id, isComplete: (IsComplete true)})

    describe "delete" do
      it "removes todo from store" do
        repo <- liftEffect getRepo
        t <- liftEffect (repo.create (Description("say hi")))
        let id = getId t
        todo <- liftEffect (repo.get id)
        todo `shouldEqual` Just(t)
        _ <- liftEffect (repo.delete id)
        deletedTodo <- liftEffect (repo.get id)
        deletedTodo `shouldEqual` Nothing

setCompleted :: ∀ a. Todo a -> Todo a
setCompleted = updateTodo (\t -> t { isComplete = (IsComplete true) })
