module Test.Todo.Repository where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (length)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Id (genId, toString)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldEqual, shouldNotContain, shouldSatisfy)
import Todo (Description(..), IsComplete(..), Todo(..), getId, updateTodo)
import Todo.Repository (TodoRepo)

-- TODO: this shouldn't hard coded to Effect as the m to TodoRepo
-- TODO: should this take an effect of a todoRepo? can't it just be given the repo?
todoRepoTests :: Aff (TodoRepo Aff) -> Spec Unit
todoRepoTests getRepo = 
  describe "TodoRepository" do
    describe "create" do
      it "returns new todo with generated id" do
        repo <- getRepo
        (Todo t) <- (repo.create (Description("say hi")))
        t.description `shouldEqual` Description("say hi")
        t.isComplete `shouldEqual` (IsComplete false)
        toString t.id `shouldSatisfy` (length >>> (_ > 0))

    describe "get" do
      it "returns nothing when given id does not exist" do
        repo <- getRepo
        id <- liftEffect $ genId
        todo <- repo.get id
        todo `shouldEqual` Nothing
      
      it "returns todo when given id exists" do
        repo <- getRepo
        t <- (repo.create (Description("say hi")))
        todo <- (repo.get $ getId t)
        todo `shouldEqual` Just(t)

    describe "getAll" do
      it "returns empty array when no todos exist" do
        repo <- getRepo
        todos <- repo.getAll
        todos `shouldEqual` []

      it "returns all todos in array" do
        repo <- getRepo
        t1 <- (repo.create (Description("one")))
        t2 <- (repo.create (Description("two")))
        t3 <- (repo.create (Description("three")))
        todos <- repo.getAll
        todos `shouldContain` t1
        todos `shouldContain` t2
        todos `shouldContain` t3

    describe "filter" do
      it "returns all active todos" do
        repo <- getRepo
        t1 <- (repo.create (Description("one")))
        t2 <- (repo.create (Description("two")))
        t3 <- (repo.create (Description("three")))
        activeTodos <- repo.filter {isComplete: (IsComplete false)}
        activeTodos `shouldContain` t1
        activeTodos `shouldContain` t2
        activeTodos `shouldContain` t3

      it "returns all completed todos" do
        repo <- getRepo
        (Todo t1) <- (repo.create (Description("one")))
        t2 <- (repo.create (Description("two")))
        (Todo t3) <- (repo.create (Description("three")))
        _ <- repo.update t1.id setCompleted
        _ <- repo.update t3.id setCompleted
        completedTodos <- repo.filter {isComplete: (IsComplete true)}
        completedTodos `shouldContain` Todo {id: t1.id, description: t1.description, isComplete: (IsComplete true)}
        completedTodos `shouldNotContain` t2
        completedTodos `shouldContain` Todo {id: t3.id, description: t3.description, isComplete: (IsComplete true)}

    describe "update" do
      it "returns nothing when given id does not exist" do
        repo <- getRepo
        id <- liftEffect $ genId
        todo <- repo.update id (updateTodo \t -> t { description = Description("Say hello") })
        todo `shouldEqual` Nothing
      
      it "returns updated todo when given id exists" do
        repo <- getRepo
        t <- (repo.create (Description("say hi")))
        let id = getId t
        _ <- repo.update id (updateTodo \tt -> tt { description = Description("Say hello"), isComplete = (IsComplete true)})
        todo <- (repo.get id)
        todo `shouldEqual` Just(Todo {description: Description("Say hello"), id: id, isComplete: (IsComplete true)})

    describe "delete" do
      it "removes todo from store" do
        repo <- getRepo
        t <- (repo.create (Description("say hi")))
        let id = getId t
        todo <- (repo.get id)
        todo `shouldEqual` Just(t)
        _ <- (repo.delete id)
        deletedTodo <- (repo.get id)
        deletedTodo `shouldEqual` Nothing

setCompleted :: ∀ a. Todo a -> Todo a
setCompleted = updateTodo (\t -> t { isComplete = (IsComplete true) })
