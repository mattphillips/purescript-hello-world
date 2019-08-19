module Todo.InMemoryInterpreters (createInMemoryTodoRepo) where

import Data.Function (const)
import Data.HashMap as HM
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Ref as Ref
import Id (genId)
import Prelude (bind, discard, pure, (<$>))
import Todo.Repository (Description, Todo, TodoId, TodoRepo)

type Store = Ref.Ref (HM.HashMap TodoId (Todo TodoId))

createInMemoryTodoRepo :: Effect (TodoRepo Effect)
createInMemoryTodoRepo = inMemoryTodoRepo <$> Ref.new HM.empty
  where
  inMemoryTodoRepo :: Store -> TodoRepo Effect
  inMemoryTodoRepo store = { create, get, update }
    where
    create :: Description -> Effect (Todo TodoId)
    create description = do
      id <- genId
      let p = { description, id }
      Ref.modify_ (HM.insert p.id p) store
      pure p

    get :: TodoId -> Effect (Maybe (Todo TodoId))
    get id = (HM.lookup id) <$> Ref.read store

    update :: TodoId -> (∀ a. Todo a -> Todo a) -> Effect (Maybe (Todo TodoId))
    update id f = do
      todo <- get id
      let updatedTodo = f <$> todo
      Ref.modify_ (HM.update (const updatedTodo) id) store
      pure updatedTodo
