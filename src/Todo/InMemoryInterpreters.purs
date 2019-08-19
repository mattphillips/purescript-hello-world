module Todo.InMemoryInterpreters (createInMemoryTodoRepo) where

import Data.Function (const, (<<<))
import Data.HashMap as HM
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Ref as Ref
import Id (genId)
import Prelude (Unit, bind, discard, pure, (<$>), (==))
import Todo.Repository (Description, Todo, TodoId, TodoRepo, Filter)

type Store = Ref.Ref (HM.HashMap TodoId (Todo TodoId))

createInMemoryTodoRepo :: Effect (TodoRepo Effect)
createInMemoryTodoRepo = inMemoryTodoRepo <$> Ref.new HM.empty
  where
  inMemoryTodoRepo :: Store -> TodoRepo Effect
  inMemoryTodoRepo store = { create, delete, get, getAll, filter, update }
    where
    create :: Description -> Effect (Todo TodoId)
    create description = do
      id <- genId
      let p = { description, id, isComplete: false }
      Ref.modify_ (HM.insert p.id p) store
      pure p

    get :: TodoId -> Effect (Maybe (Todo TodoId))
    get id = (HM.lookup id) <$> Ref.read store

    getAll :: Effect (Array (Todo TodoId))
    getAll = HM.values <$> Ref.read store

    filter :: Filter -> Effect (Array (Todo TodoId))
    filter f = (HM.values <<< (HM.filter (\{isComplete} -> isComplete == f.isComplete)) <$> Ref.read store)

    update :: TodoId -> (∀ a. Todo a -> Todo a) -> Effect (Maybe (Todo TodoId))
    update id f = do
      todo <- get id
      let updatedTodo = f <$> todo
      Ref.modify_ (HM.update (const updatedTodo) id) store
      pure updatedTodo

    delete :: TodoId -> Effect Unit
    delete id = Ref.modify_ (HM.delete id) store
