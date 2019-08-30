module Todo.InMemoryInterpreters (createInMemoryTodoRepo) where

import Data.Function (const, ($), (<<<), (>>>))
import Data.HashMap as HM
import Data.Maybe (Maybe)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Id (genId)
import Prelude (Unit, bind, discard, pure, (<$>), (==))
import Todo (Description, IsComplete(..), Todo(..), TodoId, getIsComplete)
import Todo.Repository (Filter, TodoRepo)

type Store = Ref.Ref (HM.HashMap TodoId (Todo TodoId))

createInMemoryTodoRepo :: forall m. MonadEffect m => m (TodoRepo m)
createInMemoryTodoRepo = inMemoryTodoRepo <$> (liftEffect $ Ref.new HM.empty)
  where
  inMemoryTodoRepo :: Store -> TodoRepo m
  inMemoryTodoRepo store = { create, delete, get, getAll, filter, update }
    where
    create :: Description -> m (Todo TodoId)
    create description = do
      id <- liftEffect $ genId
      let t = Todo { description, id, isComplete: (IsComplete false) }
      liftEffect $ Ref.modify_ (HM.insert id t) store
      pure t

    get :: TodoId -> m (Maybe (Todo TodoId))
    get id = (HM.lookup id) <$> (liftEffect $ Ref.read store)

    getAll :: m (Array (Todo TodoId))
    getAll = liftEffect $ HM.values <$> Ref.read store

    filter :: Filter -> m (Array (Todo TodoId))
    filter f = (HM.values <<< (HM.filter (getIsComplete >>> (_ == f.isComplete))) <$> (liftEffect $ Ref.read store))

    update :: TodoId -> (∀ a. Todo a -> Todo a) -> m (Maybe (Todo TodoId))
    update id f = do
      todo <- get id
      let updatedTodo = f <$> todo
      liftEffect $ Ref.modify_ (HM.update (const updatedTodo) id) store
      pure updatedTodo

    delete :: TodoId -> m Unit
    delete id = liftEffect $ Ref.modify_ (HM.delete id) store
