module LordOfTheRings.Treachery.Cards.DreadHasCome (dreadHasCome) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DreadHasCome = DreadHasCome TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dreadHasCome :: TreacheryCard DreadHasCome
dreadHasCome = treachery DreadHasCome Cards.dreadHasCome

instance RunMessage DreadHasCome where
  runMessage msg t@(DreadHasCome a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      players <- getRemainingPlayers
      bolgs <- select (EnemyWithCardCode $ toCardCode Enemies.bolg)
      next False [(pid, eid) | pid <- players, eid <- bolgs]
      pure t
    ScenarioSpecific "dreadHasCome.attacks" (parseMaybe parseJSON -> Just (tid, casualty, attacks)) | tid == a.id -> do
      case attacks of
        [] -> when casualty do
          current <- getCurrentQuest
          for_ current \q -> roundModifier a q (ProgressLimit 0)
        (pid, eid) : _ -> do
          immediateAttack pid eid
          doStep 1 (WithTarget (EnemyTarget eid) msg)
      pure t
    DoStep
      1
      ( WithTarget
          (EnemyTarget eid)
          (ScenarioSpecific "dreadHasCome.attacks" (parseMaybe parseJSON -> Just (tid, casualty, (_ : rest))))
        ) | tid == a.id -> do
      destroyed <- attackDestroyedACharacter eid
      next (casualty || destroyed) rest
      pure t
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      enemyGetsAttack (CardIdSource cid) eid 2
      pure t
    _ -> DreadHasCome <$> runMessage msg a
   where
    next casualty attacks = push $ ScenarioSpecific "dreadHasCome.attacks" (toJSON (a.id, casualty, attacks))
