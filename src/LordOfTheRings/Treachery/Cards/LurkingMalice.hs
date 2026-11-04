module LordOfTheRings.Treachery.Cards.LurkingMalice (lurkingMalice) where

import LordOfTheRings.Scenario.Scenarios.ShelobsLair.Helpers (getShelob, shelob)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype LurkingMalice = LurkingMalice TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

lurkingMalice :: TreacheryCard LurkingMalice
lurkingMalice = treachery LurkingMalice Cards.lurkingMalice

{- | Peril. Surge.
When Revealed: Either place 1 resource on Shelob, or she makes an immediate attack against
you. With Shelob out of play, neither can be done.
Shadow: If attacking enemy is Shelob, place 1 resource on her. Then, she gets +1 attack for
each resource on her.
-}
instance RunMessage LurkingMalice where
  runMessage msg t@(LurkingMalice attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      getShelob >>= traverse_ \e ->
        chooseOneM you do
          questionLabeled "lurkingMalice.choose"
          causedBy attrs
          labeled "lurkingMalice.resource" $ placeResources attrs e 1
          labeled "lurkingMalice.attack" $ immediateAttack you e
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      isShelob <- selectAny (EnemyWithId eid <> shelob)
      when isShelob do
        placeResources attrs eid 1
        doStep 1 msg
      pure t
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      resources <- (.resources) <$> getEnemy eid
      when (resources > 0) $ enemyGetsAttack attrs eid resources
      pure t
    _ -> LurkingMalice <$> runMessage msg attrs
