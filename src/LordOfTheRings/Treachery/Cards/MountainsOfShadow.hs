module LordOfTheRings.Treachery.Cards.MountainsOfShadow (mountainsOfShadow) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype MountainsOfShadow = MountainsOfShadow TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

mountainsOfShadow :: TreacheryCard MountainsOfShadow
mountainsOfShadow = treachery MountainsOfShadow Cards.mountainsOfShadow

{- | When Revealed: Raise each player's threat by X, where X is the printed quest points of
the active location. If there is no active location, Mountains of Shadow gains surge.
Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, raise your
threat by 2.
-}
instance RunMessage MountainsOfShadow where
  runMessage msg t@(MountainsOfShadow attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      select IsActiveLocation >>= \case
        [] -> revealEncounterCard
        lid : _ -> do
          x <- (.questPoints) <$> getLocation lid
          eachPlayer \p -> encounterRaisesThreat p x
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ pid _) | cid == attrs.cardId -> do
      encounterRaisesThreat pid 2
      pure t
    _ -> MountainsOfShadow <$> runMessage msg attrs
