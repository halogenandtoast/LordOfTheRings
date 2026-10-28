module LordOfTheRings.Treachery.Cards.LurkingInShadows (lurkingInShadows) where

import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Brigand))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype LurkingInShadows = LurkingInShadows TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

lurkingInShadows :: TreacheryCard LurkingInShadows
lurkingInShadows = treachery LurkingInShadows Cards.lurkingInShadows

-- | When Revealed: Return all Brigand enemies engaged with players to the staging area. If
-- this effect returned no Brigand enemies to the staging area, Lurking in Shadows gains
-- surge.
-- Shadow: Return attacking enemy to the staging area after it attacks.
instance RunMessage LurkingInShadows where
  runMessage msg t@(LurkingInShadows attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      brigands <- select (EnemyWithTrait Brigand <> EnemyEngagedWith AnyPlayer)
      if null brigands
        then revealEncounterCard
        else for_ brigands (`placeEnemy` StagingArea)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      returnToStagingAfterAttack eid
      pure t
    _ -> LurkingInShadows <$> runMessage msg attrs
