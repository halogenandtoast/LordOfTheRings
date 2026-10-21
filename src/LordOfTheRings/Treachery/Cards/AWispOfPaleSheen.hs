module LordOfTheRings.Treachery.Cards.AWispOfPaleSheen (aWispOfPaleSheen) where

import LordOfTheRings.Scenario.Scenarios.TheDeadMarshes.Helpers (placeOnGollum)
import LordOfTheRings.Sphere (Sphere (Lore))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype AWispOfPaleSheen = AWispOfPaleSheen TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aWispOfPaleSheen :: TreacheryCard AWispOfPaleSheen
aWispOfPaleSheen = treachery AWispOfPaleSheen Cards.aWispOfPaleSheen

-- | When Revealed: Place 2 resource tokens on Gollum. Any player may exhaust a Lore hero
-- to reduce this effect to 1 resource token.
--
-- Each player in turn, from the first, may exhaust one of their own Lore heroes; once one
-- does, no one else is asked.
instance RunMessage AWispOfPaleSheen where
  runMessage msg t@(AWispOfPaleSheen attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      offer =<< getRemainingPlayers
      pure t
    _ -> AWispOfPaleSheen <$> runMessage msg attrs
   where
    offer :: (ReverseQueue m) => [PlayerId] -> m ()
    offer = \case
      [] -> placeOnGollum attrs 2
      pid : rest -> do
        lore <- select (HeroControlledBy (PlayerWithId pid) <> HeroWithSphere Lore <> ReadyHero)
        chooseOrRunOneM pid do
          questionLabeled "aWispOfPaleSheen.exhaust"
          causedBy attrs
          targets lore \h -> do
            exhaust h
            placeOnGollum attrs 1
          labeled "aWispOfPaleSheen.allow" $ offer rest
