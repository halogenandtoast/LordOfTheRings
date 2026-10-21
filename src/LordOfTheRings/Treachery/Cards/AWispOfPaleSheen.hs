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
-- ponytail: the first player decides for the table, from every player's Lore heroes.
instance RunMessage AWispOfPaleSheen where
  runMessage msg t@(AWispOfPaleSheen attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      lore <- select (HeroWithSphere Lore <> ReadyHero)
      chooseOrRunOneM lead do
        questionLabeled "aWispOfPaleSheen.exhaust"
        causedBy attrs
        targets lore \h -> do
          exhaust h
          placeOnGollum attrs 1
        labeled "aWispOfPaleSheen.allow" $ placeOnGollum attrs 2
      pure t
    _ -> AWispOfPaleSheen <$> runMessage msg attrs
