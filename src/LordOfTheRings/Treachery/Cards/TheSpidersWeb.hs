module LordOfTheRings.Treachery.Cards.TheSpidersWeb (theSpidersWeb) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheSpidersWeb = TheSpidersWeb TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theSpidersWeb :: TreacheryCard TheSpidersWeb
theSpidersWeb = treachery TheSpidersWeb Cards.theSpidersWeb

-- | Counts as a condition attachment with the text, "Each time attached hero readies, deal
-- it 1 damage."
instance HasAbilities TheSpidersWeb where
  getAbilities (TheSpidersWeb a) = case a.placement of
    AttachedTo host -> [mkAbility a 1 $ forced $ BecameReadyWindow After host]
    _ -> []

-- | When Revealed: The player with the highest threat exhausts all heroes he controls.
-- Then, attach this card to one of that player's heroes.
instance RunMessage TheSpidersWeb where
  runMessage msg t@(TheSpidersWeb attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      withHighestThreatPlayer attrs \pid -> do
        heroes <- select (HeroControlledBy $ PlayerWithId pid)
        traverse_ exhaust heroes
        chooseOrRunOneM pid do
          questionLabeled "theSpidersWeb.attach"
          causedBy attrs
          targets heroes \h -> attachTreachery attrs.treacheryId h
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      withHost attrs \host -> dealDamage attrs host 1
      pure t
    _ -> TheSpidersWeb <$> runMessage msg attrs
